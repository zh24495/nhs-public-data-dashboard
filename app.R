library(shiny)
library(tidyverse)
library(here)
library(lubridate)
library(colourpicker)
library(plotly)

# --------------------------------------------------
# LOAD DATA
# --------------------------------------------------

measure_fact <- readRDS(
  here::here(
    "data",
    "database_files",
    "measure_fact.rds"
  )
)

geography_dim <- readRDS(
  here::here(
    "data",
    "database_files",
    "geography_dim.rds"
  )
)


# --------------------------------------------------
# SMALL AMOUNT OF DATA PREP
# --------------------------------------------------

measure_data <- measure_fact %>%
  mutate(
    date = dmy(date),
    
    # Treat blank strings as NA
    submeasure = na_if(submeasure, "")
  )


# --------------------------------------------------
# LABELS FOR ORG TYPE
# --------------------------------------------------

org_type_labels <- c(
  "Country"          = "COUNTRY_RESPONSIBILITY",
  "NHS Region"       = "NHS_REGION",
  "ICB"              = "ICB",
  "Sub-ICB Location" = "SUB_ICB_LOC"
)


# --------------------------------------------------
# GET MEASURES LIST
# --------------------------------------------------

all_measures <- measure_data %>%
  distinct(measure) %>%
  arrange(measure) %>%
  pull(measure)

#Load measure info for labels and descriptions
measure_info <- read_csv(
  here::here(
    "data",
    "measure_info.csv"
  )
)
# Only include measures in the measures table
measure_choices <- measure_info %>%
  filter(measure %in% all_measures) %>%
  arrange(display_name) %>%
  select(display_name, measure) %>%
  deframe()


# --------------------------------------------------
# GET DATE RANGE
# --------------------------------------------------

min_date <- min(
  measure_data$date,
  na.rm = TRUE
)

max_date <- max(
  measure_data$date,
  na.rm = TRUE
)


# --------------------------------------------------
# UI
# --------------------------------------------------

ui <- fluidPage(
  
  titlePanel("NHS Dementia Care Measures"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # --------------------------------------------
      # MEASURE
      # --------------------------------------------
      
      selectizeInput(
        inputId = "measure",
        label = "Select measure(s)",
        choices = measure_choices,
        selected = "DEMENTIA_REGISTER",
        multiple = TRUE,
        options = list(
          plugins = list("remove_button")
        )
      ),
      
      
      # --------------------------------------------
      # SUBMEASURE
      # --------------------------------------------
      
      conditionalPanel(
        
        condition = "output.has_submeasures",
        
        selectizeInput(
          inputId = "submeasure",
          label = "Select submeasure(s)",
          choices = NULL,
          multiple = TRUE,
          options = list(
            plugins = list("remove_button")
          )
        )
        
      ),
      
      
      # --------------------------------------------
      # ORGANISATION TYPE
      # --------------------------------------------
      
      selectInput(
        inputId = "org_type",
        label = "Select organisation type",
        choices = org_type_labels,
        selected = "COUNTRY_RESPONSIBILITY"
      ),
      
      
      # --------------------------------------------
      # ORGANISATION
      # --------------------------------------------
      
      selectizeInput(
        inputId = "org_name",
        label = "Select organisation(s)",
        choices = NULL,
        multiple = TRUE,
        options = list(
          plugins = list("remove_button")
        )
      ),
      
      
      # --------------------------------------------
      # DATE RANGE
      # --------------------------------------------
      
      dateRangeInput(
        inputId = "date_range",
        label = "Select date range",
        start = min_date,
        end = max_date,
        min = min_date,
        max = max_date,
        format = "dd-M-yyyy",
        separator = " to "
      )
      
    ),
    
    
    # ----------------------------------------------
    # MAIN PANEL
    # ----------------------------------------------
    
    mainPanel(
      
      plotlyOutput(
        outputId = "time_plot",
        height = "600px"
      )
      
    )
    
  )
)


# --------------------------------------------------
# SERVER
# --------------------------------------------------

server <- function(input, output, session) {
  
  
  # ------------------------------------------------
  # DOES SELECTED MEASURE HAVE SUBMEASURES?
  # ------------------------------------------------
  
  has_submeasures <- reactive({
    
    req(input$measure)
    
    measure_data %>%
      filter(
        measure %in% input$measure,
        !is.na(submeasure)
      ) %>%
      nrow() > 0
    
  })
  
  
  # Tell conditionalPanel whether to show submeasure
  output$has_submeasures <- reactive({
    has_submeasures()
  })
  
  
  outputOptions(
    output,
    "has_submeasures",
    suspendWhenHidden = FALSE
  )
  
  
  # ------------------------------------------------
  # UPDATE SUBMEASURE OPTIONS
  # ------------------------------------------------
  
  observeEvent(input$measure, {
    
    submeasure_choices <- measure_data %>%
      filter(
        measure %in% input$measure,
        !is.na(submeasure)
      ) %>%
      distinct(submeasure) %>%
      arrange(submeasure) %>%
      pull(submeasure)
    
    
    updateSelectizeInput(
      session,
      inputId = "submeasure",
      choices = submeasure_choices,
      selected = if (length(submeasure_choices) > 0) {
        submeasure_choices
      } else {
        NULL
      },
      server = TRUE
    )
    
  }, ignoreNULL = TRUE)
  
  
  # ------------------------------------------------
  # UPDATE ORGANISATION OPTIONS
  # ------------------------------------------------
  
  observeEvent(input$org_type, {
    
    org_choices <- measure_data %>%
      filter(
        org_type == input$org_type
      ) %>%
      distinct(name) %>%
      arrange(name) %>%
      pull(name)
    
    
    updateSelectizeInput(
      session,
      inputId = "org_name",
      choices = org_choices,
      selected = org_choices[1],
      server = TRUE
    )
    
  }, ignoreNULL = TRUE)
  
  
  # ------------------------------------------------
  # FILTER DATA
  # ------------------------------------------------
  
  filtered_data <- reactive({
    
    req(
      input$org_type,
      input$org_name,
      input$measure,
      input$date_range
    )
    
    
    # Filter by measure, organisation and date
    data <- measure_data %>%
      filter(
        measure %in% input$measure,
        org_type == input$org_type,
        name %in% input$org_name,
        date >= input$date_range[1],
        date <= input$date_range[2]
      )
    
    
    # ----------------------------------------------
    # FILTER SUBMEASURES
    # ----------------------------------------------
    
    if (
      has_submeasures() &&
      !is.null(input$submeasure) &&
      length(input$submeasure) > 0
    ) {
      
      data <- data %>%
        filter(
          is.na(submeasure) |
            submeasure %in% input$submeasure
        )
      
    }
    
    
    data %>%
      arrange(date)
    
  })
  
  
  # ------------------------------------------------
  # PLOT
  # ------------------------------------------------
  
  output$time_plot <- renderPlotly({
    
    df <- filtered_data()
    
    req(nrow(df) > 0)
    
    
    # ----------------------------------------------
    # CREATE SERIES AND HOVER INFORMATION
    # ----------------------------------------------
    
    plot_data <- df %>%
      left_join(
        measure_info %>%
          select(measure, display_name, description),
        by = "measure"
      ) %>%
      mutate(
        
        # Use display name instead of variable name
        measure_display = coalesce(
          display_name,
          measure
        ),
        
        # Use measure + submeasure for submeasures
        series = case_when(
          
          !is.na(submeasure) ~
            paste(
              measure_display,
              submeasure,
              sep = " - "
            ),
          
          TRUE ~
            measure_display
          
        ),
        
        # Include organisation in series name
        series = paste(
          name,
          series,
          sep = " - "
        ),
        
        # Information displayed when hovering
        hover_text = paste0(
          
          "<b>Organisation:</b> ",
          name,
          
          "<br><b>Measure:</b> ",
          measure_display,
          
          ifelse(
            !is.na(submeasure),
            paste0(
              "<br><b>Submeasure:</b> ",
              submeasure
            ),
            ""
          ),
          
          "<br><b>Description:</b> ",
          description,
          
          "<br><b>Date:</b> ",
          format(
            date,
            "%d %b %Y"
          ),
          
          "<br><b>Value:</b> ",
          format(
            value,
            big.mark = ","
          )
          
        )
        
      )
    
    
    # ----------------------------------------------
    # CREATE GGPLOT
    # ----------------------------------------------
    
    p <- ggplot(
      plot_data,
      aes(
        x = date,
        y = value,
        colour = series,
        group = series,
        text = hover_text
      )
    ) +
      
      geom_line(
        linewidth = 1
      ) +
      
      geom_point(
        size = 2
      ) +
      
      labs(
        x = NULL,
        y = "Value",
        colour = "Series"
      ) +
      
      theme_minimal(
        base_size = 14
      ) +
      
      theme(
        legend.position = "bottom"
      )
    
    
    # ----------------------------------------------
    # CONVERT TO INTERACTIVE PLOT
    # ----------------------------------------------
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
}


# --------------------------------------------------
# RUN APP
# --------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)