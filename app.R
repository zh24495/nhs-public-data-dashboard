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


# --------------------------------------------------
# FRIENDLY MEASURE DISPLAY NAMES
# --------------------------------------------------

measure_labels <- c(
  
  "Age" = "AGE",
  
  "Declined Memory Assessment" =
    "ASS_Declined_GP_Mem_Ass",
  
  "Declined Memory Clinic Referral" =
    "ASS_Declined_GP_Mem_Clinic",
  
  "Received GP assessment for dementia" =
    "ASS_Received_GP",
  
  "Received Memory Assessment" =
    "ASS_Received_GP_Mem_Ass",
  
  "Received Memory Clinic Referral" =
    "ASS_Received_GP_Mem_Clinic",
  
  "Received Care Plan" =
    "DIAG_Recieved_Care_Plan",
  
  "Declined Care Plan" =
    "DIAG_Declined_Care_Plan",
  
  "Received Medication Review" =
    "DIAG_Recieved_Med_Rev",
  
  "Comorbidities" =
    "COMORBIDITIES",
  
  "Delirium (12 months)" =
    "DELIRIUM_12M",
  
  "Dementia Estimate (Over 65s)" =
    "DEMENTIA_ESTIMATE_65_PLUS",
  
  "Dementia Register" =
    "DEMENTIA_REGISTER",
  
  "Dementia Register (Under 65)" =
    "DEMENTIA_REGISTER_0_64",
  
  "Dementia Register (Over 65)" =
    "DEMENTIA_REGISTER_65_PLUS",
  
  "Dementia Type" =
    "DEMENTIA_TYPE",
  
  "Diagnosis Rate (Over 65s)" =
    "DIAG_RATE_65_PLUS",
  
  "Diagnosis Rate (Over 65s - Lower limit)" =
    "DIAG_RATE_65_PLUS_LL",
  
  "Diagnosis Rate (Over 65s - Upper limit)" =
    "DIAG_RATE_65_PLUS_UL",
  
  "Ethnicity" =
    "ETHNICITY",
  
  "Incidence" =
    "INCIDENCE",
  
  "Palliative Care" =
    "PALLIATIVE_CARE",
  
  "Patient List Under 65" =
    "PAT_LIST_0_64",
  
  "Patient List Over 65" =
    "PAT_LIST_65_PLUS",
  
  "Patient List" =
    "PAT_LIST_ALL",
  
  "Residential Type" =
    "RES_TYPE",
  
  "Sex" =
    "SEX",
  
  "Young Onset" =
    "YOUNG_ONSET"
)


# --------------------------------------------------
# ONLY SHOW LABELS FOR MEASURES THAT EXIST IN DATA
# --------------------------------------------------

measure_choices <- measure_labels[
  measure_labels %in% all_measures
]


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
      mutate(
        
        # Use measure alone for ordinary measures
        # and measure + submeasure for submeasures
        series = case_when(
          
          !is.na(submeasure) ~
            paste(
              measure,
              submeasure,
              sep = " - "
            ),
          
          TRUE ~
            measure
          
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
          measure,
          
          ifelse(
            !is.na(submeasure),
            paste0(
              "<br><b>Submeasure:</b> ",
              submeasure
            ),
            ""
          ),
          
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