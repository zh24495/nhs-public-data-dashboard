library(shiny)
library(tidyverse)
library(here)
library(lubridate)
library(colourpicker)

# --------------------------------------------------
# LOAD DATA
# --------------------------------------------------
measure_fact  <- readRDS(here::here("data", "database_files", "measure_fact.rds"))
geography_dim <- readRDS(here::here("data" ,"database_files", "practice_dim.rds"))

# Prepare measure data
measure_data <- measure_fact %>%
  mutate(date = dmy(date))

# Friendly labels for org_type dropdown
org_type_labels <- c(
  "Country"          = "COUNTRY_RESPONSIBILITY",
  "NHS Region"       = "NHS_REGION",
  "ICB"              = "ICB",
  "Sub-ICB Location" = "SUB_ICB_LOC"
)

# --------------------------------------------------
# UI
# --------------------------------------------------
ui <- fluidPage(
  
  titlePanel("Primary Care Measures"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectizeInput(
        inputId = "measure",
        label = "Select measure(s)",
        choices = sort(unique(measure_data$measure)),
        selected = sort(unique(measure_data$measure))[1],
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
      
      selectInput(
        inputId = "org_type",
        label = "Select organisation type",
        choices = org_type_labels,
        selected = "ICB"
      ),
      
      selectizeInput(
        inputId = "org_name",
        label = "Select organisation(s)",
        choices = NULL,        # populated by server
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
      
    ),
    
    mainPanel(
      
      plotOutput(
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
  
  # Update the list of available org names whenever org_type changes
  observeEvent(input$org_type, {
    
    org_choices <- measure_data %>%
      filter(org_type == input$org_type) %>%
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
  
  filtered_data <- reactive({
    
    req(input$org_type, input$org_name, input$measure)
    
    measure_data %>%
      filter(
        measure  %in% input$measure,
        org_type == input$org_type,
        name     %in% input$org_name
      ) %>%
      arrange(date)
    
  })
  
  output$time_plot <- renderPlot({
    
    req(nrow(filtered_data()) > 0)
    
    plot_data <- filtered_data() %>%
      mutate(series = paste(name, measure, sep = " - "))
    
    ggplot(
      plot_data,
      aes(x = date, y = value, colour = series, group = series)
    ) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(
        x = NULL,
        y = "Value",
        colour = "Series"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
    
  })
  
}

# --------------------------------------------------
# RUN APP
# --------------------------------------------------
shinyApp(ui = ui, server = server)