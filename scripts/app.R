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
      
      selectInput(
        inputId = "measure",
        label = "Select measure",
        choices = sort(unique(measure_data$measure))
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
      selected = org_choices[1],   # start with just the first org selected
      server = TRUE
    )
    
  }, ignoreNULL = TRUE)
  
  filtered_data <- reactive({
    
    req(input$org_type, input$org_name)
    
    measure_data %>%
      filter(
        measure  == input$measure,
        org_type == input$org_type,
        name %in% input$org_name
      ) %>%
      arrange(date)
    
  })
  
  output$time_plot <- renderPlot({
    
    req(nrow(filtered_data()) > 0)
    
    ggplot(
      filtered_data(),
      aes(x = date, y = value, colour = name, group = name)
    ) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(
        title = input$measure,
        x = NULL,
        y = "Value",
        colour = "Organisation"
      ) +
      theme_minimal(base_size = 14)
    
  })
  
}

# --------------------------------------------------
# RUN APP
# --------------------------------------------------
shinyApp(ui = ui, server = server)