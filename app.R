library(shiny)
library(bslib)
library(tidyverse)
library(here)
library(lubridate)
library(colourpicker)
library(plotly)
library(DT)
library(scales)
library(paletteer)


ink      <- "#14293D"
accent   <- "#C98A3D"
canvas   <- "#F4F6F8"
surface  <- "#FFFFFF"
text_col <- "#1F2933"
muted    <- "#6B7280"

# "tvthemes::Garnet" via paletteer. Dropping pure black (last colour in
# the palette) since it fights with the gridlines/text - if you'd
# rather keep all 9, drop the `[-9]`.
series_palette <- as.character(paletteer_d("tvthemes::Garnet"))[-9]

app_theme <- bs_theme(
  version      = 5,
  bg           = canvas,
  fg           = text_col,
  primary      = ink,
  secondary    = accent,
  base_font    = font_google("Inter"),
  heading_font = font_google("Libre Franklin"),
  code_font    = font_google("IBM Plex Mono"),
  "border-radius" = "0.6rem"
)

custom_css <- HTML("
  .card { box-shadow: 0 2px 10px rgba(20,41,61,0.07); border: none !important; }
  .bslib-sidebar-layout > .sidebar { background-color: #FFFFFF !important; }
  .app-title { font-weight: 700; color: #14293D; letter-spacing: -0.01em; }
  .app-subtitle { color: #6B7280; font-size: 0.95rem; margin-top: -6px; }
  .nav-tabs .nav-link.active { font-weight: 600; border-bottom: 3px solid #C98A3D; color: #14293D !important; }
  .nav-tabs .nav-link { color: #6B7280; }
")


# --------------------------------------------------
# LOAD DATA
# --------------------------------------------------

measure_fact <- readRDS(
  here::here("data", "database_files", "measure_fact.rds")
)

geography_dim <- readRDS(
  here::here("data", "database_files", "geography_dim.rds")
)


# --------------------------------------------------
# SMALL AMOUNT OF DATA PREP
# --------------------------------------------------

measure_data <- measure_fact %>%
  mutate(
    date = dmy(date),
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
# MEASURES:
# --------------------------------------------------

all_measures <- measure_data %>%
  distinct(measure) %>%
  pull(measure)

#Get rid of upper and lower bound measures and include them in overall rate measure
ci_suffix_regex <- "_(LL|UL)$"

base_measures <- all_measures[!str_detect(all_measures, ci_suffix_regex)]

measure_info <- read_csv(
  here::here("data", "measure_info.csv"),
  show_col_types = FALSE
)

measure_choices <- measure_info %>%
  filter(measure %in% base_measures) %>%
  arrange(display_name) %>%
  select(display_name, measure) %>%
  deframe()

# Lookup table of base measure -> its LL/UL sibling codes (only where both exist)
ci_lookup <- tibble(measure = base_measures) %>%
  mutate(
    ll = paste0(measure, "_LL"),
    ul = paste0(measure, "_UL"),
    has_ci = ll %in% all_measures & ul %in% all_measures
  ) %>%
  filter(has_ci)

# Long-form CI data, reshaped so each base measure has a lower/upper column
ci_data_all <- measure_data %>%
  filter(str_detect(measure, ci_suffix_regex)) %>%
  mutate(
    base_measure = str_remove(measure, ci_suffix_regex),
    bound = if_else(str_detect(measure, "_LL$"), "ci_lower", "ci_upper")
  ) %>%
  select(name, org_type, date, base_measure, bound, value) %>%
  pivot_wider(names_from = bound, values_from = value)


# --------------------------------------------------
# GET DATE RANGE
# --------------------------------------------------

min_date <- min(measure_data$date, na.rm = TRUE)
max_date <- max(measure_data$date, na.rm = TRUE)


# --------------------------------------------------
# HELPERS
# --------------------------------------------------

# Applies the measure / date / submeasure filters shared between the
# main series and the England benchmark series, so the logic only
# lives in one place.
apply_common_filters <- function(data, input, has_submeasures) {
  
  data <- data %>%
    filter(
      measure %in% input$measure,
      date >= input$date_range[1],
      date <= input$date_range[2]
    )
  
  if (has_submeasures && !is.null(input$submeasure) && length(input$submeasure) > 0) {
    data <- data %>%
      filter(is.na(submeasure) | submeasure %in% input$submeasure)
  }
  
  data
}

format_value <- function(x) {
  scales::comma(x, accuracy = 0.1)
}


# --------------------------------------------------
# UI
# --------------------------------------------------

ui <- page_sidebar(
  
  title = tagList(
    div(class = "app-title", "NHS Dementia Care Measures"),
    div(class = "app-subtitle", "Primary care dementia diagnosis and register data across England")
  ),
  
  theme = app_theme,
  
  tags$head(tags$style(custom_css)),
  
  sidebar = sidebar(
    width = 320,
    title = "Filters",
    
    selectizeInput(
      inputId = "measure",
      label = "Select measure(s)",
      choices = measure_choices,
      selected = "DEMENTIA_REGISTER",
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    ),
    
    conditionalPanel(
      condition = "output.has_submeasures",
      selectizeInput(
        inputId = "submeasure",
        label = "Select submeasure(s)",
        choices = NULL,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    ),
    
    selectInput(
      inputId = "org_type",
      label = "Select organisation type",
      choices = org_type_labels,
      selected = "COUNTRY_RESPONSIBILITY"
    ),
    
    selectizeInput(
      inputId = "org_name",
      label = "Select organisation(s)",
      choices = NULL,
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    ),
    
    dateRangeInput(
      inputId = "date_range",
      label = "Select date range",
      start = min_date,
      end = max_date,
      min = min_date,
      max = max_date,
      format = "dd-M-yyyy",
      separator = " to "
    ),
    
    conditionalPanel(
      condition = paste0("input.org_type != 'COUNTRY_RESPONSIBILITY'"),
      checkboxInput(
        inputId = "show_benchmark",
        label = "Show England benchmark",
        value = TRUE
      )
    ),
    
    conditionalPanel(
      condition = "output.has_ci",
      checkboxInput(
        inputId = "show_ci",
        label = "Show confidence interval band",
        value = TRUE
      )
    )
  ),
  
  # ---- Main content ----
  navset_card_tab(
    nav_panel(
      "Chart",
      plotlyOutput(outputId = "time_plot", height = "560px")
    ),
    nav_panel(
      "Data table",
      div(
        style = "display:flex; justify-content:flex-end; margin: 10px 0;",
        downloadButton("download_data", "Download CSV", class = "btn-sm")
      ),
      DTOutput("data_table")
    )
  )
)


# --------------------------------------------------
# SERVER
# --------------------------------------------------

server <- function(input, output, session) {
  
  # Keeps the currently valid org choices around so the
  # "Select all" / "Clear" links can act on them.
  org_choices_r <- reactiveVal(character(0))
  
  
  # ------------------------------------------------
  # DOES SELECTED MEASURE HAVE SUBMEASURES / A CI BAND?
  # ------------------------------------------------
  
  has_submeasures <- reactive({
    req(input$measure)
    measure_data %>%
      filter(measure %in% input$measure, !is.na(submeasure)) %>%
      nrow() > 0
  })
  
  output$has_submeasures <- reactive({ has_submeasures() })
  outputOptions(output, "has_submeasures", suspendWhenHidden = FALSE)
  
  has_ci <- reactive({
    req(input$measure)
    any(input$measure %in% ci_lookup$measure)
  })
  
  output$has_ci <- reactive({ has_ci() })
  outputOptions(output, "has_ci", suspendWhenHidden = FALSE)
  
  
  # ------------------------------------------------
  # UPDATE SUBMEASURE OPTIONS
  # ------------------------------------------------
  
  observeEvent(input$measure, {
    
    submeasure_choices <- measure_data %>%
      filter(measure %in% input$measure, !is.na(submeasure)) %>%
      distinct(submeasure) %>%
      arrange(submeasure) %>%
      pull(submeasure)
    
    updateSelectizeInput(
      session,
      inputId = "submeasure",
      choices = submeasure_choices,
      selected = if (length(submeasure_choices) > 0) submeasure_choices else NULL,
      server = TRUE
    )
    
  }, ignoreNULL = TRUE)
  
  #Org Type choices
  
  observeEvent(input$org_type, {
    
    org_choices <- measure_data %>%
      filter(org_type == input$org_type) %>%
      distinct(name) %>%
      arrange(name) %>%
      pull(name)
    
    org_choices_r(org_choices)
    
    default_selection <- if (input$org_type == "COUNTRY_RESPONSIBILITY") {
      org_choices[1]
    } else {
      head(org_choices, 3)
    }
    
    updateSelectizeInput(
      session,
      inputId = "org_name",
      choices = org_choices,
      selected = default_selection,
      server = TRUE
    )
    
  }, ignoreNULL = TRUE)

  
  
  # ------------------------------------------------
  # FILTER DATA (main series)

  filtered_data <- reactive({
    
    req(input$org_type, input$org_name, input$measure, input$date_range)
    
    data <- measure_data %>%
      filter(
        org_type == input$org_type,
        name %in% input$org_name
      )
    
    apply_common_filters(data, input, has_submeasures()) %>%
      arrange(date)
    
  })
  
  
  # ------------------------------------------------
  # ENGLAND BENCHMARK SERIES
  # Only fetched when relevant: not already viewing
  # country-level data, and the toggle is switched on.
  # ------------------------------------------------
  
  benchmark_data <- reactive({
    
    req(input$measure, input$date_range)
    
    if (is.null(input$show_benchmark) || !isTRUE(input$show_benchmark)) return(NULL)
    if (identical(input$org_type, "COUNTRY_RESPONSIBILITY")) return(NULL)
    
    data <- measure_data %>%
      filter(org_type == "COUNTRY_RESPONSIBILITY", name == "ENGLAND")
    
    apply_common_filters(data, input, has_submeasures()) %>%
      arrange(date)
    
  })
  
  
  # ------------------------------------------------
  # PLOT DATA
  # Enriches the raw rows with display names, series
  # labels, hover text, and (where available) CI bounds.
  # ------------------------------------------------
  
  build_plot_data <- function(df) {
    
    df %>%
      left_join(
        measure_info %>% select(measure, display_name, description),
        by = "measure"
      ) %>%
      left_join(
        ci_data_all,
        by = c("name", "org_type", "date", "measure" = "base_measure")
      ) %>%
      mutate(
        measure_display = coalesce(display_name, measure),
        series = case_when(
          !is.na(submeasure) ~ paste(measure_display, submeasure, sep = " - "),
          TRUE ~ measure_display
        ),
        series = paste(name, series, sep = " - "),
        hover_text = paste0(
          "<b>Organisation:</b> ", name,
          "<br><b>Measure:</b> ", measure_display,
          ifelse(!is.na(submeasure), paste0("<br><b>Submeasure:</b> ", submeasure), ""),
          "<br><b>Description:</b> ", description,
          "<br><b>Date:</b> ", format(date, "%d %b %Y"),
          "<br><b>Value:</b> ", format_value(value),
          ifelse(
            !is.na(ci_lower) & !is.na(ci_upper),
            paste0("<br><b>95% CI:</b> ", format_value(ci_lower), " - ", format_value(ci_upper)),
            ""
          )
        )
      )
  }
  
  # ------------------------------------------------
  # PLOT
  
  output$time_plot <- renderPlotly({
    
    df <- filtered_data()
    
    validate(
      need(
        nrow(df) > 0,
        "No data available for this combination of filters.\nTry widening the date range or selecting a different organisation."
      )
    )
    
    pd <- build_plot_data(df)
    
    series_names <- unique(pd$series)
    palette <- setNames(
      rep_len(series_palette, length(series_names)),
      series_names
    )
    
    show_ci <- isTRUE(input$show_ci) && has_ci()
    
    p <- plot_ly()
    
    for (s in series_names) {
      
      s_data <- pd %>% filter(series == s) %>% arrange(date)
      colour <- palette[[s]]
      
      if (show_ci && all(!is.na(s_data$ci_lower)) && all(!is.na(s_data$ci_upper))) {
        
        p <- p %>%
          add_trace(
            data = s_data, x = ~date, y = ~ci_upper,
            type = "scatter", mode = "lines",
            line = list(width = 0, color = colour),
            showlegend = FALSE, hoverinfo = "skip",
            name = s
          ) %>%
          add_trace(
            data = s_data, x = ~date, y = ~ci_lower,
            type = "scatter", mode = "lines",
            line = list(width = 0, color = colour),
            fill = "tonexty",
            fillcolor = paste0(colour, "26"),  # ~15% opacity
            showlegend = FALSE, hoverinfo = "skip",
            name = s
          )
      }
      
      p <- p %>%
        add_trace(
          data = s_data, x = ~date, y = ~value,
          type = "scatter", mode = "lines+markers",
          line = list(color = colour, width = 2.5),
          marker = list(color = colour, size = 6),
          text = ~hover_text, hoverinfo = "text",
          name = s
        )
    }
    
    if (!is.null(benchmark_data())) {
      
      bd <- build_plot_data(benchmark_data())
      
      for (m in unique(bd$measure)) {
        b_data <- bd %>% filter(measure == m) %>% arrange(date)
        
        p <- p %>%
          add_trace(
            data = b_data, x = ~date, y = ~value,
            type = "scatter", mode = "lines",
            line = list(color = muted, width = 2, dash = "dash"),
            text = ~hover_text, hoverinfo = "text",
            name = paste0(unique(b_data$measure_display), " \u2013 England (benchmark)")
          )
      }
    }
    
    p %>%
      layout(
        font = list(family = "Inter", color = text_col, size = 13),
        paper_bgcolor = surface,
        plot_bgcolor = surface,
        xaxis = list(title = "", gridcolor = "#E5E9EC", zeroline = FALSE),
        yaxis = list(title = "Value", gridcolor = "#E5E9EC", zeroline = FALSE),
        legend = list(orientation = "h", y = -0.2, font = list(size = 11)),
        hoverlabel = list(bgcolor = surface, font = list(family = "Inter", size = 12), bordercolor = ink),
        margin = list(t = 20)
      ) %>%
      config(displaylogo = FALSE)
    
  })
  
  
  # ------------------------------------------------
  # DATA TABLE
  # ------------------------------------------------
  
  output$data_table <- renderDT({
    
    df <- filtered_data()
    validate(need(nrow(df) > 0, "No data available for this combination of filters."))
    
    df %>%
      left_join(measure_info %>% select(measure, display_name), by = "measure") %>%
      transmute(
        Organisation = name,
        `Organisation type` = org_type,
        Measure = coalesce(display_name, measure),
        Submeasure = submeasure,
        Date = format(date, "%d %b %Y"),
        Value = value
      ) %>%
      arrange(desc(Date)) %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 15, dom = "ftip"),
        class = "stripe hover"
      ) %>%
      formatCurrency("Value", currency = "", interval = 3, mark = ",", digits = 1)
  })
  
  
  # ------------------------------------------------
  # CSV DOWNLOAD
  # ------------------------------------------------
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("dementia_measures_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      df <- filtered_data() %>%
        left_join(measure_info %>% select(measure, display_name), by = "measure") %>%
        select(name, org_type, measure, display_name, submeasure, date, value)
      write_csv(df, file)
    }
  )
  
}


# --------------------------------------------------
# RUN APP
# --------------------------------------------------

shinyApp(ui = ui, server = server)