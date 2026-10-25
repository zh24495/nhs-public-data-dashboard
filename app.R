# Primary Care Dementia Data explorer
#
# Shows NHS England dementia statistics for England and the 7 NHS regions.
# Reads data/dementia_tidy.csv (built by pipeline/run_pipeline.R) and the
# tables in config/. Shiny loads the helper files in R/ automatically:
#   R/data.R     loading the data and the measure picker choices
#   R/periods.R  month / quarter / financial-year views and date presets
#   R/scaling.R  turning counts into rates and percentages
#   R/plots.R    drawing one chart panel
#
# This file has five parts:
#   1. Data and settings   loaded once when the app starts
#   2. Helpers             small functions used by the server
#   3. Look and feel       theme, CSS and the "About the data" tab
#   4. User interface      sidebar controls and the three tabs
#   5. Server              turns the controls into charts, notes and a table

library(shiny)
library(bslib)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(readr)


# ---- 1. Data and settings ----------------------------------------------------

APP <- load_app_data()
TIDY <- APP$tidy          # one row per area x measure x category x period
MEASURES <- APP$measures  # config/measures.csv: labels, units, descriptions
ORGS <- TIDY |> distinct(org_code, org_name, geo_level) |> arrange(geo_level, org_name)  # England, then regions A-Z
ORG_COLOURS <- org_colours(ORGS)
MIN_DATE <- min(TIDY$period_end)
MAX_DATE <- max(TIDY$period_end)

# Breakdown categories of each measure (e.g. the ethnic groups for ETHNICITY)
CATEGORIES <- TIDY |> filter(submeasure != "") |> distinct(measure, submeasure)

MAX_PANELS <- 12                       # most charts shown at once
MAX_LINES <- length(SERIES_COLOURS)    # one colour per line, so at most 8 lines per chart


# ---- 2. Helpers --------------------------------------------------------------

# Breakdown categories of a measure; empty for most measures
submeasures_of <- function(m) sort(CATEGORIES$submeasure[CATEGORIES$measure == m])
has_submeasures <- function(m) length(submeasures_of(m)) > 0

# Categories selected when a breakdown is first chosen: those with the longest
# run of data (e.g. the 65+ age bands, which monthly releases cover), at most one per colour
default_submeasures <- function(m) {
  TIDY |>
    filter(measure == m, submeasure != "") |>
    count(submeasure, name = "n_periods") |>          # one row per period, so this counts periods
    filter(n_periods == max(n_periods)) |>
    pull(submeasure) |>
    sort() |>
    head(MAX_LINES)
}

# Aggregates are built from practices (monthly releases) or sub-ICBs
units_word <- function(unit_type) if_else(unit_type == "practice", "practices", "sub-ICBs")
# TRUE where some of those units were suppressed (use on the n_units_missing column)
any_suppressed <- function(n_units_missing) !is.na(n_units_missing) & n_units_missing > 0
# Thousands separators without padding (format() would pad a vector to equal width)
with_commas <- function(x) prettyNum(x, big.mark = ",")

# -- Preparing the data for the current selection --

# Rows for the chosen measures and areas, keeping only the chosen categories of
# each breakdown. `extra` measures (denominators, CI limits) are kept whole.
select_rows <- function(measures, extra, orgs, chosen_categories) {
  d <- TIDY |> filter(measure %in% c(measures, extra), org_code %in% orgs)
  for (m in Filter(has_submeasures, measures)) {
    d <- d |> filter(measure != m | submeasure %in% chosen_categories[[m]])
  }
  d
}

# Confidence limits are stored as their own measures (e.g. DIAG_RATE_65_PLUS_LL).
# Copy them onto the measure they belong to as ci_lower / ci_upper columns.
add_ci_limits <- function(main, all_rows, ci_measures) {
  main <- mutate(main, ci_lower = NA_real_, ci_upper = NA_real_)
  for (i in seq_len(nrow(ci_measures))) {
    ci <- ci_measures[i, ]
    # One row per area and period, with the lower and upper limit side by side
    limits <- all_rows |>
      filter(measure %in% c(ci$ci_lower, ci$ci_upper)) |>
      select(org_code, period_end, measure, value) |>
      pivot_wider(names_from = measure, values_from = value) |>
      rename(lower = all_of(ci$ci_lower), upper = all_of(ci$ci_upper))
    if (!nrow(limits)) next
    main <- main |>
      left_join(limits, by = c("org_code", "period_end")) |>
      mutate(ci_lower = if_else(measure == ci$measure, lower, ci_lower),
             ci_upper = if_else(measure == ci$measure, upper, ci_upper)) |>
      select(-lower, -upper)
  }
  main
}

# Add the text the charts and table need: labels, unit, period name and what
# each value means in this time view (e.g. "Total over the quarter")
describe_rows <- function(d, view, denom_code, show_benchmark) {
  d |>
    left_join(select(MEASURES, measure, measure_label = label, measure_unit = unit), by = "measure") |>
    mutate(
      unit = unit_label(measure_unit, scaled, denom_code, APP$denominators),
      period = vapply(period_end, period_label, "", view = view),
      basis = time_type_basis(time_type, view),
      benchmark = show_benchmark & org_code == "ENG"
    )
}

# -- Building chart panels --

# Split rows into chart panels (the rows arrive sorted, so panels keep the order
# the measures were picked in):
#  - combined:        one panel per unit (measures can only share an axis if they share a unit)
#  - comparing areas: one panel per measure and category, a line per area
#  - one area:        one panel per measure, a line per category
split_into_panels <- function(d, combine, multi_org) {
  panel_key <- if (combine) d$unit
               else if (multi_org) paste(d$measure, d$submeasure)
               else d$measure
  # factor levels in order of first appearance keep the panels in picking order
  panels <- split(d, factor(panel_key, levels = unique(panel_key)))
  head(unname(panels), MAX_PANELS)
}

# Name, colour and line style of each line in a panel. Returns the panel rows
# and any categories left out because there are more lines than colours.
style_lines <- function(pd, multi_org) {
  labels <- unique(pd$label)
  dropped <- character(0)
  if (multi_org) {
    # Colour by area; with several measures in one panel, line style tells them apart
    pd <- pd |> mutate(
      colour = unname(ORG_COLOURS[org_name]),
      series = if (length(labels) > 1) paste0(org_name, " – ", label) else org_name,
      dash = c("solid", "dash", "dot", "dashdot")[(match(label, labels) - 1L) %% 4L + 1L]
    )
    # England benchmark is grey; with several measures it keeps the measure's line style
    pd <- pd |> mutate(
      colour = if_else(benchmark, INK[["secondary"]], colour),
      dash = if_else(benchmark & length(labels) == 1, "dash", dash)
    )
  } else if (length(labels) > 1) {
    # Colour by measure or category, one colour slot each
    dropped <- setdiff(labels, head(labels, MAX_LINES))
    labels <- head(labels, MAX_LINES)
    pd <- pd |>
      filter(label %in% labels) |>
      mutate(colour = SERIES_COLOURS[match(label, labels)],
             series = if_else(benchmark, paste0("England – ", label), label),
             dash = if_else(benchmark, "dash", "solid"))
  } else {
    # A single line, plus the England benchmark if shown
    pd <- pd |> mutate(series = org_name, colour = unname(ORG_COLOURS[org_name]),
                       dash = if_else(benchmark, "dash", "solid"))
  }
  list(data = pd, dropped = dropped)
}

# Tooltip for each point: value, what it means, the sum behind a rate, and how
# many practices or sub-ICBs were suppressed. (panel_unit is named so it can't be
# confused with the data's own `unit` column inside mutate().)
add_hover_text <- function(pd, panel_unit) {
  pd |> mutate(hover = paste0(
    "<b>", series, "</b><br>", period, "<br>",
    "<b>", format_value(value, panel_unit), "</b>",
    if (panel_unit %in% c("Count", "%")) "" else paste0(" ", panel_unit), "<br>",
    basis,
    if_else(scaled, paste0("<br>", format_value(numerator, "Count"), " ÷ ",
                           format_value(denominator_value, "Count")), ""),
    if_else(any_suppressed(n_units_missing),
            paste0("<br>", with_commas(n_units_missing), " of ", with_commas(n_units), " ",
                   units_word(unit_type), " suppressed (left out)"), "")
  ))
}

# Everything needed to draw one panel
make_panel <- function(rows, i, multi_org) {
  styled <- style_lines(rows, multi_org)
  unit <- styled$data$unit[1]
  pd <- add_hover_text(styled$data, unit)
  measures <- unique(pd$measure)
  title <- if (length(measures) > 1) paste(unique(pd$measure_label), collapse = " · ")
           else if (multi_org && pd$submeasure[1] != "") pd$label[1]
           else pd$measure_label[1]
  list(id = paste0("plot_", i), data = pd, unit = unit, title = title, measures = measures,
       orgs = unique(pd$org_code), time_types = unique(pd$time_type),
       suppressed = filter(pd, any_suppressed(n_units_missing)), dropped = styled$dropped)
}

# The card around one chart: title, badges, warnings, description and the plot
panel_card <- function(p) {
  info <- MEASURES[match(p$measures, MEASURES$measure), ]
  description <- if (nrow(info) > 1) {
    tags$ul(class = "panel-desc", lapply(seq_len(nrow(info)), function(j)
      tags$li(tags$b(info$label[j]), ": ", info$description[j])))
  } else {
    div(class = "panel-desc", info$description)
  }
  suppression <- if (nrow(p$suppressed)) {
    worst <- p$suppressed[which.max(p$suppressed$n_units_missing / p$suppressed$n_units), ]
    div(class = "supp-note", sprintf(
      "⚠ Some values suppressed: up to %s of %s %s left out (%s, %s). Hover points for details.",
      with_commas(worst$n_units_missing), with_commas(worst$n_units),
      units_word(worst$unit_type), worst$org_name, worst$period))
  }
  too_many_lines <- if (length(p$dropped)) {
    div(class = "supp-note", "Only 8 lines fit on one chart; not shown: ", paste(p$dropped, collapse = ", "))
  }
  card(
    card_header(div(
      div(class = "panel-title", p$title),
      div(class = "panel-meta",
          lapply(p$time_types, function(t) span(class = "tt-badge", TIME_TYPE_LABELS[[t]])), p$unit),
      suppression,
      too_many_lines
    )),
    description,
    # Taller panels for long legends so the plot area keeps its size
    plotlyOutput(p$id, height = paste0(300 + 24 * max(0, n_distinct(p$data$series) - 2), "px"))
  )
}

# -- Table --

# Rows for the Table tab and the CSV download
table_rows <- function(d) {
  d |>
    arrange(measure_label, submeasure, org_name, period_end) |>
    transmute(
      Period = period, `Period end` = period_end, Area = org_name, Measure = measure_label,
      Category = submeasure, Value = round(value, 3), Unit = unit, `What the value is` = basis,
      Numerator = if_else(scaled, numerator, NA_real_), Denominator = denominator_value,
      `Units suppressed` = if_else(any_suppressed(n_units_missing),
                                   paste(n_units_missing, "of", n_units, units_word(unit_type)), ""),
      `Lower 95% CI` = ci_lower, `Upper 95% CI` = ci_upper, Source = source
    )
}


# ---- 3. Look and feel ----------------------------------------------------------

theme <- bs_theme(
  version = 5, bg = "#f9f9f7", fg = INK[["primary"]], primary = SERIES_COLOURS[1],
  secondary = INK[["secondary"]], base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "sans-serif"),
  "border-radius" = "0.5rem", "card-border-color" = "rgba(11,11,11,0.10)"
)

css <- "
  .app-title { font-weight: 650; font-size: 1.15rem; margin: 0; }
  .app-subtitle { color: #52514e; font-size: 0.85rem; }
  .bslib-sidebar-layout > .sidebar { background: #fcfcfb; }
  .card { background: #fcfcfb; box-shadow: none; }
  .panel-title { font-weight: 600; }
  .panel-meta { color: #52514e; font-size: 0.82rem; margin-top: 2px; }
  .panel-desc { color: #52514e; font-size: 0.82rem; margin: 0 0 4px; }
  .tt-badge { display: inline-block; font-size: 0.72rem; font-weight: 600; padding: 1px 8px;
              border-radius: 999px; background: #eef4fc; color: #1c5cab; margin-right: 6px; }
  .supp-note { color: #8a5a00; font-size: 0.8rem; margin-top: 2px; }
  .note-list { margin: 0 0 12px; padding: 10px 14px; border-radius: 0.5rem; background: #fff8e6;
               border: 1px solid #f3dca3; font-size: 0.88rem; }
  .note-list ul { margin: 0; padding-left: 18px; }
  .sidebar .form-label, .sidebar .control-label { font-weight: 600; font-size: 0.88rem; }
  .help { color: #52514e; font-size: 0.78rem; margin-top: -6px; margin-bottom: 10px; }
  .about h5 { margin-top: 1rem; }
"

about_ui <- function() {
  div(class = "about",
    p("Data: NHS England, ",
      a("Primary Care Dementia Data", target = "_blank",
        href = "https://digital.nhs.uk/data-and-information/publications/statistical/primary-care-dementia-data"),
      ". Monthly releases to March 2026, quarterly from June 2026. Each month or quarter is taken from the latest release that includes it."),
    h5("What each point means"),
    tags$ul(
      tags$li(tags$b("Point in time: "), "a count on the last day of the period (e.g. the dementia register). Quarters and financial years show the value at the end of the period."),
      tags$li(tags$b("Count in period: "), "new events in the period (e.g. new diagnoses, medication reviews). Quarters and financial years add the months up, and are only shown when every month is available."),
      tags$li(tags$b("Rolling 12 months: "), "events in the 12 months to the end of the period (e.g. care plans). Shown as the end-of-period value."),
      tags$li(tags$b("Financial year to date: "), "events since 1 April. Shown as the end-of-period value; the matching 'in period' measure gives monthly or quarterly counts.")
    ),
    h5("Rates"),
    p("Rates per 1,000 use registered patients for the same area and period. Shares of the dementia register or of the estimated dementia population are shown as percentages. Only pairings that make sense for a measure are offered."),
    h5("Suppressed values"),
    p("Before 2026/27 some practice-level and sub-ICB-level values were suppressed (*) to protect small numbers. Regional and England figures built from them leave those values out, so they can be slightly low. Charts and the table show how many practices or sub-ICBs were excluded."),
    h5("Series breaks"),
    tableOutput("breaks_table")
  )
}


# ---- 4. User interface ---------------------------------------------------------

sidebar_controls <- sidebar(
  width = 320,

  # What to show
  selectizeInput("measures", "Measures", choices = measure_choices(MEASURES, TIDY, basic_only = TRUE),
                 selected = "DEMENTIA_REGISTER", multiple = TRUE,
                 options = list(maxItems = 4, plugins = list("remove_button"))),
  input_switch("all_measures", "Show all measures", FALSE),
  conditionalPanel("input.measures && input.measures.length > 1",
                   checkboxInput("combine", "Show measures on one chart", FALSE)),
  uiOutput("submeasure_ui"),   # a category picker for each breakdown measure

  # Where
  checkboxGroupInput("orgs", "Areas", choices = setNames(ORGS$org_code, ORGS$org_name), selected = "ENG"),
  conditionalPanel("input.orgs && input.orgs.indexOf('ENG') < 0",
                   checkboxInput("benchmark", "Show England for comparison", TRUE)),

  # How
  selectInput("scale", "Show as", choices = c("Count" = "count")),   # rate choices filled in by the server
  radioButtons("view", "Time view", TIME_VIEWS, inline = TRUE),
  div(class = "help", "Point-in-time measures show the end-of-period value; counts in a period are added up."),

  # When
  selectInput("preset", "Date range", DATE_PRESETS, selected = "all"),
  conditionalPanel("input.preset == 'custom'",
                   dateRangeInput("custom_range", NULL, start = MIN_DATE, end = MAX_DATE,
                                  min = MIN_DATE, max = MAX_DATE, format = "M yyyy", startview = "year")),
  uiOutput("ci_ui")            # confidence interval switch, only for measures that have one
)

ui <- page_sidebar(
  title = div(div(class = "app-title", "Dementia in primary care"),
              div(class = "app-subtitle", "NHS England Primary Care Dementia Data · England and NHS regions")),
  theme = theme,
  tags$head(tags$style(HTML(css))),
  sidebar = sidebar_controls,
  navset_card_underline(
    nav_panel("Charts", uiOutput("notes"), uiOutput("panels")),
    nav_panel("Table",
      div(style = "display:flex; justify-content:flex-end; margin-bottom:8px;",
          downloadButton("download", "Download CSV", class = "btn-sm btn-outline-primary")),
      DTOutput("table")),
    nav_panel("About the data", about_ui())
  )
)


# ---- 5. Server -----------------------------------------------------------------
# The flow is: controls -> view_data() (the rows to show) -> panels() (rows split
# into charts) -> charts, notes and table.

server <- function(input, output, session) {

  # ---- Controls that depend on other controls ----

  # One category picker per chosen breakdown measure (keeps earlier choices)
  output$submeasure_ui <- renderUI({
    lapply(Filter(has_submeasures, input$measures), function(m) {
      id <- paste0("sub_", m)
      selectizeInput(id, paste0(MEASURES$label[MEASURES$measure == m], ": categories"), choices = submeasures_of(m),
                     selected = isolate(input[[id]]) %||% default_submeasures(m), multiple = TRUE,
                     options = list(maxItems = MAX_LINES, plugins = list("remove_button")))
    })
  })

  # Basic list by default; the switch adds every measure. Choices not in the
  # new list are dropped, falling back to the dementia register.
  observeEvent(input$all_measures, {
    choices <- measure_choices(MEASURES, TIDY, basic_only = !isTRUE(input$all_measures))
    keep <- intersect(input$measures, unlist(choices))
    updateSelectizeInput(session, "measures", choices = choices,
                         selected = if (length(keep)) keep else "DEMENTIA_REGISTER")
  }, ignoreInit = TRUE)

  # "Show as" offers only the rates that make sense for the chosen measures
  observeEvent(input$measures, {
    choices <- scale_choices(input$measures, APP$pairs, APP$denominators)
    keep <- if (isTRUE(input$scale %in% choices)) input$scale else "count"
    updateSelectInput(session, "scale", choices = choices, selected = keep)
  }, ignoreNULL = FALSE)

  # Chosen measures that have confidence limits (currently the diagnosis rate)
  ci_measures <- reactive(filter(MEASURES, measure %in% input$measures, ci_lower != ""))
  output$ci_ui <- renderUI({
    if (nrow(ci_measures())) checkboxInput("show_ci", "Show 95% confidence interval", TRUE)
  })

  # First and last day of the date range shown
  date_range <- reactive({
    if (identical(input$preset, "custom")) {
      req(input$custom_range)
      from <- input$custom_range[1]
      to <- input$custom_range[2]
      return(c(as.Date(format(from, "%Y-%m-01")), month_end(year_of(to), month_of(to))))
    }
    c(preset_start(input$preset, MAX_DATE, MIN_DATE), MAX_DATE)
  })

  combine <- reactive(isTRUE(input$combine) && length(input$measures) > 1)

  # ---- The rows to show ----
  # Select -> put into the time view -> cut to the date range -> scale to rates
  # -> attach confidence limits -> add labels

  view_data <- reactive({
    measures <- req(input$measures)
    orgs <- req(input$orgs)
    show_benchmark <- !"ENG" %in% orgs && isTRUE(input$benchmark)
    denom_code <- input$scale %||% "count"   # "count", or the measure to divide by
    ci <- ci_measures()

    breakdowns <- Filter(has_submeasures, measures)
    chosen_categories <- setNames(lapply(breakdowns, function(m) input[[paste0("sub_", m)]] %||% default_submeasures(m)), breakdowns)
    extra <- c(ci$ci_lower, ci$ci_upper, if (denom_code != "count") denom_code)
    rows <- select_rows(measures, extra, unique(c(orgs, if (show_benchmark) "ENG")), chosen_categories)

    hidden <- dropped_by_view(filter(rows, measure %in% measures), input$view)   # quarterly data in Month view
    date_window <- date_range()
    rows <- rows |>
      aggregate_view(input$view) |>
      filter(period_end >= date_window[1], period_end <= date_window[2])

    main <- apply_scaling(filter(rows, measure %in% measures), filter(rows, measure == denom_code), denom_code,
                          APP$pairs, APP$denominators)
    main <- add_ci_limits(main, rows, ci)
    main <- describe_rows(main, input$view, denom_code, show_benchmark)

    # England counts dwarf regional ones, so the comparison is only drawn for rates and percentages
    count_benchmark <- main |> filter(benchmark, unit == "Count") |> pull(measure_label) |> unique()
    main <- main |> filter(!(benchmark & unit == "Count"))
    not_scaled <- if (denom_code == "count") character(0)
                  else main |> filter(!scaled, measure_unit == "count") |> pull(measure_label) |> unique()

    list(data = main, range = date_window, multi_org = length(orgs) > 1,
         hidden = hidden, not_scaled = not_scaled, count_benchmark = count_benchmark)
  })

  # ---- Charts ----

  panels <- reactive({
    view <- view_data()
    if (!nrow(view$data)) return(list())
    d <- view$data |>
      mutate(label = if_else(submeasure == "", measure_label, paste0(measure_label, ": ", submeasure)),
             pick_order = match(measure, input$measures)) |>
      arrange(pick_order, submeasure, org_name, period_end)
    groups <- split_into_panels(d, combine(), view$multi_org)
    lapply(seq_along(groups), function(i) make_panel(groups[[i]], i, view$multi_org))
  })

  output$panels <- renderUI({
    ps <- panels()
    if (!length(ps)) return(div(class = "note-list", "No data for this selection. Try a wider date range or another time view."))
    layout_column_wrap(width = if (length(ps) > 1) "520px" else 1, !!!lapply(ps, panel_card))
  })

  # Draw each panel's chart, with the series breaks that apply to its measures and areas
  observe({
    ps <- panels()
    date_window <- isolate(view_data())$range
    for (p in ps) local({
      panel <- p
      breaks <- APP$breaks |>
        filter(measure == "" | measure %in% panel$measures, org_code == "" | org_code %in% panel$orgs)
      output[[panel$id]] <- renderPlotly(
        panel_plot(panel$data, panel$unit, breaks, input$view, date_window, show_ci = isTRUE(input$show_ci))
      )
    })
  })

  # Yellow notes above the charts explaining what is (not) shown
  output$notes <- renderUI({
    view <- view_data()
    measure_list <- function(m) paste(MEASURES$label[MEASURES$measure %in% m], collapse = ", ")
    too_many_panels <- view$multi_org && !combine() &&
      nrow(distinct(view$data, measure, submeasure)) > MAX_PANELS
    notes <- c(
      if (length(view$hidden))
        sprintf("Data from June 2026 is published quarterly, so the Month view stops at March 2026. Switch to the Quarter view to see later data for: %s.",
                measure_list(view$hidden)),
      if (combine() && n_distinct(view$data$unit) > 1)
        "Measures in different units can't share an axis, so they are on separate charts.",
      if (length(view$count_benchmark))
        sprintf("England is only drawn for comparison when values are rates or percentages (its counts are far larger than any region's). Choose a rate under 'Show as' to compare %s with England.",
                paste(view$count_benchmark, collapse = ", ")),
      if (length(view$not_scaled))
        sprintf("Shown as counts because no rate with this denominator applies: %s.", paste(view$not_scaled, collapse = ", ")),
      if (input$view == "fy")
        "Financial years appear once complete: counts need all 12 months, point-in-time values need the March figure.",
      if (too_many_panels)
        sprintf("Showing the first %d charts. Choose fewer categories to see the rest.", MAX_PANELS)
    )
    if (length(notes)) div(class = "note-list", tags$ul(lapply(notes, tags$li)))
  })

  # ---- Table and download ----

  table_data <- reactive(table_rows(view_data()$data))

  output$table <- renderDT({
    td <- table_data()
    # Leave out columns with nothing in them for this selection (e.g. CIs, numerators)
    empty <- vapply(td, function(x) all(is.na(x) | (is.character(x) & x == "")), logical(1))
    td <- td[, !empty]
    number_cols <- which(names(td) %in% c("Value", "Numerator", "Denominator", "Lower 95% CI", "Upper 95% CI")) - 1L
    # Whole numbers without decimals, rates to 2 places (DataTables counts columns from 0)
    format_number <- JS("function(d, type) { if (type !== 'display' || d === null) return d;",
                        "return Number(d).toLocaleString('en-GB', {maximumFractionDigits: Number.isInteger(d) ? 0 : 2}); }")
    datatable(td, rownames = FALSE, class = "compact stripe hover",
              options = list(pageLength = 25, dom = "ftip", scrollX = TRUE,
                             columnDefs = list(list(targets = number_cols, render = format_number, className = "dt-right")),
                             order = list(list(which(names(td) == "Period end") - 1L, "desc"))))
  })

  output$download <- downloadHandler(
    filename = function() paste0("dementia_data_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) write_csv(table_data(), file, na = "")
  )

  # Series breaks listed on the "About the data" tab
  output$breaks_table <- renderTable({
    APP$breaks |>
      arrange(period_end) |>
      transmute(
        From = format(period_end, "%b %Y"),
        Measure = if_else(measure == "", "All measures", MEASURES$label[match(measure, MEASURES$measure)]),
        Area = if_else(org_code == "", "All areas", ORGS$org_name[match(org_code, ORGS$org_code)]),
        Change = description
      )
  })
}

shinyApp(ui, server)
