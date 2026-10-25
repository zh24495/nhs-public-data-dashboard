# Time views: month, quarter and financial year
#
# The data holds one value per month (to March 2026) or per quarter (from June
# 2026). This file turns those into the time view the user picks, and supplies
# the labels and date presets that go with it.

TIME_VIEWS <- c("Month" = "month", "Quarter" = "quarter", "Financial year" = "fy")
VIEW_MONTHS <- c(month = 1L, quarter = 3L, fy = 12L)   # months in one period of each view


# ---- What a value means ----

# Badge shown on each chart, by the measure's time type (from measures.csv)
TIME_TYPE_LABELS <- c(
  stock     = "Point in time",           # e.g. people on the register on the last day
  flow      = "Count in period",         # e.g. new diagnoses during the period
  rolling12 = "Rolling 12 months",       # e.g. care plans in the 12 months to the period end
  fytd      = "Financial year to date"   # e.g. medication reviews since 1 April
)

# One-line explanation for tooltips and the table, e.g. "Total over the quarter"
time_type_basis <- function(time_type, view) {
  period <- c(month = "month", quarter = "quarter", fy = "financial year")[[view]]
  unname(c(
    stock     = paste0("Position at end of ", period),
    flow      = paste0("Total over the ", period),
    rolling12 = paste0("12 months to end of ", period),
    fytd      = paste0("Year to date at end of ", period)
  )[time_type])
}


# ---- Date arithmetic ----
# Every period is identified by its last day, e.g. 2026-03-31 for March 2026.

year_of  <- function(d) as.integer(format(d, "%Y"))
month_of <- function(d) as.integer(format(d, "%m"))

# Last day of month m of year y
month_end <- function(y, m) {
  next_y <- y + (m == 12L)
  next_m <- m %% 12L + 1L
  as.Date(sprintf("%04d-%02d-01", next_y, next_m)) - 1
}

# Last day of the quarter / financial year (April to March) containing each date
quarter_end <- function(d) month_end(year_of(d), ((month_of(d) - 1L) %/% 3L + 1L) * 3L)
fy_end      <- function(d) as.Date(sprintf("%04d-03-31", year_of(d) + (month_of(d) >= 4L)))

# Month end n months after (or before, if n < 0) the month of d
add_months <- function(d, n) {
  months_since_year_0 <- year_of(d) * 12L + month_of(d) - 1L + n
  month_end(months_since_year_0 %/% 12L, months_since_year_0 %% 12L + 1L)
}

# How a period is written: "Mar 2026", "Jan–Mar 2026" or "2025/26"
period_label <- function(period_end, view) {
  switch(view,
    month   = format(period_end, "%b %Y"),
    quarter = paste0(format(add_months(period_end, -2L), "%b"), "–", format(period_end, "%b %Y")),
    fy      = sprintf("%d/%02d", year_of(period_end) - 1L, year_of(period_end) %% 100L)
  )
}


# ---- Date range presets ----

DATE_PRESETS <- c("Last 12 months" = "12", "Last 2 years" = "24", "Last 5 years" = "60",
                  "All data" = "all", "Custom range" = "custom")

# First day shown for a preset, counting back from the latest period
preset_start <- function(preset, max_date, min_date) {
  if (preset %in% c("all", "custom")) return(min_date)
  add_months(max_date, -as.integer(preset)) + 1
}


# ---- Putting the data into a time view ----

# Columns kept in every view
VIEW_COLUMNS <- c("geo_level", "org_code", "org_name", "measure", "submeasure", "time_type",
                  "period_end", "period_months", "value", "source", "unit_type", "n_units", "n_units_missing")

# Month view: monthly rows only (quarterly data from June 2026 needs the
#   Quarter view).
# Quarter / financial year view:
#   - point-in-time, rolling 12-month and year-to-date measures take the value
#     at the end of the period;
#   - counts in a period are added up, and only kept when every month (or
#     quarter) of the period is there.
aggregate_view <- function(d, view) {
  if (view == "month") {
    return(d |> filter(period_months == 1L) |> select(all_of(VIEW_COLUMNS)))
  }
  months_needed <- VIEW_MONTHS[[view]]
  d <- mutate(d, bucket = if (view == "quarter") quarter_end(period_end) else fy_end(period_end))

  # Values at the end of each period
  end_values <- d |>
    filter(time_type != "flow", period_end == bucket) |>
    mutate(period_months = pmax(period_months, months_needed)) |>
    select(all_of(VIEW_COLUMNS))

  # summarise() would still evaluate max() on an empty table and warn
  if (!any(d$time_type == "flow")) return(end_values)

  # Totals over each period, built from monthly (or quarterly) counts
  totals <- d |>
    filter(time_type == "flow") |>
    group_by(geo_level, org_code, org_name, measure, submeasure, time_type, bucket) |>
    summarise(
      value = sum(value),
      months_covered = sum(period_months),
      source = paste(unique(source), collapse = "/"),
      unit_type = unit_type[1],
      n_units = max(n_units),
      n_units_missing = max(n_units_missing),   # worst month in the period
      .groups = "drop"
    ) |>
    filter(months_covered == months_needed) |>
    mutate(period_end = bucket, period_months = months_covered) |>
    select(all_of(VIEW_COLUMNS))

  bind_rows(end_values, totals)
}

# Measures with data the chosen view can't show (quarterly data in Month view),
# so the app can say so
dropped_by_view <- function(d, view) {
  if (view != "month") return(character(0))
  d |> filter(period_months > 1L) |> distinct(measure) |> pull(measure)
}
