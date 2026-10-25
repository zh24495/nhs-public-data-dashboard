# Loading the data
#
# Reads the pipeline's output and the config tables once, when the app starts.
#   load_app_data()    everything the app needs, as a list of data frames
#   measure_choices()  the grouped list shown in the "Measures" picker


# Read a config CSV with every column as text, keeping empty cells as "" rather
# than NA (an empty `measure` in series_breaks.csv means "all measures")
read_config <- function(name, dir) {
  read_csv(file.path(dir, "config", name), col_types = cols(.default = col_character()),
           na = character(), progress = FALSE)
}

# Dates typed into the config files. Accepts 2022-04-30 or 30/04/2022 (Excel
# re-saves dates in the second form). Stops rather than guess at anything else.
parse_config_date <- function(x, file) {
  out <- as.Date(x, format = "%Y-%m-%d")
  day_first <- is.na(out)
  out[day_first] <- as.Date(x[day_first], format = "%d/%m/%Y")
  bad <- x[is.na(out)]
  if (length(bad)) stop("Unrecognised date(s) in ", file, ": ", paste(bad, collapse = ", "), call. = FALSE)
  out
}

load_app_data <- function(dir = ".") {
  measures <- read_config("measures.csv", dir)

  # The tidy data: one row per area x measure x category x period. Empty text
  # cells become "" (e.g. submeasure is "" for measures without categories).
  tidy <- read_csv(
    file.path(dir, "data", "dementia_tidy.csv"),
    col_types = cols(period_end = col_date(), period_months = col_integer(), value = col_double(),
                     n_units = col_integer(), n_units_missing = col_integer(), .default = col_character()),
    na = "", progress = FALSE
  ) |>
    mutate(across(c(submeasure, unit_type, source), ~ coalesce(.x, ""))) |>
    # time_type (stock / flow / rolling12 / fytd) decides how periods are combined
    left_join(select(measures, measure, time_type), by = "measure")

  breaks <- read_config("series_breaks.csv", dir) |>
    mutate(period_end = parse_config_date(period_end, "series_breaks.csv"))

  list(
    tidy = tidy,
    measures = measures,
    pairs = read_config("measure_denominators.csv", dir),   # which rates suit which measure
    denominators = read_config("denominators.csv", dir),    # how each rate is worded and scaled
    breaks = breaks
  )
}

# Choices for the measure picker, grouped under headings (Diagnosis, Care, ...).
# Only measures that have data are offered; hidden ones (CI limits) never are.
# basic_only keeps the short starter list (basic = TRUE in measures.csv).
measure_choices <- function(measures, tidy, basic_only = FALSE) {
  offered <- measures |> filter(group != "Hidden", measure %in% tidy$measure)
  if (basic_only) offered <- filter(offered, basic == "TRUE")

  groups <- unique(offered$group)
  choices <- lapply(groups, function(g) {
    in_group <- filter(offered, group == g)
    setNames(in_group$measure, in_group$label)   # shows the label, returns the code
  })
  setNames(choices, groups)
}
