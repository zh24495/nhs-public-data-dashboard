library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(fs)
library(here)
library(lubridate)

source(here("scripts", "helper_functions.R"))

# ------------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------------
org_config <- list(
  sub_icb = list(levels = c("SUB_ICB_NAME", "SUB_ICB_ODS_CODE"), type = "SUB_ICB",
                 code = "SUB_ICB_ODS_CODE", name = "SUB_ICB_NAME"),
  icb     = list(levels = c("ICB_NAME", "ICB_ODS_CODE"), type = "ICB",
                 code = "ICB_ODS_CODE", name = "ICB_NAME"),
  region  = list(levels = c("REGION_NAME", "REGION_ODS_CODE"), type = "REGION",
                 code = "REGION_ODS_CODE", name = "REGION_NAME"),
  country = list(levels = character(0), type = "COUNTRY", code = NULL, name = NULL)
)

# Every known org_type variant -> one canonical label
org_type_map <- c(
  COUNTRY                = "COUNTRY_RESPONSIBILITY",
  COUNTRY_RESPONSIBILITY = "COUNTRY_RESPONSIBILITY",
  ICB                    = "ICB",
  NHS_REGION             = "NHS_REGION",
  REGION                 = "NHS_REGION",
  SUB_ICB_LOC            = "SUB_ICB_LOC",
  SUB_ICB                = "SUB_ICB_LOC"
)

# Files already in long format (NAME, ORG_CODE, ORG_TYPE, ACH_DATE, MEASURE, VALUE).
# `pattern` is matched against file names in data/measures, so every
# year's file that matches is picked up automatically.
standard_specs <- list(
  list(pattern = "rate"),
  list(pattern = "comor", drop = "DEMENTIA_REGISTER_65_PLUS"),
  list(pattern = "incidence-onset-delirium"))

# Files that need pivoting via group_to_level() / make_wide().
#   measure = NULL  -> keep measure as is, submeasure blank
#   measure = "X"   -> measure becomes "X", old measure moves to submeasure
#   prep            -> optional pre-processing on the raw file
wide_specs <- list(
  list(pattern = "ass-plans"),
  list(pattern = "ethnicity", measure = "ETHNICITY"),
  list(pattern = "dem-type",  measure = "DEMENTIA_TYPE"),
  list(pattern = "res-type",  measure = "RES_TYPE"),
  list(pattern = "age-sex",   measure = "AGE",
       prep = \(d) filter(d, str_starts(Measure, "ALL_AGED_"))),
  list(pattern = "age-sex",   measure = "SEX",
       prep = \(d) d %>%
         filter(str_detect(Measure, "^(FEMALE|MALE)_AGED_")) %>%
         mutate(Measure = if_else(str_starts(Measure, "FEMALE_"), "FEMALE", "MALE")) %>%
         group_by(across(-Value)) %>%
         summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop"))
)

# ------------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------------
meas_dir <- here("data", "measures")

find_files <- function(pattern) {
  f <- list.files(meas_dir, pattern = paste0(pattern, ".*\\.csv$"), full.names = TRUE)
  if (!length(f)) warning("No files found for pattern: ", pattern)
  f
}

# Fallback reporting date from a file name like "...-mar-2026.csv" -> 2026-03-31
file_date <- function(f) {
  m <- str_match(basename(f), "-([a-z]{3})-(\\d{4})")
  ceiling_date(dmy(paste0("01-", m[2], "-", m[3])), "month") - days(1)
}

# Convert dates to Date whatever readr guessed (Date already, or text in
# several possible formats). Day-first is tried before month-first.
tidy_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  as_date(parse_date_time(as.character(x),
                          orders = c("ymd", "dmy", "d b y", "d B Y"),
                          quiet = TRUE))
}

read_standard <- function(file, drop = character(0)) {
  read_csv(file, show_col_types = FALSE) %>%
    rename_with(tolower) %>%                      # copes with MEASURE/Measure, VALUE/Value
    select(name, org_code, org_type, date = ach_date, measure, value) %>%
    filter(!measure %in% drop) %>%
    mutate(date       = tidy_date(date),
           value      = as.numeric(na_if(as.character(value), ".")),
           submeasure = "",
           source_file = basename(file))
}

read_wide <- function(file, measure_label = NULL, prep = identity) {
  raw <- read_csv(file, show_col_types = FALSE) %>% prep()
  
  out <- map_dfr(org_config, \(cfg) {
    group_to_level(raw, cfg$levels) %>%
      make_wide(cfg$type, cfg$code, cfg$name) %>%
      janitor::clean_names()
  })
  
  if (!"date" %in% names(out)) out$date <- file_date(file)
  out$date <- tidy_date(out$date)
  out$source_file <- basename(file)
  
  if (is.null(measure_label)) {
    mutate(out, submeasure = "")
  } else {
    mutate(out, submeasure = measure, measure = measure_label)
  }
}

# ------------------------------------------------------------------
# BUILD measure_fact
# ------------------------------------------------------------------
standard <- map_dfr(standard_specs, \(s)
                    map_dfr(find_files(s$pattern), read_standard, drop = s$drop %||% character(0)))

wide <- map_dfr(wide_specs, \(s)
                map_dfr(find_files(s$pattern), read_wide,
                        measure_label = s$measure, prep = s$prep %||% identity))

measure_fact <- bind_rows(standard, wide) %>%
  mutate(org_type = recode(org_type, !!!org_type_map),
         name     = str_trim(toupper(name))) %>%
  # guard against the same period appearing in more than one file
  distinct(org_type, org_code, measure, submeasure, date, .keep_all = TRUE)

# ------------------------------------------------------------------
# CHECKS
# ------------------------------------------------------------------
unmapped <- setdiff(unique(measure_fact$org_type), org_type_map)
if (length(unmapped) > 0) {
  warning("Unmapped org_type value(s): ", paste(unmapped, collapse = ", "),
          " - add to org_type_map.")
}

if (anyNA(measure_fact$date)) {
  bad <- measure_fact %>% filter(is.na(date)) %>% distinct(source_file)
  warning("Unparseable dates in: ", paste(bad$source_file, collapse = ", "))
}

# ------------------------------------------------------------------
# SAVE
# ------------------------------------------------------------------
geography_dim <- read_csv(here("data", "gp-reg-pat-prac-map-03-2026.csv"))

out_dir <- dir_create(here("data", "database_files"))
saveRDS(measure_fact,  path(out_dir, "measure_fact.rds"))   # drop source_file here if the app doesn't need it
saveRDS(geography_dim, path(out_dir, "geography_dim.rds"))
