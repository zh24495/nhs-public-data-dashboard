library(dplyr)
library(here)
library(readr)
library(fs)

#This function accepts an NHA dataset and groups by measure, date and desired summary level.
group_to_level <- function(dataset, levels = character(0)) {
  dataset <- dataset %>%
    mutate(
      Value = as.numeric(ifelse(Value == ".", NA, Value))
    ) %>%
    group_by(across(all_of(levels)), ACH_DATE, Measure) %>%
    summarise(
    value = sum(Value, na.rm = TRUE),
    .groups = "drop"
    )
}

#This function takes a public, grouped NHS dataset and adds a column for org_type so we can add them to measure_fact
make_wide <- function(dataset, org_type, org_code = NULL, name = NULL) {
  
  dataset %>%
    mutate(
      ORG_TYPE = org_type,
      ORG_CODE = if (!is.null(org_code)) .data[[org_code]] else "ENG",
      NAME = if (!is.null(name)) .data[[name]] else "England"
    ) %>%
    select(
      Date = ACH_DATE,
      ORG_TYPE,
      ORG_CODE,
      NAME,
      Measure,
      Value = value
    )
}
