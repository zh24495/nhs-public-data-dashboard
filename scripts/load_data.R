library(dplyr)
library(here)
library(readr)
library(fs)


##Create database files folder
database_dir <- "data/database_files/"
dir_create(here::here(database_dir))

#Find data files for rate
rate_files <- list.files(
  here("data", "measures"),
  pattern = "rate",
  full.names = TRUE
)

#Just working with one for now
f <- rate_files[1]

#Load files
dem_rate <- read_csv(
  f, 
  col_select = c(
    name=NAME,
    org_code=ORG_CODE,
    org_type=ORG_TYPE,
    date=ACH_DATE,
    measure = MEASURE,
    value=VALUE))

#Load geography data
geography_dim <- read_csv(here("data", "gp-reg-pat-prac-map-03-2026.csv"))


#Find data for COMORBIDITIES
comor_files <- list.files(
  here("data", "measures"),
  pattern = "comor",
  full.names = TRUE
)

#Just working with one for now
com <- comor_files[1]
#Load files
comor_rate <- read_csv(
  com, 
  col_select = c(
    name=NAME,
    org_code=ORG_CODE,
    org_type=ORG_TYPE,
    date=ACH_DATE,
    measure = Measure,
    value=Value)) %>%
  filter(measure !="DEMENTIA_REGISTER_65_PLUS")


measure_fact <- bind_rows(dem_rate, comor_rate)

library(dplyr)
library(here)
library(readr)
library(fs)

##Create database files folder
database_dir <- "data/database_files/"
dir_create(here::here(database_dir))

#Find data files for rate
rate_files <- list.files(
  here("data", "measures"),
  pattern = "rate",
  full.names = TRUE
)
#Just working with one for now
f <- rate_files[1]
#Load files
dem_rate <- read_csv(
  f, 
  col_select = c(
    name=NAME,
    org_code=ORG_CODE,
    org_type=ORG_TYPE,
    date=ACH_DATE,
    measure = MEASURE,
    value=VALUE))

#Load geography data
geography_dim <- read_csv(here("data", "gp-reg-pat-prac-map-03-2026.csv"))

#Find data for COMORBIDITIES
comor_files <- list.files(
  here("data", "measures"),
  pattern = "comor",
  full.names = TRUE
)
#Just working with one for now
com <- comor_files[1]
#Load files
comor_rate <- read_csv(
  com, 
  col_select = c(
    name=NAME,
    org_code=ORG_CODE,
    org_type=ORG_TYPE,
    date=ACH_DATE,
    measure = Measure,
    value=Value)) %>%
  filter(measure !="DEMENTIA_REGISTER_65_PLUS")

measure_fact <- bind_rows(dem_rate, comor_rate)

# --------------------------------------------------
# STANDARDISE org_type LABELS
# --------------------------------------------------
# Different source files use inconsistent org_type labels
# (e.g. "COUNTRY" vs "COUNTRY_RESPONSIBILITY"). Map every
# known variant to a single canonical value here so new
# measure files don't silently create a duplicate category.

org_type_map <- c(
  "COUNTRY"                = "COUNTRY_RESPONSIBILITY",
  "COUNTRY_RESPONSIBILITY" = "COUNTRY_RESPONSIBILITY",
  "ICB"                    = "ICB",
  "NHS_REGION"             = "NHS_REGION",
  "REGION"                 = "NHS_REGION",
  "SUB_ICB_LOC"            = "SUB_ICB_LOC",
  "SUB_ICB"                = "SUB_ICB_LOC"
)

measure_fact <- measure_fact %>%
  mutate(
    org_type = recode(org_type, !!!org_type_map)
  ) %>%
  mutate(
    name = str_trim(toupper(name))
  )

# Flag anything that didn't match a known label, so unmapped
# variants get caught here instead of silently vanishing from the app
unmapped <- measure_fact %>%
  filter(!org_type %in% org_type_map) %>%
  distinct(org_type)

if (nrow(unmapped) > 0) {
  warning(
    "Unmapped org_type value(s) found: ",
    paste(unmapped$org_type, collapse = ", "),
    " — add these to org_type_map in load script."
  )
}

saveRDS(
  measure_fact,
  here("data","database_files", "measure_fact.rds")
)
saveRDS(
  geography_dim,
  here("data", "database_files", "practice_dim.rds")
)
saveRDS(
  measure_fact,
  here("data","database_files", "measure_fact.rds")
)
saveRDS(
  geography_dim,
  here("data", "database_files", "practice_dim.rds")
)
