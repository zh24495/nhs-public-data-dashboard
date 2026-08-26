library(dplyr)
library(here)
library(readr)
library(fs)


##Create database files folder
database_dir <- "data/database_files/"
dir_create(here::here(database_dir))

#~Find data files
files <- list.files(
  here("data", "measures"),
  pattern = "\\.csv$",
  full.names = TRUE
)

#Just working with one for now
f <- files[1]

#Load files
measure_fact <- read_csv(
  f, 
  col_select = c(
    name=NAME,
    org_code=ORG_CODE,
    org_type=ORG_TYPE,
    date=ACH_DATE,
    measure = MEASURE,
    value=VALUE))

geography_dim <- read_csv(here("data", "gp-reg-pat-prac-map-03-2026.csv"))

saveRDS(
  measure_fact,
  here("data","database_files", "measure_fact.rds")
)
saveRDS(
  geography_dim,
  here("data", "database_files", "practice_dim.rds")
)
