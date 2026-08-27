library(dplyr)
library(here)
library(readr)
library(fs)

#Source functions
source("scripts/helper_functions.R")

#Org Level Config
org_config <- list(
  
  sub_icb = list(
    levels = c("SUB_ICB_NAME", "SUB_ICB_ODS_CODE"),
    type = "SUB_ICB",
    code = "SUB_ICB_ODS_CODE",
    name = "SUB_ICB_NAME"
  ),
  
  icb = list(
    levels = c("ICB_NAME", "ICB_ODS_CODE"),
    type = "ICB",
    code = "ICB_ODS_CODE",
    name = "ICB_NAME"
  ),
  
  region = list(
    levels = c("REGION_NAME", "REGION_ODS_CODE"),
    type = "REGION",
    code = "REGION_ODS_CODE",
    name = "REGION_NAME"
  ),
  
  country = list(
    levels = character(0),
    type = "COUNTRY",
    code = NULL,
    name = NULL
  )
)

##Create database files folder
database_dir <- "data/database_files/"
dir_create(here::here(database_dir))

#Load geography data
geography_dim <- read_csv(here("data", "gp-reg-pat-prac-map-03-2026.csv"))


##Start loading data for measures_fact (main table)
#Find data files for rate
rate_files <- list.files(
  here("data", "measures"),
  pattern = "rate",
  full.names = TRUE
)

#Just working with one for now - we will add more and then loop over these.
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
    value=VALUE)) %>%
  mutate(submeasure = "")


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
  filter(measure !="DEMENTIA_REGISTER_65_PLUS") %>%
  mutate(submeasure = "")


#combine these
measure_fact <- bind_rows(dem_rate, comor_rate)

#young onset / incidence / delirium
young_data <- read_csv(
  here("data", "measures", "pcdem-sicbl-incidence-onset-delirium-mar-2026.csv"),
  col_select = c(
    name=NAME,
    org_code=ORG_CODE,
    org_type=ORG_TYPE,
    date=ACH_DATE,
    measure = Measure,
    value=Value)) %>%
  mutate(submeasure = "")

measure_fact <- bind_rows(measure_fact, young_data)

# #Cognitive impairment
# cog_imp <- read_csv(
#   here("data", "measures", "pcdem_sicbl-cog-imp-mar-2026.csv"),
#   col_select = c(
#     name = NAME,
#     org_code = ORG_CODE,
#     org_type = ORG_TYPE,
#     date = ACH_DATE,
#     measure = Measure,
#     value = Value
#   )
# ) %>%
#   mutate(
#     value = as.numeric(na_if(value, ".")),
#     submeasure = ""
#   )
# 
# measure_fact <- bind_rows(measure_fact, cog_imp)

##Adding some new measures that came in a spreadsheet with different format
#Load file
ass_plans <- read_csv(
  here("data", "measures", "pcdem-prac-ass-plans-mar-2026.csv"))

grouped_data <- lapply(
  org_config,
  function(x) {
    group_to_level(
      ass_plans,
      x$levels
    )
  }
)

wide_data <- Map(
  function(data, config) {
    make_wide(
      data,
      config$type,
      config$code,
      config$name
    )
  },
  grouped_data,
  org_config
)

all_orgs <- wide_data %>%
  lapply(\(x) janitor::clean_names(x)) %>%
  bind_rows()%>%
  mutate(submeasure = "")
measure_fact <- bind_rows(
  janitor::clean_names(measure_fact),
  all_orgs
)


## Add ethnicity measures
ethnicity <- read_csv(
  here("data", "measures", "pcdem-sicbl-ethnicity-mar-2026.csv"))


grouped_data <- lapply(
  org_config,
  function(x) {
    group_to_level(
      ethnicity,
      x$levels
    )
  }
)

wide_data <- Map(
  function(data, config) {
    make_wide(
      data,
      config$type,
      config$code,
      config$name
    )
  },
  grouped_data,
  org_config
)

all_orgs <- wide_data %>%
  lapply(\(x) janitor::clean_names(x)) %>%
  bind_rows() %>%
  mutate(submeasure = measure) %>%
  mutate(measure = "ETHNICITY")


measure_fact <- bind_rows(
  janitor::clean_names(measure_fact),
  janitor::clean_names(all_orgs)
)

##Add dementia type submeasures
dem_type <- read_csv(
  here("data", "measures", "pcdem-sicbl-dem-type-mar-2026.csv"))


grouped_data <- lapply(
  org_config,
  function(x) {
    group_to_level(
      dem_type,
      x$levels
    )
  }
)

wide_data <- Map(
  function(data, config) {
    make_wide(
      data,
      config$type,
      config$code,
      config$name
    )
  },
  grouped_data,
  org_config
)

all_orgs <- wide_data %>%
  lapply(\(x) janitor::clean_names(x)) %>%
  bind_rows() %>%
  mutate(submeasure = measure) %>%
  mutate(measure = "DEMENTIA_TYPE")


measure_fact <- bind_rows(
  janitor::clean_names(measure_fact),
  janitor::clean_names(all_orgs)
)

## Residential Type
res_type <- read_csv(
  here("data", "measures", "pcdem-sicbl-res-type-mar-2026.csv"))


grouped_data <- lapply(
  org_config,
  function(x) {
    group_to_level(
      res_type,
      x$levels
    )
  }
)

wide_data <- Map(
  function(data, config) {
    make_wide(
      data,
      config$type,
      config$code,
      config$name
    )
  },
  grouped_data,
  org_config
)

all_orgs <- wide_data %>%
  lapply(\(x) janitor::clean_names(x)) %>%
  bind_rows() %>%
  mutate(submeasure = measure) %>%
  mutate(measure = "RES_TYPE")


measure_fact <- bind_rows(
  janitor::clean_names(measure_fact),
  janitor::clean_names(all_orgs)
)

##Add age
age_data <- read_csv(
  here("data", "measures", "pcdem-sicbl-age-sex-mar-2026.csv")) %>%
  filter(str_starts(Measure, "ALL_AGED_"))

grouped_data <- lapply(
  org_config,
  function(x) {
    group_to_level(
      age_data,
      x$levels
    )
  }
)

wide_data <- Map(
  function(data, config) {
    make_wide(
      data,
      config$type,
      config$code,
      config$name
    )
  },
  grouped_data,
  org_config
)

all_orgs <- wide_data %>%
  lapply(\(x) janitor::clean_names(x)) %>%
  bind_rows() %>%
  mutate(submeasure = measure) %>%
  mutate(measure = "AGE")


measure_fact <- bind_rows(
  janitor::clean_names(measure_fact),
  janitor::clean_names(all_orgs)
)

##Add sex
sex_data <- read_csv(
  here("data", "measures", "pcdem-sicbl-age-sex-mar-2026.csv")) %>%
  filter(str_detect(Measure, "^(FEMALE|MALE)_AGED_")) %>%
  mutate(Measure = case_when(
    str_starts(Measure, "FEMALE_") ~ "FEMALE",
    str_starts(Measure, "MALE_") ~ "MALE"
  )) %>% group_by(
    across(-Value)
  ) %>%
  summarise(
    Value=sum(Value,na.rm=TRUE),
    .groups = "drop"
  )

grouped_data <- lapply(
  org_config,
  function(x) {
    group_to_level(
      sex_data,
      x$levels
    )
  }
)

wide_data <- Map(
  function(data, config) {
    make_wide(
      data,
      config$type,
      config$code,
      config$name
    )
  },
  grouped_data,
  org_config
)

all_orgs <- wide_data %>%
  lapply(\(x) janitor::clean_names(x)) %>%
  bind_rows()  %>%
  mutate(submeasure = measure) %>%
  mutate(measure = "SEX")


measure_fact <- bind_rows(
  janitor::clean_names(measure_fact),
  janitor::clean_names(all_orgs)
)



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
  here("data", "database_files", "geography_dim.rds")
)

