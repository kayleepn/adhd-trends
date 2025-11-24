# Load packages
library(readxl)
library(tidyverse)
library(janitor)
library(here)
library(httr)


# Create url list for icd10 data
url_start <- "https://files.digital.nhs.uk/"

# Compared to opencodecounts, I've skipped 2 fewer rows so I can select using column names later, and selected my columns at a later point
icd10_code_usage_urls <- list(
  "fy24to25" = list(
    url = paste0(
      url_start,
      "CC/EA025D/hosp-epis-stat-admi-diag-2024-25-tab.xlsx"
    ),
    sheet = 6,
    skip_rows = 10
  )
)

# Function to download and read the xlsx files - same as opencodecounts
read_icd10_usage_xlsx_from_url <- function(url_list, ...) {
  temp_file <- tempfile(fileext = ".xlsx")
  GET(
    url_list$url,
    write_disk(temp_file, overwrite = TRUE)
  )
  readxl::read_xlsx(
    temp_file,
    col_names = TRUE,
    .name_repair = janitor::make_clean_names,
    sheet = url_list$sheet,
    skip = url_list$skip,
    ...
  )
}


# Function to select the correct columns and remove empty row after headers. Removed the bit where I turn stuff into integers but will add that in later.
select_all_diag_breakdowns <- function(data, url_list) {
  dplyr::select(
    data,
    icd10_code = 1,
    description = 2,
    c("main_diagnosis", "all_diagnoses"),
    c("male", "female", "gender_unknown"),
    starts_with("age")
  ) |>
    remove_empty("rows") |>
    # Fix janitor turning "age 90+" into age_90
    rename(
      age_over_90 = age_90
    )
}

# Combine both functions - same as opencodecounts
get_icd10_data <- function(url_list, ...) {
  df_temp <- read_icd10_usage_xlsx_from_url(url_list, ...)
  select_all_diag_breakdowns(df_temp, url_list)
}

icd10_usage <- icd10_code_usage_urls |>
  map(get_icd10_data) |>
  bind_rows(.id = "nhs_fy") |>
  separate(nhs_fy, c("start_date", "end_date"), "to") |>
  mutate(
    start_date = as.Date(
      paste0("20", str_extract_all(start_date, "\\d+"), "-04-01")
    ),
    end_date = as.Date(
      paste0("20", str_extract_all(end_date, "\\d+"), "-03-31")
    ),
    icd10_code = gsub("\\s?[^[:alnum:]]+\\s?", "", icd10_code)
  )

# Pivot main/all diagnosis, age, and sex breakdowns into tidy form. Turn usage column into integers.
tidy_icd_10_usage <- icd10_usage |>
  pivot_longer(
    cols = main_diagnosis:age_over_90,
    names_to = "breakdown",
    values_to = "usage"
  ) |>
  dplyr::mutate(
    usage = as.integer(usage)
  )

# Filter for F90 codes only
f90_icd_10_breakdowns <- tidy_icd_10_usage |>
  filter(str_detect(icd10_code, "^F90"))

write_csv(
  f90_icd_10_breakdowns,
  here("data", "temp_icd10_breakdowns", "df_all_2425_icd10_f90_breakdowns.csv")
)
