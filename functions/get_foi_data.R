# This script downloads all available FOI ADHD prescribing data from the NHSBSA FOI disclosure log https://opendata.nhsbsa.net/theme/freedom-of-information-disclosure-log
# and combines them into one file

library(tidyverse)
library(janitor)
library(here)
library(httr)

url_start <- "https://opendata.nhsbsa.net/dataset/"

# use skip and n_max to remove irrelevant rows at top and bottom of csv file; note skip reduces n_max by 1

foi_adhd_rx_urls <- list(
  "01_2015to05_2024" = list(
    url = paste0(
      url_start,
      "bb2a890c-aefc-47dc-98a4-9e3f8dad884d/resource/2608dfdc-e8df-4d99-ad86-3f35f9aed27c/download/foi-02082-data.csv"
    ),
    skip = 1,
    n_max = 73866
  ),
  "06_2024to08_2024" = list(
    url = paste0(
      url_start,
      "971a7ca0-11a9-41cb-a556-4bc8d06515b1/resource/3dcbcda6-7368-467f-bc27-df332791ae06/download/foi-02360-completed-request.csv"
    ),
    skip = 1,
    n_max = 5111
  ),
  "09_2024to02_2025" = list(
    url = paste0(
      url_start,
      "47c34fce-9199-43b2-930e-9bf34a27d55c/resource/efe89a92-1d25-46ee-8e9e-3349759c34c5/download/foi-02722_data.csv"
    ),
    skip = 1,
    n_max = 10656
  ),
  "03_2025to04_2025" = list(
    url = paste0(
      url_start,
      "435dee00-ad71-4ac3-a613-e17cea25ef64/resource/463bf0bd-e68d-4dac-b696-ccb5059bb4c1/download/foi-02847-compeleted-request.csv"
    ),
    skip = 0,
    n_max = 3699
  ),
  "05_2025to10_2025" = list(
    url = paste0(
      url_start,
      "9066d0bc-3f32-44b8-ad8a-b57ce073caa1/resource/42ce67ae-50ea-44ed-8cf2-4cde9bece9cd/download/foi_03492.csv"
    ),
    skip = 0,
    n_max = 11824
  )
)

# Function to read csv from url, same logic as reading xlsx files for OpenCodeCounts

read_foi_rx_csv_from_url <- function(url_list, ...) {
  temp_file <- tempfile(fileext = ".csv")
  GET(
    url_list$url,
    write_disk(temp_file, overwrite = TRUE)
  )
  read_csv(
    temp_file,
    col_names = TRUE,
    skip = url_list$skip,
    n_max = url_list$n_max,
    ...
  )
}

# Function to select the correct columns
select_foi_rx_cols <- function(data, url_list) {
  dplyr::select(
    data,
    year_month = 1,
    icb_code = 2,
    icb_name = 3,
    bnf_chemical = 4,
    bnf_chemical_name = 5,
    bnf_presentation_code = 6,
    bnf_presentation = 7,
    items = 8
  ) |>
    dplyr::mutate(
      year_month = as.character(year_month),
      items = as.integer(items)
    )
}

# Combine both functions
get_foi_rx_data <- function(url_list, ...) {
  df_temp <- read_foi_rx_csv_from_url(url_list, ...)
  select_foi_rx_cols(df_temp, url_list)
}

# Get FOI prescribing data
df_adhd_foi_rx <- foi_adhd_rx_urls |>
  map(get_foi_rx_data) |>
  bind_rows(.id = NULL) |>
  # Fix date format and filter for correct time period
  # Month recorded as first day of month as with OpenPrescribing
  mutate(
    month = as.Date(
      paste0(
        str_extract_all(year_month, "\\d{4}"),
        "-",
        str_extract_all(year_month, "\\d{2}$"),
        "-01"
      )
    ),
  ) |>
  select(-c(year_month)) |>
  relocate(month, .before = icb_code) |>
  filter(between(month, as.Date("2015-08-01"), as.Date("2025-07-01"))) |>
  # Fix BNF names and remove irrelevant medications
  mutate(bnf_chemical_name = str_extract(bnf_chemical_name, "(\\w+)")) |>
  filter(
    bnf_chemical_name %in%
      c(
        "Methylphenidate",
        "Lisdexamfetamine",
        "Dexamfetamine",
        "Atomoxetine",
        "Guanfacine"
      )
  )

# Check for correct time period
n_distinct(df_adhd_foi_rx$month)
# [1] 120

# Check for correct BNF chemical names
unique(df_adhd_foi_rx$bnf_name)
# [1] "Methylphenidate"  "Dexamfetamine"    "Lisdexamfetamine"
# [4] "Atomoxetine"      "Guanfacine"

# Check BNF presentation names
unique(df_adhd_foi_rx$bnf_presentation)
# 158 unique presentations

# Write data
write_csv(
  df_adhd_foi_rx,
  here("output", "foi_prescribing", "adhd_foi_prescribing.csv")
)
