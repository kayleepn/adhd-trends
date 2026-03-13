# Downloads ADHD prescribing data from the NHSBSA FOI disclosure log
# https://opendata.nhsbsa.net/theme/freedom-of-information-disclosure-log

library(tidyverse)
library(janitor)
library(here)

url_start <- "https://opendata.nhsbsa.net/dataset/"

# Each entry is one FOI request. 'skip' removes leading non-data rows, and 'n_max'
# limits rows to exclude a trailing total row. Note: skip counts against n_max,
# so n_max must account for skipped rows. Skipping empty rows is not an option
# here as the total row is non-empty and should not be loaded here.
foi_adhd_rx_urls <- list(
  "foi02082" = list(
    url = paste0(
      url_start,
      "bb2a890c-aefc-47dc-98a4-9e3f8dad884d/resource/2608dfdc-e8df-4d99-ad86-3f35f9aed27c/download/foi-02082-data.csv"
    ),
    skip = 1,
    n_max = 73866
  ),
  "foi02360" = list(
    url = paste0(
      url_start,
      "971a7ca0-11a9-41cb-a556-4bc8d06515b1/resource/3dcbcda6-7368-467f-bc27-df332791ae06/download/foi-02360-completed-request.csv"
    ),
    skip = 1,
    n_max = 5111
  ),
  "foi02722" = list(
    url = paste0(
      url_start,
      "47c34fce-9199-43b2-930e-9bf34a27d55c/resource/efe89a92-1d25-46ee-8e9e-3349759c34c5/download/foi-02722_data.csv"
    ),
    skip = 1,
    n_max = 10656
  ),
  "foi02847" = list(
    url = paste0(
      url_start,
      "435dee00-ad71-4ac3-a613-e17cea25ef64/resource/463bf0bd-e68d-4dac-b696-ccb5059bb4c1/download/foi-02847-compeleted-request.csv"
    ),
    skip = 0,
    n_max = 3699
  ),
  "foi03492" = list(
    url = paste0(
      url_start,
      "9066d0bc-3f32-44b8-ad8a-b57ce073caa1/resource/42ce67ae-50ea-44ed-8cf2-4cde9bece9cd/download/foi_03492.csv"
    ),
    skip = 0,
    n_max = 11824
  )
)

# Read raw CSV from URL
read_foi_rx_data <- function(url_list, ...) {
  read_csv(
    url_list$url,
    col_names = TRUE,
    skip = url_list$skip,
    n_max = url_list$n_max,
    ...
  )
}

# Select and rename columns by position, parse month, and clean BNF chemical name
clean_foi_rx_data <- function(data) {
  data |>
    dplyr::select(
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
      month = as.Date(paste0(
        str_extract(year_month, "\\d{4}"),
        "-",
        str_extract(year_month, "\\d{2}$"),
        "-01"
      )),
      items = as.integer(items),
      bnf_chemical_name = str_extract(bnf_chemical_name, "\\w+")
    ) |>
    dplyr::select(-year_month) |>
    dplyr::relocate(month, .before = icb_code)
}

# Read and clean a single FOI request
get_foi_rx_data <- function(url_list, ...) {
  read_foi_rx_data(url_list, ...) |>
    clean_foi_rx_data()
}

# ADHD medications to include
adhd_bnf_chemical_names <- c(
  "Methylphenidate",
  "Lisdexamfetamine",
  "Dexamfetamine",
  "Atomoxetine",
  "Guanfacine"
)

# Download, combine, and filter all FOI requests
df_adhd_foi_rx <- foi_adhd_rx_urls |>
  map(get_foi_rx_data) |>
  bind_rows(.id = "foi_id") |>
  filter(between(month, as.Date("2015-08-01"), as.Date("2025-07-01"))) |>
  filter(bnf_chemical_name %in% adhd_bnf_chemical_names)

# Check expected number of months (120 = Aug 2015 to Jul 2025)
n_distinct(df_adhd_foi_rx$month)
# [1] 120

# Check no months appear in more than one FOI request
df_adhd_foi_rx |>
  distinct(foi_id, month) |>
  count(month) |>
  filter(n > 1)
# should return 0 rows

# Check all 5 BNF chemical names are present
unique(df_adhd_foi_rx$bnf_chemical_name)
# [1] "Methylphenidate"  "Dexamfetamine"    "Lisdexamfetamine"
# [4] "Atomoxetine"      "Guanfacine"

# Check no NAs in key columns
df_adhd_foi_rx |>
  summarise(across(c(month, bnf_chemical_name, items), ~ sum(is.na(.x))))

# Check BNF presentation names
unique(df_adhd_foi_rx$bnf_presentation)
# 158 unique presentations

write_csv(
  df_adhd_foi_rx,
  here("output", "foi_prescribing", "adhd_foi_prescribing.csv")
)
