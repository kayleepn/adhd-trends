# Load packages
library(tidyverse)
library(janitor)
library(here)
library(httr)

# Get urls and grab range for first 5 rows with actual content
url_start <- "https://files.digital.nhs.uk/"

icd10_5rows_urls <- list(
  "fy24to25" = list(
    url = paste0(
      url_start,
      "CC/EA025D/hosp-epis-stat-admi-diag-2024-25-tab.xlsx"
    ),
    sheet = 6,
    range = "A11:AK15"
  ),
  "fy23to24" = list(
    url = paste0(
      url_start,
      "A5/5B8474/hosp-epis-stat-admi-diag-2023-24-tab.xlsx"
    ),
    sheet = 6,
    range = "A11:AK15"
  ),
  "fy22to23" = list(
    url = paste0(
      url_start,
      "7A/DB1B00/hosp-epis-stat-admi-diag-2022-23-tab_V2.xlsx"
    ),
    sheet = 6,
    range = "A11:AK15"
  ),
  "fy21to22" = list(
    url = paste0(
      url_start,
      "0E/E70963/hosp-epis-stat-admi-diag-2021-22-tab.xlsx"
    ),
    sheet = 6,
    range = "A11:AK15"
  ),
  "fy20to21" = list(
    url = paste0(
      url_start,
      "5B/AD892C/hosp-epis-stat-admi-diag-2020-21-tab.xlsx"
    ),
    sheet = 6,
    range = "A11:AK15"
  ),
  "fy19to20" = list(
    url = paste0(
      url_start,
      "37/8D9781/hosp-epis-stat-admi-diag-2019-20-tab%20supp.xlsx"
    ),
    sheet = 6,
    range = "A11:AK15"
  ),
  "fy18to19" = list(
    url = paste0(
      url_start,
      "1C/B2AD9B/hosp-epis-stat-admi-diag-2018-19-tab.xlsx"
    ),
    sheet = 6,
    range = "A11:AK15"
  ),
  "fy17to18" = list(
    url = paste0(
      url_start,
      "B2/5CEC8D/hosp-epis-stat-admi-diag-2017-18-tab.xlsx"
    ),
    sheet = 6,
    range = "A11:AK15"
  ),
  "fy16to17" = list(
    url = paste0(
      url_start,
      "publication/7/d/hosp-epis-stat-admi-diag-2016-17-tab.xlsx"
    ),
    sheet = 6,
    range = "A11:AK15"
  ),
  "fy15to16" = list(
    url = paste0(
      url_start,
      "publicationimport/pub22xxx/pub22378/hosp-epis-stat-admi-diag-2015-16-tab.xlsx"
    ),
    sheet = 6,
    range = "A11:AK15"
  ),
  "fy14to15" = list(
    url = paste0(
      url_start,
      "publicationimport/pub19xxx/pub19124/hosp-epis-stat-admi-diag-2014-15-tab.xlsx"
    ),
    sheet = 6,
    range = "A11:AK15"
  ),
  "fy13to14" = list(
    url = paste0(
      url_start,
      "publicationimport/pub16xxx/pub16719/hosp-epis-stat-admi-diag-2013-14-tab.xlsx"
    ),
    sheet = 6,
    range = "A17:AF21"
  ),
  "fy12to13" = list(
    url = paste0(
      url_start,
      "publicationimport/pub12xxx/pub12566/hosp-epis-stat-admi-diag-2012-13-tab.xlsx"
    ),
    sheet = 6,
    range = "A18:AF22"
  )
)

# Function to download and read the xlsx files
read_icd10_5rows_xlsx_from_url <- function(url_list, ...) {
  temp_file <- tempfile(fileext = ".xlsx")
  GET(
    url_list$url,
    write_disk(temp_file, overwrite = TRUE)
  )
  readxl::read_xlsx(
    temp_file,
    col_names = FALSE,
    .name_repair = janitor::make_clean_names,
    sheet = url_list$sheet,
    range = url_list$range,
    ...
  )
}

# Probably not necessary but wasn't sure if this does something important
get_icd10_5rows <- function(url_list, ...) {
  df_temp <- read_icd10_5rows_xlsx_from_url(url_list, ...)
}

# Get 5 rows of ICD-10 usage data for each FY
icd10_5rows <- icd10_5rows_urls |>
  map(get_icd10_5rows) |>
  bind_rows(.id = "nhs_fy") |>
  mutate(row_number = rep(1:5, length.out = n())) |>
  relocate(row_number, .before = 2)

write_csv(
  icd10_5rows,
  here("data", "temp_icd10_breakdowns", "df_icd10_5rows_breakdowns.csv")
)

# Get column names for each FY, ordered by column number as readxl reads them in
icd10_col_names <- icd10_5rows |>
  filter(row_number == 1) |>
  pivot_longer(
    cols = x:x_37,
    names_to = "col_number",
    values_to = "col_header"
  ) |>
  pivot_wider(
    id_cols = col_number,
    names_from = nhs_fy,
    values_from = col_header
  )

write_csv(
  icd10_col_names,
  here("data", "temp_icd10_breakdowns", "df_icd10_col_names_breakdown.csv")
)

# Although we can't see this here, the dataframe with 5 rows show that code descriptions are always stored in the 2nd column (x_2).
