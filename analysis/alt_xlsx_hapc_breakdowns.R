# Load packages
library(readxl)
library(tidyverse)
library(janitor)
library(here)
library(httr)

# Alternative approach: defining what's in every single column so that functions still work if column names are very different. Since I don't need to read in column names, the ranges are slightly different too. However the script is now much longer.

# Create url list for icd10 data
url_start <- "https://files.digital.nhs.uk/"


alt_icd10_code_usage_urls <- list(
  "fy24to25" = list(
    url = paste0(
      url_start,
      "CC/EA025D/hosp-epis-stat-admi-diag-2024-25-tab.xlsx"
    ),
    sheet = 6,
    range = "A13:AK11359",
    icd10_code_col = 1,
    description_col = 2,
    all_col = 8,
    main_col = 9,
    male_col = 10,
    female_col = 11,
    gender_unknown_col = 12,
    age_0_col = 14,
    age_1_4_col = 15,
    age_5_9_col = 16,
    age_10_14_col = 17,
    age_15_col = 18,
    age_16_col = 19,
    age_17_col = 20,
    age_18_col = 21,
    age_19_col = 22,
    age_20_24_col = 23,
    age_25_29_col = 24,
    age_30_34_col = 25,
    age_35_39_col = 26,
    age_40_44_col = 27,
    age_45_49_col = 28,
    age_50_54_col = 29,
    age_55_59_col = 30,
    age_60_64_col = 31,
    age_65_69_col = 32,
    age_70_74_col = 33,
    age_75_79_col = 34,
    age_80_84_col = 35,
    age_85_89_col = 36,
    age_90plus_col = 37
  ),
  "fy19to20" = list(
    url = paste0(
      url_start,
      "37/8D9781/hosp-epis-stat-admi-diag-2019-20-tab%20supp.xlsx"
    ),
    sheet = 6,
    range = "A12:AK11390",
    icd10_code_col = 1,
    description_col = 2,
    all_col = 8,
    main_col = 9,
    male_col = 10,
    female_col = 11,
    gender_unknown_col = 12,
    age_0_col = 14,
    age_1_4_col = 15,
    age_5_9_col = 16,
    age_10_14_col = 17,
    age_15_col = 18,
    age_16_col = 19,
    age_17_col = 20,
    age_18_col = 21,
    age_19_col = 22,
    age_20_24_col = 23,
    age_25_29_col = 24,
    age_30_34_col = 25,
    age_35_39_col = 26,
    age_40_44_col = 27,
    age_45_49_col = 28,
    age_50_54_col = 29,
    age_55_59_col = 30,
    age_60_64_col = 31,
    age_65_69_col = 32,
    age_70_74_col = 33,
    age_75_79_col = 34,
    age_80_84_col = 35,
    age_85_89_col = 36,
    age_90plus_col = 37
  ),
  "fy12to13" = list(
    url = paste0(
      url_start,
      "publicationimport/pub12xxx/pub12566/hosp-epis-stat-admi-diag-2012-13-tab.xlsx"
    ),
    sheet = 6,
    range = "A20:AF11400",
    icd10_code_col = 1,
    description_col = 2,
    all_col = 3,
    main_col = 4,
    male_col = 5,
    female_col = 6,
    gender_unknown_col = 7,
    age_0_col = 9,
    age_1_4_col = 10,
    age_5_9_col = 11,
    age_10_14_col = 12,
    age_15_col = 13,
    age_16_col = 14,
    age_17_col = 15,
    age_18_col = 16,
    age_19_col = 17,
    age_20_24_col = 18,
    age_25_29_col = 19,
    age_30_34_col = 20,
    age_35_39_col = 21,
    age_40_44_col = 22,
    age_45_49_col = 23,
    age_50_54_col = 24,
    age_55_59_col = 25,
    age_60_64_col = 26,
    age_65_69_col = 27,
    age_70_74_col = 28,
    age_75_79_col = 29,
    age_80_84_col = 30,
    age_85_89_col = 31,
    age_90plus_col = 32
  )
)

# Function to download and read the xlsx files - same as the method in xlsx_hapc_breakdowns but col_names = FALSE
alt_read_icd10_usage_xlsx_from_url <- function(url_list, ...) {
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

# Function to select the correct columns
alt_select_all_diag_breakdowns <- function(data, url_list) {
  dplyr::select(
    data,
    icd10_code = url_list$icd10_code_col,
    description = url_list$description_col,
    all_diagnoses = url_list$all_col,
    main_diagnosis = url_list$main_col,
    male = url_list$male_col,
    female = url_list$female_col,
    gender_unknown = url_list$gender_unknown_col,
    age_0 = url_list$age_0_col,
    age_1_4 = url_list$age_1_4_col,
    age_5_9 = url_list$age_5_9_col,
    age_10_14 = url_list$age_10_14_col,
    age_15 = url_list$age_15_col,
    age_16 = url_list$age_16_col,
    age_17 = url_list$age_17_col,
    age_18 = url_list$age_18_col,
    age_19 = url_list$age_19_col,
    age_20_24 = url_list$age_20_24_col,
    age_25_29 = url_list$age_25_29_col,
    age_30_34 = url_list$age_30_34_col,
    age_35_39 = url_list$age_35_39_col,
    age_40_44 = url_list$age_40_44_col,
    age_45_49 = url_list$age_45_49_col,
    age_50_54 = url_list$age_50_54_col,
    age_55_59 = url_list$age_55_59_col,
    age_60_64 = url_list$age_60_64_col,
    age_65_69 = url_list$age_65_69_col,
    age_70_74 = url_list$age_70_74_col,
    age_75_79 = url_list$age_75_79_col,
    age_80_84 = url_list$age_80_84_col,
    age_85_89 = url_list$age_85_89_col,
    age_90plus = url_list$age_90plus_col
  )
}

# Combine both functions - same as opencodecounts
alt_get_icd10_data <- function(url_list, ...) {
  df_temp <- alt_read_icd10_usage_xlsx_from_url(url_list, ...)
  alt_select_all_diag_breakdowns(df_temp, url_list)
}

alt_icd10_usage <- alt_icd10_code_usage_urls |>
  map(alt_get_icd10_data) |>
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
alt_tidy_icd_10_usage <- alt_icd10_usage |>
  pivot_longer(
    cols = all_diagnoses:age_90plus,
    names_to = "breakdown",
    values_to = "usage"
  ) |>
  dplyr::mutate(
    usage = as.integer(usage)
  )

# Filter for F90 codes only
alt_f90_icd_10_breakdowns <- alt_tidy_icd_10_usage |>
  filter(str_detect(icd10_code, "^F90"))

write_csv(
  alt_f90_icd_10_breakdowns,
  here("data", "temp_icd10_breakdowns", "alt_df_icd10_f90_breakdowns.csv")
)
