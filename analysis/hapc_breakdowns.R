# Load packages
library(janitor)
library(tidyverse)
library(here)

# Read diagnosis data for 2024-25
url_start <- "https://files.digital.nhs.uk/"
url_202425 <- "29/A12D21/hosp-epis-stat-admi-diag-2024-25.csv"

df_2425_icd10_breakdowns <- read_csv(
  paste0(url_start, url_202425),
  name_repair = janitor::make_clean_names
)

# Define measures we want to select
gender_measures <- c(
  "FCE_Male_Sum",
  "FCE_MALE_SUM",
  "FCE_Female_Sum",
  "FCE_FEMALE_SUM",
  "FCE_UNKNOWN_GENDER_Sum",
  "FCE_UNKNOWN_GENDER_SUM",
  "Male",
  "Female",
  "Unknown"
)

age_measures <- c(
  "Age_0_Sum",
  "AGE_0_SUM",
  "Age_1_4_Sum",
  "AGE_1_4_SUM",
  "Age_5_9_Sum",
  "AGE_5_9_SUM",
  "Age_10_14_Sum",
  "AGE_10_14_SUM",
  "Age_15_Sum",
  "AGE_15_SUM",
  "Age_16_Sum",
  "AGE_16_SUM",
  "Age_17_Sum",
  "AGE_17_SUM",
  "Age_18_Sum",
  "AGE_18_SUM",
  "Age_19_Sum",
  "AGE_19_SUM",
  "Age_20_24_Sum",
  "AGE_20_24_SUM",
  "Age_25_29_Sum",
  "AGE_25_29_SUM",
  "Age_30_34_Sum",
  "AGE_30_34_SUM",
  "Age_35_39_Sum",
  "AGE_35_39_SUM",
  "Age_40_44_Sum",
  "AGE_40_44_SUM",
  "Age_45_49_Sum",
  "AGE_45_49_SUM",
  "Age_50_54_Sum",
  "AGE_50_54_SUM",
  "Age_55_59_Sum",
  "AGE_55_59_SUM",
  "Age_60_64_Sum",
  "AGE_60_64_SUM",
  "Age_65_69_Sum",
  "AGE_65_69_SUM",
  "Age_70_74_Sum",
  "AGE_70_74_SUM",
  "Age_75_79_Sum",
  "AGE_75_79_SUM",
  "Age_80_84_Sum",
  "AGE_80_84_SUM",
  "Age_85_89_Sum",
  "AGE_85_89_SUM",
  "Age_90_120_Sum",
  "AGE_90_120_SUM",
  "0",
  "1_4",
  "5_9",
  "10_14",
  "15",
  "16",
  "17",
  "18",
  "19",
  "20_24",
  "25_29",
  "30_34",
  "35_39",
  "40_44",
  "45_49",
  "50_54",
  "55_59",
  "60_64",
  "65_69",
  "70_74",
  "75_79",
  "80_84",
  "85_89",
  "90+"
)

# Filter data to (1) only include all measures defined above, and (2) all codes starting with "F90".
df_2425_icd10_f90_breakdowns <- df_2425_icd10_breakdowns |>
  filter(attribute %in% c(gender_measures, age_measures)) |>
  filter(str_detect(code, "^F90"))

# Write data
write_csv(
  df_2425_icd10_f90_breakdowns,
  here("data", "temp_icd10_breakdowns", "df_2425_icd10_f90_breakdowns.csv")
)

# Repeat for 2023-24
url_202324 <- "7B/BCBF63/hosp-epis-stat-admi-diag-2023-24.csv"

df_2324_icd10_breakdowns <- read_csv(
  paste0(url_start, url_202324),
  name_repair = janitor::make_clean_names
)

# Change 'attribute' to 'measure' to match column names
df_2324_icd10_f90_breakdowns <- df_2324_icd10_breakdowns |>
  filter(measure %in% c(gender_measures, age_measures)) |>
  filter(str_detect(code, "^F90"))

write_csv(
  df_2324_icd10_f90_breakdowns,
  here("data", "temp_icd10_breakdowns", "df_2324_icd10_f90_breakdowns.csv")
)

#Repeat for 2022-23
url_202223 <- "C1/BF31FC/hosp-epis-stat-admi-diag-2022-23-data.csv"

df_2223_icd10_breakdowns <- read_csv(
  paste0(url_start, url_202223),
  name_repair = janitor::make_clean_names
)

df_2223_icd10_f90_breakdowns <- df_2223_icd10_breakdowns |>
  filter(measure %in% c(gender_measures, age_measures)) |>
  filter(str_detect(code, "^F90"))

write_csv(
  df_2223_icd10_f90_breakdowns,
  here("data", "temp_icd10_breakdowns", "df_2223_icd10_f90_breakdowns.csv")
)

# 2014-15 to 2021-22: data not available as csv

# Repeat for 2013-14
url_201314 <- "61/E07E65/hes_apc_national_diagnosis_2013_14.csv"

df_1314_icd10_breakdowns <- read_csv(
  paste0(url_start, url_201314),
  name_repair = janitor::make_clean_names
)

df_1314_icd10_f90_breakdowns <- df_1314_icd10_breakdowns |>
  filter(measure_sub_category %in% c(gender_measures, age_measures)) |>
  filter(str_detect(dimension_code, "^F90"))

write_csv(
  df_1314_icd10_f90_breakdowns,
  here("data", "temp_icd10_breakdowns", "df_1314_icd10_f90_breakdowns.csv")
)

# Repeat for 2012-13
url_201213 <- "77/E81C5C/hes_apc_national_diagnosis_2012_13.csv"

df_1213_icd10_breakdowns <- read_csv(
  paste0(url_start, url_201213),
  name_repair = janitor::make_clean_names
)

df_1213_icd10_f90_breakdowns <- df_1213_icd10_breakdowns |>
  filter(measure_sub_category %in% c(gender_measures, age_measures)) |>
  filter(str_detect(dimension_code, "^F90"))

write_csv(
  df_1213_icd10_f90_breakdowns,
  here("data", "temp_icd10_breakdowns", "df_1213_icd10_f90_breakdowns.csv")
)
