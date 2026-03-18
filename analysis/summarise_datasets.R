library(tidyverse)
library(arrow)
library(here)
library(opencodecounts)

# Read OpenPrescribing Hospital data
df_scmd <- read_csv(
  here("output", "openprescribing-hospitals", "adhd_scmd_dose.csv")
)

# Read OpenPrescribing data
df_op <- read_parquet(
  here("output", "openprescribing", "adhd_gp_prescribing_bnf.parquet")
)

# Summarise temporal coverage of a date column in a data frame.
# Returns a <date_summary> object with range, unique count, NAs, and gaps
# against the expected sequence (day / week / month / year).
summarise_dates <- function(df, date_col, unit = "month") {
  d <- as.Date(pull(df, {{ date_col }}))
  expected <- seq(min(d, na.rm = TRUE), max(d, na.rm = TRUE), by = unit)
  gaps <- as.Date(setdiff(expected, unique(na.omit(d))), origin = "1970-01-01")

  structure(
    list(
      min = min(d, na.rm = TRUE),
      max = max(d, na.rm = TRUE),
      n_unique = n_distinct(na.omit(d)),
      n_na = sum(is.na(d)),
      n_expected = length(expected),
      gaps = gaps
    ),
    class = "date_summary"
  )
}

# Custom print method for <date_summary> to give a readable overview when
# checking date coverage across datasets with different date columns and cadences.
print.date_summary <- function(x, ...) {
  pct <- round(100 * x$n_unique / x$n_expected, 1)
  cat(sprintf("Range:  %s -- %s\n", x$min, x$max))
  cat(sprintf(
    "Unique: %d / %d expected (%s%%)\n",
    x$n_unique,
    x$n_expected,
    pct
  ))
  cat(sprintf("NAs:    %d\n", x$n_na))
  if (length(x$gaps)) {
    cat(sprintf(
      "Gaps     : %s\n",
      paste(format(x$gaps, "%b %Y"), collapse = ", ")
    ))
  }
  invisible(x)
}

# Check OpenPrescribing data
summarise_dates(df_scmd, year_month, unit = "month")
summarise_dates(df_op, month, unit = "month")

# Check opencodecounts data
summarise_dates(snomed_usage, end_date, unit = "year")
summarise_dates(icd10_usage, end_date, unit = "year")
