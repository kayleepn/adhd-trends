connect_openprescribing <- function(dataset = "hscic") {
  # Get BigQuery credentials path from environment variable
  credentials_path <- Sys.getenv("OP_CREDENTIALS")

  # Authorise BigQuery
  bigrquery::bq_auth(path = credentials_path)

  # Connect to BigQuery
  DBI::dbConnect(
    bigrquery::bigquery(),
    project = "ebmdatalab",
    dataset = dataset,
    billing = "ebmdatalab"
  )
}


get_practices <- function(
  con,
  add_setting_labels = FALSE,
  filter_setting = NULL,
  add_status_code_labels = FALSE,
  filter_status_code = NULL
) {
  practices_query <- dplyr::tbl(con, "practices")

  # Add status code description labels
  if (add_status_code_labels) {
    practices_query <- practices_query |>
      dplyr::mutate(
        status_code_label = dplyr::case_when(
          status_code == "A" ~ "Active",
          status_code == "B" ~ "Retired",
          status_code == "C" ~ "Closed",
          status_code == "D" ~ "Dormant",
          status_code == "P" ~ "Proposed",
          status_code == "U" ~ "Unknown",
          .default = "Unknown"
        )
      )
  }

  # Add setting description labels
  if (add_setting_labels) {
    practices_query <- practices_query |>
      dplyr::mutate(
        setting_label = dplyr::case_when(
          setting == 0 ~ "Other",
          setting == 1 ~ "WIC Practice",
          setting == 2 ~ "OOH Practice",
          setting == 3 ~ "WIC + OOH Practice",
          setting == 4 ~ "GP Practice",
          setting == 8 ~ "Public Health Service",
          setting == 9 ~ "Community Health Service",
          setting == 10 ~ "Hospital Service",
          setting == 11 ~ "Optometry Service",
          setting == 12 ~ "Urgent & Emergency Care",
          setting == 13 ~ "Hospice",
          setting == 14 ~ "Care Home / Nursing Home",
          setting == 15 ~ "Border Force",
          setting == 16 ~ "Young Offender Institution",
          setting == 17 ~ "Secure Training Centre",
          setting == 18 ~ "Secure Childrens Home",
          setting == 19 ~ "Immigration Removal Centre",
          setting == 20 ~ "Court",
          setting == 21 ~ "Police Custody",
          setting == 22 ~ "Sexual Assault Referral Centre (SARC)",
          setting == 24 ~ "Other - Justice Estate",
          setting == 25 ~ "Prison",
          setting == 26 ~ "Primary Care Network",
          setting == 27 ~ "Independent Pharmacy Prescriber Pathfinder",
          .default = "Unknown"
        )
      )
  }

  # Filter by setting codes
  if (!is.null(filter_setting)) {
    practices_query <- practices_query |>
      dplyr::filter(setting %in% filter_setting)
  }

  # Filter by setting codes
  if (!is.null(filter_status_code)) {
    practices_query <- practices_query |>
      dplyr::filter(status_code %in% filter_status_code)
  }

  practices_query
}

get_normalised_prescribing <- function(
  con,
  bnf_codes = NULL,
  start_date = NULL,
  end_date = NULL
) {
  np_query <- dplyr::tbl(con, "normalised_prescribing")

  # Filter by BNF codes if provided
  if (!is.null(bnf_codes)) {
    np_query <- np_query |>
      dplyr::filter(bnf_code %in% !!bnf_codes)
  }

  # Filter by start date if provided
  if (!is.null(start_date)) {
    np_query <- np_query |>
      dplyr::filter(month >= !!start_date)
  }

  # Filter by end date if provided
  if (!is.null(end_date)) {
    np_query <- np_query |>
      dplyr::filter(month <= !!end_date)
  }

  np_query
}


get_gp_prescribing <- function(con, bnf_codes, start_date, end_date) {
  query_practices <- get_practices(con, filter_setting = 4)

  query_prescribing <- get_normalised_prescribing(
    con,
    bnf_codes = bnf_codes,
    start_date = start_date,
    end_date = end_date
  )

  query_prescribing |>
    inner_join(query_practices, by = c("practice" = "code"))
}
