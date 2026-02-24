# OpenPrescribing Hospital (SCMD) functions
# Functions for working with Secondary Care Medicines Data via BigQuery

connect_scmd <- function(dataset = "scmd_pipeline") {
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

get_scmd_prescribing <- function(
  con,
  table_name = c(
    "scmd_processed",
    "dose",
    "ingredient_quantity",
    "ddd_quantity"
  ),
  vmp_codes
) {

  table_name <- match.arg(table_name)

  dplyr::tbl(con, table_name) |>
    dplyr::filter(vmp_code %in% vmp_codes)
}

get_scmd_trusts <- function(con) {
  dplyr::tbl(con, "ods")
}
