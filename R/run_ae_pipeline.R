
#' Run an End-to-End SDTM-like AE Pipeline
#'
#' Reads synthetic demographics and adverse event CSV files,
#' generates DM and AE datasets, validates their integrity,
#' and exports reproducible outputs and a validation report.
#'
#' @param ae_input_path Path to the raw AE CSV.
#' @param dm_input_path Path to the raw DM CSV.
#' @param study_id Non-empty study identifier.
#' @param output_dir Directory for output artifacts.
#'
#' @return A list containing DM, AE, validation_report,
#'   and exported file paths.
#'
#' @details
#' Identifiers are imported as character values to preserve
#' leading zeros. Files are exported only after all checks pass.
#' This pipeline does not establish full CDISC compliance.
#'
#' @export
run_ae_pipeline <- function(
    ae_input_path,
    dm_input_path,
    study_id,
    output_dir = "outputs") {

  started_at <- Sys.time()

  # 1. Validate arguments

  for (path in c(ae_input_path, dm_input_path)) {
    if (!is.character(path) ||
        length(path) != 1L ||
        is.na(path) ||
        !file.exists(path) ||
        dir.exists(path)) {
      stop("Input CSV file does not exist.", call. = FALSE)
    }
  }

  if (!is.character(study_id) ||
      length(study_id) != 1L ||
      is.na(study_id) ||
      !nzchar(trimws(study_id))) {
    stop("study_id must be a non-empty string.", call. = FALSE)
  }

  if (!is.character(output_dir) ||
      length(output_dir) != 1L ||
      is.na(output_dir) ||
      !nzchar(trimws(output_dir))) {
    stop("output_dir must be a non-empty string.", call. = FALSE)
  }

  # 2. Read raw datasets

  dm_types <- readr::cols(
    subject_id = readr::col_character(),
    site_id = readr::col_character(),
    age = readr::col_double(),
    sex = readr::col_character(),
    arm = readr::col_character(),
    rfstdtc = readr::col_character(),
    .default = readr::col_character()
  )

  ae_types <- readr::cols(
    subject_id = readr::col_character(),
    site_id = readr::col_character(),
    event_id = readr::col_character(),
    event_term = readr::col_character(),
    severity = readr::col_character(),
    serious = readr::col_character(),
    relationship = readr::col_character(),
    outcome = readr::col_character(),
    start_date = readr::col_character(),
    end_date = readr::col_character(),
    .default = readr::col_character()
  )

  raw_dm <- readr::read_csv(
    dm_input_path,
    col_types = dm_types,
    na = c("", "NA"),
    show_col_types = FALSE,
    progress = FALSE
  )

  raw_ae <- readr::read_csv(
    ae_input_path,
    col_types = ae_types,
    na = c("", "NA"),
    show_col_types = FALSE,
    progress = FALSE
  )

  if (nrow(readr::problems(raw_dm)) > 0L ||
      nrow(readr::problems(raw_ae)) > 0L) {
    stop("Input CSV parsing errors detected.", call. = FALSE)
  }

  if (nrow(raw_dm) == 0L || nrow(raw_ae) == 0L) {
    stop("Input datasets must not be empty.", call. = FALSE)
  }

  # 3. Generate DM

  dm <- generate_dm(
    raw_data = raw_dm,
    study_id = study_id
  )

  validate_dm(dm)

  # 4. Generate AE using DM

  ae <- generate_ae(
    raw_data = raw_ae,
    dm = dm
  )

  # 5. Validate AE and referential integrity

  validate_ae(ae, dm)

  if (nrow(dm) != nrow(raw_dm)) {
    stop("DM record count mismatch.", call. = FALSE)
  }

  if (nrow(ae) != nrow(raw_ae)) {
    stop("AE record count mismatch.", call. = FALSE)
  }

  if (any(ae$STUDYID != study_id)) {
    stop("AE STUDYID mismatch.", call. = FALSE)
  }

  # 6. Build validation report

  checks <- c(
    "Input CSV parsing",
    "DM generation",
    "DM validation",
    "AE generation",
    "AE validation",
    "DM record count",
    "AE record count",
    "AE primary key uniqueness",
    "AE to DM referential integrity",
    "Study identifier consistency"
  )

  executed_at <- format(
    Sys.time(),
    "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )

  elapsed <- as.numeric(
    difftime(Sys.time(), started_at, units = "secs")
  )

  validation_report <- tibble::tibble(
    STUDYID = study_id,
    DOMAIN = "AE",
    CHECK_ID = sprintf("AE%03d", seq_along(checks)),
    CHECK_DESCRIPTION = checks,
    STATUS = rep("PASS", length(checks)),
    RAW_DM_RECORDS = nrow(raw_dm),
    DM_RECORDS = nrow(dm),
    RAW_AE_RECORDS = nrow(raw_ae),
    AE_RECORDS = nrow(ae),
    AE_UNIQUE_PATIENTS = length(unique(ae$USUBJID)),
    EXECUTION_TIME_SECONDS = round(elapsed, 3),
    EXECUTED_AT_UTC = executed_at
  )

  # 7. Prepare output directory

  if (!dir.exists(output_dir)) {
    dir.create(
      output_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  if (!dir.exists(output_dir)) {
    stop("Unable to create output directory.", call. = FALSE)
  }

  dm_path <- file.path(output_dir, "dm.csv")
  ae_path <- file.path(output_dir, "ae.csv")
  report_path <- file.path(
    output_dir,
    "ae_validation_report.csv"
  )

  # 8. Export validated datasets

  readr::write_csv(dm, dm_path, na = "")
  readr::write_csv(ae, ae_path, na = "")

  readr::write_csv(
    validation_report,
    report_path,
    na = ""
  )

  # 9. Return execution results

  list(
    dm = dm,
    ae = ae,
    validation_report = validation_report,
    dm_path = dm_path,
    ae_path = ae_path,
    report_path = report_path
  )
}
