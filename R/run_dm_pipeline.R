
#' Run an End-to-End SDTM DM Pipeline
#'
#' Reads synthetic raw clinical demographics from a CSV file,
#' generates an SDTM-like DM dataset, validates its structure,
#' and exports the dataset and an execution validation report.
#'
#' @param input_path Path to the raw demographics CSV file.
#' @param study_id A non-empty study identifier.
#' @param output_dir Directory where output files will be written.
#'
#' @return A list containing the generated DM dataset,
#'   validation report, and output file paths.
#'
#' @details
#' Subject and site identifiers are read as character values
#' to preserve leading zeros.
#'
#' Validation is limited to the rules implemented in
#' generate_dm() and validate_dm(). It does not establish
#' full CDISC SDTM compliance.
#'
#' @export
run_dm_pipeline <- function(
    input_path,
    study_id,
    output_dir = "outputs"
) {

  # ---------------------------------------------------------
  # 1. Validate pipeline arguments
  # ---------------------------------------------------------

  if (!is.character(input_path) ||
      length(input_path) != 1L ||
      is.na(input_path) ||
      !file.exists(input_path) ||
      dir.exists(input_path)) {
    stop("Input CSV file does not exist.", call. = FALSE)
  }

  if (!is.character(output_dir) ||
      length(output_dir) != 1L ||
      is.na(output_dir) ||
      !nzchar(trimws(output_dir))) {
    stop("output_dir must be a non-empty string.", call. = FALSE)
  }

  started_at <- Sys.time()

  # ---------------------------------------------------------
  # 2. Ingest raw clinical data
  # ---------------------------------------------------------

  raw_data <- readr::read_csv(
    file = input_path,
    col_types = readr::cols(
      subject_id = readr::col_character(),
      site_id = readr::col_character(),
      age = readr::col_double(),
      sex = readr::col_character(),
      arm = readr::col_character(),
      rfstdtc = readr::col_character(),
      .default = readr::col_character()
    ),
    na = c("", "NA"),
    show_col_types = FALSE,
    progress = FALSE
  )

  parsing_problems <- readr::problems(raw_data)

  if (nrow(parsing_problems) > 0L) {
    stop(
      "Input CSV contains parsing errors.",
      call. = FALSE
    )
  }

  if (nrow(raw_data) == 0L) {
    stop("Input CSV contains no records.", call. = FALSE)
  }

  # ---------------------------------------------------------
  # 3. Transform raw data into SDTM-like DM
  # ---------------------------------------------------------

  dm <- generate_dm(
    raw_data = raw_data,
    study_id = study_id
  )

  # ---------------------------------------------------------
  # 4. Validate generated DM dataset
  # ---------------------------------------------------------

  validation_passed <- validate_dm(dm)

  if (!isTRUE(validation_passed)) {
    stop("DM validation failed.", call. = FALSE)
  }

  if (nrow(dm) != nrow(raw_data)) {
    stop(
      "Record count mismatch between raw data and DM.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------
  # 5. Build execution validation report
  # ---------------------------------------------------------

  elapsed_seconds <- as.numeric(
    difftime(
      Sys.time(),
      started_at,
      units = "secs"
    )
  )

  validation_report <- tibble::tibble(
    STUDYID = study_id,
    DOMAIN = "DM",
    CHECK_ID = c(
      "DM001",
      "DM002",
      "DM003",
      "DM004",
      "DM005",
      "DM006",
      "DM007"
    ),
    CHECK_DESCRIPTION = c(
      "Input CSV parsed successfully",
      "Raw-to-DM transformation completed",
      "DM structural validation passed",
      "Raw and DM record counts match",
      "USUBJID values are unique",
      "DOMAIN values equal DM",
      "Study identifiers are consistent"
    ),
    STATUS = c(
      "PASS",
      "PASS",
      "PASS",
      if (nrow(dm) == nrow(raw_data)) "PASS" else "FAIL",
      if (anyDuplicated(dm$USUBJID) == 0L) "PASS" else "FAIL",
      if (all(dm$DOMAIN == "DM")) "PASS" else "FAIL",
      if (all(dm$STUDYID == study_id)) "PASS" else "FAIL"
    ),
    RAW_RECORDS = nrow(raw_data),
    OUTPUT_RECORDS = nrow(dm),
    EXECUTION_TIME_SECONDS = round(elapsed_seconds, 3),
    EXECUTED_AT_UTC = format(
      Sys.time(),
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    )
  )

  if (any(validation_report$STATUS == "FAIL")) {
    stop("One or more DM pipeline checks failed.", call. = FALSE)
  }

  # ---------------------------------------------------------
  # 6. Export pipeline artifacts
  # ---------------------------------------------------------

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

  report_path <- file.path(
    output_dir,
    "dm_validation_report.csv"
  )

  readr::write_csv(dm, dm_path, na = "")

  readr::write_csv(
    validation_report,
    report_path,
    na = ""
  )

  # ---------------------------------------------------------
  # 7. Return pipeline results
  # ---------------------------------------------------------

  list(
    dm = dm,
    validation_report = validation_report,
    dm_path = dm_path,
    report_path = report_path
  )
}
