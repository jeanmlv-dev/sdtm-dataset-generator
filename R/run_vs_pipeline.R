
#' Run an End-to-End SDTM-like VS Pipeline
#'
#' Reads synthetic raw demographics and vital signs,
#' generates DM and VS datasets, validates their structure,
#' and exports datasets and a validation report.
#'
#' @param vs_input_path Path to the raw VS CSV.
#' @param dm_input_path Path to the raw DM CSV.
#' @param study_id A non-empty study identifier.
#' @param output_dir Directory for exported artifacts.
#'
#' @return A list containing DM, VS, validation report,
#'   and output file paths.
#'
#' @details
#' Subject and site identifiers are imported as character
#' values to preserve leading zeros.
#'
#' This pipeline implements a limited SDTM-like mapping.
#' It does not establish full CDISC SDTM compliance.
#'
#' @export
run_vs_pipeline <- function(
  vs_input_path,
  dm_input_path,
  study_id,
  output_dir = "outputs"
) {
  # 1. Validate arguments

  check_input <- function(path) {
    if (
      !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(trimws(path)) ||
      !file.exists(path) ||
      dir.exists(path)
    ) {
      stop(
        "Input CSV file does not exist.",
        call. = FALSE
      )
    }
  }

  check_input(vs_input_path)
  check_input(dm_input_path)

  if (
    !is.character(study_id) ||
    length(study_id) != 1L ||
    is.na(study_id) ||
    !nzchar(trimws(study_id))
  ) {
    stop(
      "study_id must be a non-empty string.",
      call. = FALSE
    )
  }

  if (
    !is.character(output_dir) ||
    length(output_dir) != 1L ||
    is.na(output_dir) ||
    !nzchar(trimws(output_dir))
  ) {
    stop(
      "output_dir must be a non-empty string.",
      call. = FALSE
    )
  }

  started_at <- Sys.time()

  # 2. Read raw clinical data

  raw_dm <- readr::read_csv(
    dm_input_path,
    col_types = readr::cols(
      subject_id = readr::col_character(),
      site_id = readr::col_character(),
      age = readr::col_double(),
      sex = readr::col_character(),
      arm = readr::col_character(),
      rfstdtc = readr::col_character(),
      .default = readr::col_character()
    ),
    show_col_types = FALSE,
    progress = FALSE
  )

  raw_vs <- readr::read_csv(
  vs_input_path,
  col_types = readr::cols(
    subject_id = readr::col_character(),
    site_id = readr::col_character(),
    .default = readr::col_guess()
  ),
  show_col_types = FALSE,
  progress = FALSE
  )

  if (
    nrow(readr::problems(raw_dm)) > 0L ||
    nrow(readr::problems(raw_vs)) > 0L
  ) {
    stop(
      "Input CSV contains parsing errors.",
      call. = FALSE
    )
  }

  if (
    nrow(raw_dm) == 0L ||
    nrow(raw_vs) == 0L
  ) {
    stop(
      "Input datasets must not be empty.",
      call. = FALSE
    )
  }

  # 3. Generate SDTM-like datasets

  dm <- generate_dm(
    raw_data = raw_dm,
    study_id = study_id
  )

  vs <- generate_vs(
    raw_data = raw_vs,
    dm = dm
  )

  # 4. Validate generated datasets

  if (!isTRUE(validate_dm(dm))) {
    stop("DM validation failed.", call. = FALSE)
  }

  if (!isTRUE(validate_vs(vs, dm))) {
    stop("VS validation failed.", call. = FALSE)
  }

  # 5. Check record counts and referential integrity

  dm_count_match <- nrow(dm) == nrow(raw_dm)
  vs_count_match <- nrow(vs) == nrow(raw_vs)

  subjects_valid <- all(
    vs$USUBJID %in% dm$USUBJID
  )

  keys_unique <- anyDuplicated(
    vs[c("USUBJID", "VSSEQ")]
  ) == 0L

  domain_valid <- all(vs$DOMAIN == "VS")

  study_valid <- all(
    vs$STUDYID == study_id
  )

  if (
    !dm_count_match ||
    !vs_count_match ||
    !subjects_valid ||
    !keys_unique ||
    !domain_valid ||
    !study_valid
  ) {
    stop(
      "VS pipeline integrity checks failed.",
      call. = FALSE
    )
  }

  # 6. Build validation report

  elapsed_seconds <- as.numeric(
    difftime(
      Sys.time(),
      started_at,
      units = "secs"
    )
  )

  check_ids <- sprintf(
    "VS%03d",
    seq_len(10L)
  )

  descriptions <- c(
    "DM input CSV parsed successfully",
    "VS input CSV parsed successfully",
    "DM transformation and validation passed",
    "VS transformation and validation passed",
    "DM record counts match",
    "VS record counts match",
    "VS subjects exist in DM",
    "VS primary keys are unique",
    "VS DOMAIN values are consistent",
    "VS STUDYID values are consistent"
  )

  check_results <- c(
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    dm_count_match,
    vs_count_match,
    subjects_valid,
    keys_unique,
    domain_valid,
    study_valid
  )

  validation_report <- tibble::tibble(
    STUDYID = study_id,
    DOMAIN = "VS",
    CHECK_ID = check_ids,
    CHECK_DESCRIPTION = descriptions,
    STATUS = ifelse(
      check_results,
      "PASS",
      "FAIL"
    ),
    RAW_DM_RECORDS = nrow(raw_dm),
    RAW_VS_RECORDS = nrow(raw_vs),
    DM_RECORDS = nrow(dm),
    VS_RECORDS = nrow(vs),
    EXECUTION_TIME_SECONDS = round(
      elapsed_seconds,
      3
    ),
    EXECUTED_AT_UTC = format(
      Sys.time(),
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    )
  )

  if (any(validation_report$STATUS != "PASS")) {
    stop(
      "One or more VS pipeline checks failed.",
      call. = FALSE
    )
  }

  # 7. Export validated artifacts

  if (!dir.exists(output_dir)) {
    dir.create(
      output_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  if (!dir.exists(output_dir)) {
    stop(
      "Unable to create output directory.",
      call. = FALSE
    )
  }

  dm_path <- file.path(
    output_dir,
    "dm.csv"
  )

  vs_path <- file.path(
    output_dir,
    "vs.csv"
  )

  report_path <- file.path(
    output_dir,
    "vs_validation_report.csv"
  )

  readr::write_csv(
    dm,
    dm_path,
    na = ""
  )

  readr::write_csv(
    vs,
    vs_path,
    na = ""
  )

  readr::write_csv(
    validation_report,
    report_path,
    na = ""
  )

  # 8. Return results

  list(
    dm = dm,
    vs = vs,
    validation_report = validation_report,
    dm_path = dm_path,
    vs_path = vs_path,
    report_path = report_path
  )
}
