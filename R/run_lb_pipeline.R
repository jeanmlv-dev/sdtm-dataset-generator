
#' Run the SDTM-like Laboratory Tests Pipeline
#'
#' Reads synthetic raw DM and LB data, generates SDTM-like
#' datasets, validates their structure and consistency,
#' and exports validated CSV artifacts.
#'
#' The pipeline performs:
#' \enumerate{
#'   \item Input file and structural validation.
#'   \item DM and LB dataset generation.
#'   \item Domain-level validation.
#'   \item Additional quality and integrity checks.
#'   \item Validation report generation.
#'   \item CSV export after successful validation.
#' }
#'
#' @param lb_input_path Path to the raw LB CSV file.
#' @param dm_input_path Path to the raw DM CSV file.
#' @param study_id Non-empty study identifier.
#' @param output_dir Directory for validated output files.
#'
#' @return A list containing `dm`, `lb`,
#'   `validation_report`, `dm_path`, `lb_path`,
#'   and `report_path`.
#'
#' @export
run_lb_pipeline <- function(
  lb_input_path,
  dm_input_path,
  study_id,
  output_dir = "outputs"
) {

  # -------------------------------------------------------
  # 1. Validate pipeline arguments
  # -------------------------------------------------------

  check_path <- function(path, label) {
    if (
      !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(trimws(path))
    ) {
      stop(
        paste(label, "must be a non-empty path."),
        call. = FALSE
      )
    }
  }

  check_path(lb_input_path, "lb_input_path")
  check_path(dm_input_path, "dm_input_path")
  check_path(output_dir, "output_dir")

  if (
    !is.character(study_id) ||
    length(study_id) != 1L ||
    is.na(study_id) ||
    !nzchar(trimws(study_id))
  ) {
    stop(
      "study_id must be a non-empty character value.",
      call. = FALSE
    )
  }

  if (
    !file.exists(lb_input_path) ||
    !file.exists(dm_input_path)
  ) {
    stop(
      "Input CSV file does not exist.",
      call. = FALSE
    )
  }

  if (
    dir.exists(lb_input_path) ||
    dir.exists(dm_input_path)
  ) {
    stop(
      "Input paths must refer to CSV files.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 2. Read raw input datasets
  # -------------------------------------------------------

  raw_dm <- readr::read_csv(
    file = dm_input_path,
    col_types = readr::cols(
      subject_id = readr::col_character(),
      site_id = readr::col_character(),
      .default = readr::col_guess()
    ),
    show_col_types = FALSE
  )

  raw_lb <- readr::read_csv(
    file = lb_input_path,
    col_types = readr::cols(
      subject_id = readr::col_character(),
      site_id = readr::col_character(),
      lab_id = readr::col_character(),
      lab_test = readr::col_character(),
      collection_date = readr::col_character(),
      visit = readr::col_character(),
      unit = readr::col_character(),
      .default = readr::col_guess()
    ),
    show_col_types = FALSE
  )

  if (nrow(raw_dm) == 0L || nrow(raw_lb) == 0L) {
    stop(
      "Input datasets must not be empty.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 3. Validate raw dataset structure
  # -------------------------------------------------------

  required_dm <- c(
    "subject_id", "site_id", "age",
    "sex", "arm", "rfstdtc"
  )

  required_lb <- c(
    "subject_id",
    "site_id",
    "lab_id",
    "lab_test",
    "result",
    "unit",
    "lower_limit",
    "upper_limit",
    "collection_date",
    "visit",
    "visitnum"
  )

  missing_dm <- setdiff(required_dm, names(raw_dm))
  missing_lb <- setdiff(required_lb, names(raw_lb))

  if (length(missing_dm) > 0L) {
    stop(
      paste(
        "Missing raw DM columns:",
        paste(missing_dm, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (length(missing_lb) > 0L) {
    stop(
      paste(
        "Missing raw LB columns:",
        paste(missing_lb, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Reject malformed CSV parsing.
  if (
    nrow(readr::problems(raw_dm)) > 0L ||
    nrow(readr::problems(raw_lb)) > 0L
  ) {
    stop(
      "Input CSV files contain parsing problems.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 4. Generate SDTM-like datasets
  # -------------------------------------------------------

  dm <- generate_dm(
    raw_data = raw_dm,
    study_id = study_id
  )

  lb <- generate_lb(
    raw_data = raw_lb,
    dm = dm
  )

  # -------------------------------------------------------
  # 5. Validate generated datasets
  # -------------------------------------------------------

  validate_dm(dm)
  validate_lb(lb, dm)

  # -------------------------------------------------------
  # 6. Additional quality checks
  # -------------------------------------------------------

  checks <- list(
    DM_VALIDATION = function() {
      validate_dm(dm)
    },

    LB_VALIDATION = function() {
      validate_lb(lb, dm)
    },

    DM_UNIQUE_SUBJECTS = function() {
      if (anyDuplicated(dm$USUBJID)) {
        stop("Duplicate DM subjects detected.")
      }
    },

    LB_PRIMARY_KEYS = function() {
      if (anyDuplicated(lb[c("USUBJID", "LBSEQ")])) {
        stop("Duplicate LB primary keys detected.")
      }
    },

    LB_REFERENTIAL_INTEGRITY = function() {
      if (!all(lb$USUBJID %in% dm$USUBJID)) {
        stop("LB subjects not found in DM.")
      }
    },

    LB_STUDY_CONSISTENCY = function() {
      dm_index <- match(lb$USUBJID, dm$USUBJID)

      if (
        anyNA(dm_index) ||
        !all(lb$STUDYID == dm$STUDYID[dm_index])
      ) {
        stop("LB STUDYID is inconsistent with DM.")
      }
    },

    LB_STANDARD_RESULTS = function() {
      if (
        !is.numeric(lb$LBSTRESN) ||
        anyNA(lb$LBSTRESN) ||
        any(!is.finite(lb$LBSTRESN))
      ) {
        stop("Invalid LB standardized numeric results.")
      }
    },

    LB_REFERENCE_LIMITS = function() {
      if (
        anyNA(lb$LBSTNRLO) ||
        anyNA(lb$LBSTNRHI) ||
        any(lb$LBSTNRLO > lb$LBSTNRHI)
      ) {
        stop("Invalid LB reference limits.")
      }
    },

    LB_COLLECTION_DATES = function() {
      parsed <- suppressWarnings(
        as.Date(lb$LBDTC, format = "%Y-%m-%d")
      )

      if (anyNA(parsed)) {
        stop("Invalid LB collection dates.")
      }
    },

    LB_RECORD_COUNT = function() {
      if (nrow(lb) != nrow(raw_lb)) {
        stop("LB record count differs from raw input.")
      }
    }
  )

  # -------------------------------------------------------
  # 7. Execute quality checks
  # -------------------------------------------------------

  check_names <- names(checks)

  validation_report <- data.frame(
    CHECK = check_names,
    STATUS = rep("PASS", length(check_names)),
    MESSAGE = rep("Validation passed.", length(check_names)),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(checks)) {
    tryCatch(
      checks[[i]](),
      error = function(e) {
        validation_report$STATUS[i] <<- "FAIL"
        validation_report$MESSAGE[i] <<-
          conditionMessage(e)
      }
    )
  }

  if (any(validation_report$STATUS == "FAIL")) {
    failed <- validation_report[
      validation_report$STATUS == "FAIL",
      ,
      drop = FALSE
    ]

    stop(
      paste(
        "LB pipeline validation failed:",
        paste(
          paste(failed$CHECK, failed$MESSAGE, sep = ": "),
          collapse = "; "
        )
      ),
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 8. Prepare output paths
  # -------------------------------------------------------

  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  dm_path <- file.path(output_dir, "dm.csv")
  lb_path <- file.path(output_dir, "lb.csv")
  report_path <- file.path(
    output_dir,
    "lb_validation_report.csv"
  )

  # -------------------------------------------------------
  # 9. Export validated artifacts
  # -------------------------------------------------------

  # Stage all files before publishing. This reduces the
  # chance of partially written output files.

  staging_dir <- tempfile(
    pattern = "lb_pipeline_"
  )

  dir.create(staging_dir)

  on.exit(
    unlink(staging_dir, recursive = TRUE),
    add = TRUE
  )

  staged_dm <- file.path(staging_dir, "dm.csv")
  staged_lb <- file.path(staging_dir, "lb.csv")
  staged_report <- file.path(
    staging_dir,
    "lb_validation_report.csv"
  )

  readr::write_csv(dm, staged_dm)
  readr::write_csv(lb, staged_lb)
  readr::write_csv(
    validation_report,
    staged_report
  )

  staged_files <- c(
    staged_dm,
    staged_lb,
    staged_report
  )

  target_files <- c(
    dm_path,
    lb_path,
    report_path
  )

  copied <- file.copy(
    from = staged_files,
    to = target_files,
    overwrite = TRUE
  )

  if (!all(copied)) {
    stop(
      "Failed to export all LB pipeline artifacts.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 10. Return pipeline results
  # -------------------------------------------------------

  list(
    dm = dm,
    lb = lb,
    validation_report = validation_report,
    dm_path = dm_path,
    lb_path = lb_path,
    report_path = report_path
  )
}
