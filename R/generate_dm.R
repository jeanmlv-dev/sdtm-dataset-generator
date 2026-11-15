
#' Generate an SDTM-like Demographics (DM) Dataset
#'
#' Transforms synthetic raw clinical trial demographics into a
#' standardized subset of CDISC SDTM DM variables.
#'
#' This function implements explicit mapping and structural checks.
#' It does not establish full CDISC SDTM compliance.
#'
#' @param raw_data A data.frame containing raw demographics.
#' @param study_id A non-empty study identifier.
#'
#' @return A tibble containing mapped DM variables.
#'
#' @details
#' Required raw columns:
#' subject_id, site_id, age, sex, arm, rfstdtc.
#'
#' Subject identifiers must be unique within the supplied study.
#' Dates must be missing or valid ISO 8601 calendar dates
#' in YYYY-MM-DD format.
#'
#' @export
#'
#' @examples
#' raw <- data.frame(
#'   subject_id = c("001", "002"),
#'   site_id = c("101", "101"),
#'   age = c(35, 48),
#'   sex = c("M", "F"),
#'   arm = c("Placebo", "Active"),
#'   rfstdtc = c("2026-01-01", "2026-01-02")
#' )
#'
#' generate_dm(raw, study_id = "STUDY001")
generate_dm <- function(raw_data, study_id) {

  if (!is.data.frame(raw_data)) {
    stop("raw_data must be a data.frame.", call. = FALSE)
  }

  if (!is.character(study_id) ||
        length(study_id) != 1L ||
        is.na(study_id) ||
        !nzchar(trimws(study_id))) {
    stop("study_id must be a non-empty string.", call. = FALSE)
  }

  required_columns <- c(
    "subject_id",
    "site_id",
    "age",
    "sex",
    "arm",
    "rfstdtc"
  )

  missing_columns <- setdiff(
    required_columns,
    names(raw_data)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste(
        "Missing required columns:",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (nrow(raw_data) == 0L) {
    stop("raw_data must contain at least one record.", call. = FALSE)
  }

  subject_id <- as.character(raw_data$subject_id)
  site_id <- as.character(raw_data$site_id)

  if (anyNA(subject_id) ||
        any(!nzchar(trimws(subject_id)))) {
    stop("subject_id contains missing or empty values.", call. = FALSE)
  }

  if (anyNA(site_id) ||
        any(!nzchar(trimws(site_id)))) {
    stop("site_id contains missing or empty values.", call. = FALSE)
  }

  if (anyDuplicated(subject_id)) {
    stop("Duplicate subject_id values detected.", call. = FALSE)
  }

  if (!is.numeric(raw_data$age) ||
        any(!is.na(raw_data$age) &
              (!is.finite(raw_data$age) |
                 raw_data$age < 0))) {
    stop("age must contain non-negative numeric values.", call. = FALSE)
  }

  sex <- toupper(trimws(as.character(raw_data$sex)))

  if (anyNA(sex) || any(!sex %in% c("M", "F", "U"))) {
    stop("sex must contain M, F, or U.", call. = FALSE)
  }

  arm <- trimws(as.character(raw_data$arm))

  if (anyNA(arm) || any(!nzchar(arm))) {
    stop("arm contains missing or empty values.", call. = FALSE)
  }

  # Explicit, study-specific synthetic arm mapping.
  arm_lookup <- c(
    "Placebo" = "PBO",
    "Active" = "ACT"
  )

  if (any(!arm %in% names(arm_lookup))) {
    stop("arm contains values not defined in arm_lookup.",
         call. = FALSE)
  }

  rfstdtc <- as.character(raw_data$rfstdtc)

  valid_date <- is.na(rfstdtc) |
    (
      !is.na(rfstdtc) &
        grepl("^\\d{4}-\\d{2}-\\d{2}$", rfstdtc) &
        !is.na(as.Date(rfstdtc, format = "%Y-%m-%d")) &
        format(
          as.Date(rfstdtc, format = "%Y-%m-%d"),
          "%Y-%m-%d"
        ) == rfstdtc
    )

  if (!all(valid_date)) {
    stop("rfstdtc must contain valid YYYY-MM-DD dates or NA.",
         call. = FALSE)
  }

  dm <- tibble::tibble(
    STUDYID = rep(study_id, nrow(raw_data)),
    DOMAIN = rep("DM", nrow(raw_data)),
    USUBJID = paste(study_id, site_id, subject_id, sep = "-"),
    SUBJID = subject_id,
    SITEID = site_id,
    RFSTDTC = rfstdtc,
    AGE = as.numeric(raw_data$age),
    AGEU = rep("YEARS", nrow(raw_data)),
    SEX = sex,
    ARMCD = unname(arm_lookup[arm]),
    ARM = arm
  )

  dm
}
