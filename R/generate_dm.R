
#' Generate an SDTM-like Demographics (DM) Dataset
#'
#' Transforms synthetic raw clinical trial demographics into a
#' standardized subset of CDISC SDTM DM variables.
#'
#' Supports two execution modes:
#'
#' - Legacy mode: uses the original hard-coded mapping rules.
#' - Metadata-driven mode: uses CSV mapping metadata through
#'   read_mapping(), validate_mapping(), and apply_mapping().
#'
#' Both modes preserve the same DM output structure.
#'
#' This function does not establish full CDISC SDTM compliance.
#'
#' @param raw_data A data.frame containing raw demographics.
#' @param study_id A non-empty study identifier.
#' @param mapping_path Optional path to a mapping metadata CSV.
#'   If NULL, the original mapping logic is used.
#'
#' @return A tibble containing mapped DM variables.
#'
#' @details
#' Required raw columns:
#' subject_id, site_id, age, sex, arm, rfstdtc.
#'
#' The output contains:
#' STUDYID, DOMAIN, USUBJID, SUBJID, SITEID, RFSTDTC,
#' AGE, AGEU, SEX, ARMCD, ARM.
#'
#' Subject identifiers must be unique within the supplied study.
#' Dates must be missing or valid ISO 8601 calendar dates
#' in YYYY-MM-DD format.
#'
#' In metadata-driven mode, domain-specific derived variables
#' are calculated after applying the mapping rules.
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
generate_dm <- function(
  raw_data,
  study_id,
  mapping_path = NULL
) {

  # ----------------------------------------------------------
  # 1. Validate input structure
  # ----------------------------------------------------------

  if (!is.data.frame(raw_data)) {
    stop(
      "raw_data must be a data.frame.",
      call. = FALSE
    )
  }

  if (!is.character(study_id) ||
      length(study_id) != 1L ||
      is.na(study_id) ||
      !nzchar(trimws(study_id))) {
    stop(
      "study_id must be a non-empty string.",
      call. = FALSE
    )
  }

  if (!is.null(mapping_path) &&
      (!is.character(mapping_path) ||
       length(mapping_path) != 1L ||
       is.na(mapping_path) ||
       !nzchar(trimws(mapping_path)))) {
    stop(
      "mapping_path must be NULL or a non-empty file path.",
      call. = FALSE
    )
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
    stop(
      "raw_data must contain at least one record.",
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # 2. Validate source identifiers
  # ----------------------------------------------------------

  subject_id <- as.character(raw_data$subject_id)
  site_id <- as.character(raw_data$site_id)

  if (anyNA(subject_id) ||
      any(!nzchar(trimws(subject_id)))) {
    stop(
      "subject_id contains missing or empty values.",
      call. = FALSE
    )
  }

  if (anyNA(site_id) ||
      any(!nzchar(trimws(site_id)))) {
    stop(
      "site_id contains missing or empty values.",
      call. = FALSE
    )
  }

  if (anyDuplicated(subject_id)) {
    stop(
      "Duplicate subject_id values detected.",
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # 3. Validate demographics
  # ----------------------------------------------------------

  if (!is.numeric(raw_data$age) ||
      any(!is.na(raw_data$age) &
          (!is.finite(raw_data$age) |
           raw_data$age < 0))) {
    stop(
      "age must contain non-negative numeric values.",
      call. = FALSE
    )
  }

  sex <- toupper(
    trimws(as.character(raw_data$sex))
  )

  if (anyNA(sex) ||
      any(!sex %in% c("M", "F", "U"))) {
    stop(
      "sex must contain M, F, or U.",
      call. = FALSE
    )
  }

  arm <- trimws(
    as.character(raw_data$arm)
  )

  if (anyNA(arm) ||
      any(!nzchar(arm))) {
    stop(
      "arm contains missing or empty values.",
      call. = FALSE
    )
  }

  arm_lookup <- c(
    "Placebo" = "PBO",
    "Active" = "ACT"
  )

  if (any(!arm %in% names(arm_lookup))) {
    stop(
      "arm contains values not defined in arm_lookup.",
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # 4. Validate reference dates
  # ----------------------------------------------------------

  rfstdtc <- as.character(raw_data$rfstdtc)

  parsed_dates <- suppressWarnings(
    as.Date(
      rfstdtc,
      format = "%Y-%m-%d"
    )
  )

  valid_date <- is.na(rfstdtc) |
    (
      grepl(
        "^[0-9]{4}-[0-9]{2}-[0-9]{2}$",
        rfstdtc
      ) &
      !is.na(parsed_dates) &
      format(
        parsed_dates,
        "%Y-%m-%d"
      ) == rfstdtc
    )

  valid_date[is.na(valid_date)] <- FALSE

  if (!all(valid_date)) {
    stop(
      "rfstdtc must contain valid YYYY-MM-DD dates or NA.",
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # 5. Define canonical DM structure
  # ----------------------------------------------------------

  dm_columns <- c(
    "STUDYID",
    "DOMAIN",
    "USUBJID",
    "SUBJID",
    "SITEID",
    "RFSTDTC",
    "AGE",
    "AGEU",
    "SEX",
    "ARMCD",
    "ARM"
  )

  # ----------------------------------------------------------
  # 6. Legacy mode
  # ----------------------------------------------------------

  if (is.null(mapping_path)) {

    dm <- tibble::tibble(
      STUDYID = rep(study_id, nrow(raw_data)),
      DOMAIN = rep("DM", nrow(raw_data)),
      USUBJID = paste(
        study_id,
        site_id,
        subject_id,
        sep = "-"
      ),
      SUBJID = subject_id,
      SITEID = site_id,
      RFSTDTC = rfstdtc,
      AGE = as.numeric(raw_data$age),
      AGEU = rep("YEARS", nrow(raw_data)),
      SEX = sex,
      ARMCD = unname(arm_lookup[arm]),
      ARM = arm
    )

    return(dm)
  }

  # ----------------------------------------------------------
  # 7. Metadata-driven mode
  # ----------------------------------------------------------

  mapping <- read_mapping(mapping_path)

  # Restrict mapping metadata to DM.
  mapping <- mapping[
    mapping$domain == "DM",
    ,
    drop = FALSE
  ]

  if (nrow(mapping) == 0L) {
    stop(
      "No DM mapping rules found.",
      call. = FALSE
    )
  }

  # Ensure all canonical DM targets are declared.
  missing_targets <- setdiff(
    dm_columns,
    mapping$target_variable
  )

  if (length(missing_targets) > 0L) {
    stop(
      paste(
        "Missing DM mapping targets:",
        paste(missing_targets, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Define controlled terminology dictionaries.
  terminology <- list(
    sex = c(
      "M" = "M",
      "F" = "F",
      "U" = "U"
    ),
    arm = arm_lookup
  )

  # Normalize supported source values before mapping.
  mapping_data <- raw_data

  mapping_data$subject_id <- subject_id
  mapping_data$site_id <- site_id
  mapping_data$sex <- sex
  mapping_data$arm <- arm
  mapping_data$rfstdtc <- rfstdtc

  # Validate metadata and source compatibility.
  validate_mapping(
    mapping = mapping,
    raw_data = mapping_data,
    terminology = terminology,
    study_id = study_id
  )

  # Execute metadata-driven transformations.
  mapped <- apply_mapping(
    raw_data = mapping_data,
    mapping = mapping,
    study_id = study_id,
    terminology = terminology
  )

  # ----------------------------------------------------------
  # 8. Domain-specific derivations
  # ----------------------------------------------------------

  # The current mapping engine supports direct, constant,
  # and terminology_map transformations.
  #
  # Composite identifiers and other domain-specific derived
  # variables are finalized here.

  mapped$STUDYID <- rep(
    study_id,
    nrow(mapping_data)
  )

  mapped$DOMAIN <- rep(
    "DM",
    nrow(mapping_data)
  )

  mapped$USUBJID <- paste(
    study_id,
    site_id,
    subject_id,
    sep = "-"
  )

  mapped$SUBJID <- subject_id
  mapped$SITEID <- site_id
  mapped$RFSTDTC <- rfstdtc
  mapped$AGE <- as.numeric(mapping_data$age)
  mapped$AGEU <- rep("YEARS", nrow(mapping_data))
  mapped$SEX <- sex
  mapped$ARMCD <- unname(arm_lookup[arm])
  mapped$ARM <- arm

  # ----------------------------------------------------------
  # 9. Enforce canonical DM output
  # ----------------------------------------------------------

  dm <- tibble::as_tibble(
    mapped[, dm_columns, drop = FALSE]
  )

  # Validate the final DM dataset.
  validate_dm(dm)

  dm
}

