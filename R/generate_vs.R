
#' Generate an SDTM-like Vital Signs Dataset
#'
#' Maps synthetic vital signs measurements into a standardized
#' vertical VS dataset, linked to a validated DM dataset.
#'
#' Supports both legacy programmatic mapping and declarative
#' metadata-driven mapping while preserving clinical derivations
#' and structural integrity.
#'
#' @param raw_data A data.frame of raw vital signs measurements.
#' @param dm A previously generated DM dataset.
#' @param mapping_path Optional path to a metadata mapping CSV file.
#'   When NULL, the legacy VS generation logic is used.
#'   When provided, the metadata-driven mapping workflow is enabled.
#'
#' @return A tibble containing mapped VS variables.
#'
#' @details
#' Required raw columns:
#' subject_id, site_id, visit, visitnum, test_code,
#' result, unit, assessment_date.
#'
#' Supported vital signs tests include systolic and diastolic
#' blood pressure, heart rate, temperature, weight and height.
#'
#' Temperature measurements in Fahrenheit are converted to Celsius
#' for standardized results.
#'
#' Subject identifiers are linked to the supplied DM dataset.
#' Duplicate source assessments are rejected.
#'
#' Study days are derived relative to DM.RFSTDTC.
#' VSSEQ is assigned deterministically within each subject.
#'
#' Metadata-driven mapping controls declarative variable mappings.
#' Clinical derivations, unit conversions and subject linkage
#' remain programmatically controlled.
#'
#' This function generates SDTM-like datasets for demonstration
#' purposes and does not establish full CDISC SDTM compliance.
#'
#' @export
generate_vs <- function(
    raw_data,
    dm,
    mapping_path = NULL
) {

  # ------------------------------------------------------------
  # 1. Validate input structure
  # ------------------------------------------------------------

  if (!is.data.frame(raw_data) || nrow(raw_data) == 0L) {
    stop(
      "raw_data must be a non-empty data.frame.",
      call. = FALSE
    )
  }

  validate_dm(dm)

  required_raw <- c(
    "subject_id",
    "site_id",
    "visit",
    "visitnum",
    "test_code",
    "result",
    "unit",
    "assessment_date"
  )

  missing_raw <- setdiff(
    required_raw,
    names(raw_data)
  )

  if (length(missing_raw) > 0L) {
    stop(
      paste(
        "Missing required columns:",
        paste(missing_raw, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # ------------------------------------------------------------
  # 2. Normalize input fields
  # ------------------------------------------------------------

  subject_id <- trimws(
    as.character(raw_data$subject_id)
  )

  site_id <- trimws(
    as.character(raw_data$site_id)
  )

  visit <- trimws(
    as.character(raw_data$visit)
  )

  test_code <- toupper(
    trimws(as.character(raw_data$test_code))
  )

  unit <- trimws(
    as.character(raw_data$unit)
  )

  date_text <- as.character(
    raw_data$assessment_date
  )

  check_text <- function(x, label) {

    if (anyNA(x) || any(!nzchar(x))) {
      stop(
        paste(
          label,
          "contains missing or empty values."
        ),
        call. = FALSE
      )
    }
  }

  for (field in c(
    "subject_id",
    "site_id",
    "visit",
    "test_code",
    "unit",
    "date_text"
  )) {
    check_text(get(field), field)
  }

  if (
    !is.numeric(raw_data$result) ||
    anyNA(raw_data$result) ||
    any(!is.finite(raw_data$result))
  ) {
    stop(
      "result must contain finite numeric values.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(raw_data$visitnum) ||
    anyNA(raw_data$visitnum) ||
    any(!is.finite(raw_data$visitnum))
  ) {
    stop(
      "visitnum must contain finite numeric values.",
      call. = FALSE
    )
  }

  # ------------------------------------------------------------
  # 3. Validate supported test codes
  # ------------------------------------------------------------

  test_names <- c(
    SYSBP = "Systolic Blood Pressure",
    DIABP = "Diastolic Blood Pressure",
    HR = "Heart Rate",
    TEMP = "Temperature",
    WEIGHT = "Weight",
    HEIGHT = "Height"
  )

  if (any(!test_code %in% names(test_names))) {
    stop(
      "Unsupported vital signs test code.",
      call. = FALSE
    )
  }

  # ------------------------------------------------------------
  # 4. Validate original units
  # ------------------------------------------------------------

  allowed_units <- list(
    SYSBP = "mmHg",
    DIABP = "mmHg",
    HR = "beats/min",
    TEMP = c("C", "F"),
    WEIGHT = "kg",
    HEIGHT = "cm"
  )

  valid_units <- mapply(
    function(code, value) {
      value %in% allowed_units[[code]]
    },
    test_code,
    unit
  )

  if (!all(valid_units)) {
    stop(
      "Unsupported unit for vital signs test.",
      call. = FALSE
    )
  }

  # ------------------------------------------------------------
  # 5. Validate calendar dates
  # ------------------------------------------------------------

  parsed_dates <- suppressWarnings(
    as.Date(
      date_text,
      format = "%Y-%m-%d"
    )
  )

  valid_dates <- grepl(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}$",
    date_text
  ) &
    !is.na(parsed_dates) &
    !is.na(date_text)

  valid_dates[is.na(valid_dates)] <- FALSE

  valid_dates <- valid_dates &
    !is.na(parsed_dates) &
    format(parsed_dates, "%Y-%m-%d") == date_text

  valid_dates[is.na(valid_dates)] <- FALSE

  if (!all(valid_dates)) {
    stop(
      paste(
        "assessment_date must contain",
        "valid YYYY-MM-DD dates."
      ),
      call. = FALSE
    )
  }

  # ------------------------------------------------------------
  # 6. Link subjects to DM
  # ------------------------------------------------------------

  raw_key <- paste(
    site_id,
    subject_id,
    sep = "\r"
  )

  dm_key <- paste(
    dm$SITEID,
    dm$SUBJID,
    sep = "\r"
  )

  if (anyDuplicated(dm_key)) {
    stop(
      "DM contains duplicate site/subject keys.",
      call. = FALSE
    )
  }

  dm_index <- match(
    raw_key,
    dm_key
  )

  if (anyNA(dm_index)) {
    stop(
      "VS subjects not found in DM.",
      call. = FALSE
    )
  }

  # ------------------------------------------------------------
  # 7. Reject duplicate source assessments
  # ------------------------------------------------------------

  assessment_key <- data.frame(
    USUBJID = dm$USUBJID[dm_index],
    VISITNUM = raw_data$visitnum,
    VSTESTCD = test_code,
    VSDTC = date_text
  )

  if (anyDuplicated(assessment_key)) {
    stop(
      "Duplicate VS assessments detected.",
      call. = FALSE
    )
  }

  # ------------------------------------------------------------
  # 8. Standardize results
  # ------------------------------------------------------------

  standard_result <- as.numeric(
    raw_data$result
  )

  fahrenheit <- test_code == "TEMP" &
    unit == "F"

  standard_result[fahrenheit] <-
    (standard_result[fahrenheit] - 32) * 5 / 9

  standard_unit <- c(
    SYSBP = "mmHg",
    DIABP = "mmHg",
    HR = "beats/min",
    TEMP = "C",
    WEIGHT = "kg",
    HEIGHT = "cm"
  )

  # ------------------------------------------------------------
  # 9. Derive study days
  # ------------------------------------------------------------

  reference_text <- as.character(
    dm$RFSTDTC[dm_index]
  )

  reference_dates <- suppressWarnings(
    as.Date(
      reference_text,
      format = "%Y-%m-%d"
    )
  )

  study_day <- rep(
    NA_integer_,
    nrow(raw_data)
  )

  has_reference <- !is.na(reference_dates)

  day_difference <- as.integer(
    parsed_dates[has_reference] -
      reference_dates[has_reference]
  )

  study_day[has_reference] <- ifelse(
    day_difference >= 0L,
    day_difference + 1L,
    day_difference
  )

  # ------------------------------------------------------------
  # 10. Define the expected VS output structure
  # ------------------------------------------------------------

  expected_columns <- c(
    "STUDYID",
    "DOMAIN",
    "USUBJID",
    "VSSEQ",
    "VSTESTCD",
    "VSTEST",
    "VISIT",
    "VISITNUM",
    "VSORRES",
    "VSORRESU",
    "VSSTRESC",
    "VSSTRESN",
    "VSSTRESU",
    "VSDTC",
    "VSSTDY"
  )

  # ------------------------------------------------------------
  # 11. Legacy mapping workflow
  # ------------------------------------------------------------

  if (is.null(mapping_path)) {

    vs <- tibble::tibble(
      STUDYID = dm$STUDYID[dm_index],
      DOMAIN = "VS",
      USUBJID = dm$USUBJID[dm_index],
      VSSEQ = NA_integer_,
      VSTESTCD = test_code,
      VSTEST = unname(test_names[test_code]),
      VISIT = visit,
      VISITNUM = as.numeric(raw_data$visitnum),
      VSORRES = as.character(raw_data$result),
      VSORRESU = unit,
      VSSTRESC = as.character(
        signif(standard_result, 10)
      ),
      VSSTRESN = standard_result,
      VSSTRESU = unname(standard_unit[test_code]),
      VSDTC = date_text,
      VSSTDY = study_day
    )

  } else {

    # ----------------------------------------------------------
    # 12. Metadata-driven mapping workflow
    # ----------------------------------------------------------

    # Validate the mapping path.

    if (
      !is.character(mapping_path) ||
      length(mapping_path) != 1L ||
      is.na(mapping_path) ||
      !nzchar(trimws(mapping_path))
    ) {
      stop(
        "mapping_path must be a non-empty file path.",
        call. = FALSE
      )
    }

    if (!file.exists(mapping_path)) {
      stop(
        paste(
          "Mapping file does not exist:",
          mapping_path
        ),
        call. = FALSE
      )
    }

    # Metadata-driven VS generation currently supports
    # one study per execution.

    study_ids <- unique(
      as.character(dm$STUDYID)
    )

    if (length(study_ids) != 1L) {
      stop(
        "DM must contain exactly one study.",
        call. = FALSE
      )
    }

    # Read the declarative mapping specification.

    mapping <- read_mapping(
      mapping_path
    )

    # Ensure that the mapping belongs to the VS domain.

    if (
      nrow(mapping) == 0L ||
      anyNA(mapping$domain) ||
      any(mapping$domain != "VS")
    ) {
      stop(
        "Mapping must contain only VS domain rules.",
        call. = FALSE
      )
    }

    # Define mandatory metadata-controlled targets.

    required_targets <- c(
      "STUDYID",
      "DOMAIN",
      "VSTESTCD",
      "VSTEST",
      "VISIT",
      "VISITNUM",
      "VSORRES",
      "VSORRESU",
      "VSDTC"
    )

    missing_targets <- setdiff(
      required_targets,
      mapping$target_variable
    )

    if (length(missing_targets) > 0L) {
      stop(
        paste(
          "Missing VS mapping targets:",
          paste(
            missing_targets,
            collapse = ", "
          )
        ),
        call. = FALSE
      )
    }

    # Prevent metadata from overriding protected
    # programmatic derivations.

    unsupported_targets <- setdiff(
      mapping$target_variable,
      required_targets
    )

    if (length(unsupported_targets) > 0L) {
      stop(
        paste(
          "Unsupported VS mapping targets:",
          paste(
            unsupported_targets,
            collapse = ", "
          )
        ),
        call. = FALSE
      )
    }

    # Define controlled terminology dictionaries.

    terminology <- list(
      vs_test = test_names
    )

    # Normalize a local copy of the source data.
    # The original source object remains unchanged.

    mapping_data <- raw_data

    mapping_data$subject_id <- subject_id
    mapping_data$site_id <- site_id
    mapping_data$visit <- visit
    mapping_data$test_code <- test_code
    mapping_data$result <- as.character(raw_data$result)
    mapping_data$unit <- unit
    mapping_data$assessment_date <- date_text

    # Validate metadata and source compatibility.

    validate_mapping(
      mapping = mapping,
      raw_data = mapping_data,
      terminology = terminology
    )

    # Execute the declarative mapping rules.

    mapped_vs <- apply_mapping(
      raw_data = mapping_data,
      mapping = mapping,
      study_id = study_ids[1L],
      terminology = terminology
    )

    # Verify that metadata did not alter the record count.

    if (nrow(mapped_vs) != nrow(raw_data)) {
      stop(
        "Raw and mapped VS record counts do not match.",
        call. = FALSE
      )
    }

    # Protect identifiers and clinical source values used
    # by programmatic derivations.

    if (
      !identical(
        as.character(mapped_vs$STUDYID),
        as.character(dm$STUDYID[dm_index])
      ) ||
      anyNA(mapped_vs$DOMAIN) ||
      any(mapped_vs$DOMAIN != "VS")
    ) {
      stop(
        "Mapped VS study or domain identifiers are inconsistent.",
        call. = FALSE
      )
    }

    if (
      !identical(
        as.character(mapped_vs$VSTESTCD),
        test_code
      ) ||
      !identical(
        as.character(mapped_vs$VSORRES),
        as.character(raw_data$result)
      ) ||
      !identical(
        as.character(mapped_vs$VSORRESU),
        unit
      ) ||
      !identical(
        as.character(mapped_vs$VSDTC),
        date_text
      ) ||
      !identical(
        as.numeric(mapped_vs$VISITNUM),
        as.numeric(raw_data$visitnum)
      )
    ) {
      stop(
        paste(
          "Mapped VS values are inconsistent with",
          "clinical derivation inputs."
        ),
        call. = FALSE
      )
    }

    # Assemble metadata-controlled variables.

    vs <- tibble::as_tibble(
      mapped_vs
    )

    # Add protected programmatic derivations.

    vs$USUBJID <- dm$USUBJID[dm_index]

    vs$VSSEQ <- rep(
      NA_integer_,
      nrow(raw_data)
    )

    vs$VSSTRESC <- as.character(
      signif(standard_result, 10)
    )

    vs$VSSTRESN <- standard_result

    vs$VSSTRESU <- unname(
      standard_unit[test_code]
    )

    vs$VSSTDY <- study_day

    # Ensure consistent output data types.

    vs$STUDYID <- as.character(vs$STUDYID)
    vs$DOMAIN <- as.character(vs$DOMAIN)
    vs$VSTESTCD <- as.character(vs$VSTESTCD)
    vs$VSTEST <- as.character(vs$VSTEST)
    vs$VISIT <- as.character(vs$VISIT)
    vs$VISITNUM <- as.numeric(vs$VISITNUM)
    vs$VSORRES <- as.character(vs$VSORRES)
    vs$VSORRESU <- as.character(vs$VSORRESU)
    vs$VSDTC <- as.character(vs$VSDTC)
  }

  # ------------------------------------------------------------
  # 13. Enforce consistent output column ordering
  # ------------------------------------------------------------

  vs <- vs[
    ,
    expected_columns,
    drop = FALSE
  ]

  # ------------------------------------------------------------
  # 14. Deterministic ordering and sequencing
  # ------------------------------------------------------------

  vs <- vs[
    order(
      vs$USUBJID,
      vs$VSDTC,
      vs$VISITNUM,
      vs$VSTESTCD
    ),
    ,
    drop = FALSE
  ]

  vs$VSSEQ <- stats::ave(
    seq_len(nrow(vs)),
    vs$USUBJID,
    FUN = seq_along
  )

  rownames(vs) <- NULL

  # ------------------------------------------------------------
  # 15. Final integrity checks
  # ------------------------------------------------------------

  if (nrow(vs) != nrow(raw_data)) {
    stop(
      "Raw and VS record counts do not match.",
      call. = FALSE
    )
  }

  if (anyDuplicated(
    vs[c("STUDYID", "USUBJID", "VSSEQ")]
  )) {
    stop(
      "Duplicate VS primary keys detected.",
      call. = FALSE
    )
  }

  # ------------------------------------------------------------
  # 16. Return standardized VS dataset
  # ------------------------------------------------------------

  vs
}
