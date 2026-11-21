
#' Generate an SDTM-like Laboratory Test Results Dataset
#'
#' Transforms synthetic raw laboratory measurements into a
#' standardized LB dataset using the DM domain as reference.
#'
#' Features:
#' - Required-column and source-data validation
#' - Subject-level referential integrity with DM
#' - Controlled laboratory test terminology
#' - Original and standardized results
#' - Laboratory reference ranges
#' - Normal-range indicators
#' - ISO 8601 collection dates
#' - Study-day derivation relative to DM.RFSTDTC
#' - Deterministic subject-level sequence numbers
#'
#' This function generates SDTM-like demonstration data.
#' It does not claim full CDISC SDTM compliance or perform
#' official laboratory terminology coding.
#'
#' @param raw_data A data.frame containing raw laboratory data.
#' @param dm A validated DM dataset.
#'
#' @return A data.frame containing standardized LB records.
#' @export
generate_lb <- function(raw_data, dm) {

  # -------------------------------------------------------
  # 1. Validate source structure
  # -------------------------------------------------------

  if (!is.data.frame(raw_data) || nrow(raw_data) == 0L) {
    stop(
      "Raw LB data must be a non-empty data.frame.",
      call. = FALSE
    )
  }

  required_raw <- c(
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

  missing_raw <- setdiff(required_raw, names(raw_data))

  if (length(missing_raw) > 0L) {
    stop(
      paste(
        "Missing raw LB variables:",
        paste(missing_raw, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  validate_dm(dm)

  # -------------------------------------------------------
  # 2. Validate identifiers
  # -------------------------------------------------------

  check_text <- function(x, label) {
    if (
      !is.character(x) ||
      anyNA(x) ||
      any(!nzchar(trimws(x)))
    ) {
      stop(
        paste(label, "contains missing or invalid values."),
        call. = FALSE
      )
    }
  }

for (field in c(
  "subject_id",
  "site_id",
  "lab_id",
  "lab_test",
  "unit",
  "visit"
)) {
  check_text(raw_data[[field]], field)
}

if (!inherits(raw_data$collection_date, "Date") &&
    !is.character(raw_data$collection_date)) {
  stop(
    "collection_date must be a character or Date column.",
    call. = FALSE
  )
}

if (anyNA(raw_data$collection_date)) {
  stop(
    "collection_date contains missing values.",
    call. = FALSE
  )
}

raw_data$collection_date <- as.character(
  raw_data$collection_date
)

  if (anyDuplicated(raw_data$lab_id)) {
    stop(
      "Duplicate laboratory source identifiers detected.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(raw_data$visitnum) ||
    anyNA(raw_data$visitnum) ||
    any(!is.finite(raw_data$visitnum)) ||
    any(raw_data$visitnum < 0)
  ) {
    stop(
      "visitnum must contain non-negative numeric values.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 3. Match LB subjects to DM
  # -------------------------------------------------------

  dm_index <- match(
    paste(raw_data$site_id, raw_data$subject_id, sep = "|"),
    paste(dm$SITEID, dm$SUBJID, sep = "|")
  )

  if (anyNA(dm_index)) {
    stop(
      "LB subjects not found in DM.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 4. Controlled laboratory terminology
  # -------------------------------------------------------

  test_dictionary <- data.frame(
    LBTESTCD = c(
      "HGB", "WBC", "PLAT", "CREAT", "ALT", "CRP"
    ),
    LBTEST = c(
      "Hemoglobin",
      "Leukocytes",
      "Platelets",
      "Creatinine",
      "Alanine Aminotransferase",
      "C-Reactive Protein"
    ),
    LBCAT = c(
      "HEMATOLOGY",
      "HEMATOLOGY",
      "HEMATOLOGY",
      "CHEMISTRY",
      "CHEMISTRY",
      "CHEMISTRY"
    ),
    LBSTRESU = c(
      "g/dL",
      "10^9/L",
      "10^9/L",
      "mg/dL",
      "U/L",
      "mg/L"
    ),
    LBSPEC = c(
      "BLOOD",
      "BLOOD",
      "BLOOD",
      "SERUM",
      "SERUM",
      "SERUM"
    ),
    stringsAsFactors = FALSE
  )

  test_index <- match(
    raw_data$lab_test,
    test_dictionary$LBTESTCD
  )

  if (anyNA(test_index)) {
    stop(
      "Unsupported laboratory test codes.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 5. Validate results and reference ranges
  # -------------------------------------------------------

  parse_numeric <- function(x, label) {
    if (!is.numeric(x) && !is.character(x)) {
      stop(
        paste(label, "must be numeric or numeric text."),
        call. = FALSE
      )
    }

    values <- suppressWarnings(as.numeric(x))

    if (anyNA(values) || any(!is.finite(values))) {
      stop(
        paste(label, "must contain finite numeric values."),
        call. = FALSE
      )
    }

    values
  }

  result_numeric <- parse_numeric(
    raw_data$result,
    "result"
  )

  lower_numeric <- parse_numeric(
    raw_data$lower_limit,
    "lower_limit"
  )

  upper_numeric <- parse_numeric(
    raw_data$upper_limit,
    "upper_limit"
  )

  if (any(lower_numeric > upper_numeric)) {
    stop(
      "Lower reference limits exceed upper limits.",
      call. = FALSE
    )
  }

  expected_units <- test_dictionary$LBSTRESU[test_index]

  if (any(raw_data$unit != expected_units)) {
    stop(
      "Unsupported or inconsistent laboratory units.",
      call. = FALSE
    )
  }

  # All synthetic source units already match the
  # standardized units, so no conversion is required.
  standardized_numeric <- result_numeric
  standardized_lower <- lower_numeric
  standardized_upper <- upper_numeric

  # -------------------------------------------------------
  # 6. Derive normal-range indicators
  # -------------------------------------------------------

  normal_indicator <- ifelse(
    standardized_numeric < standardized_lower,
    "LOW",
    ifelse(
      standardized_numeric > standardized_upper,
      "HIGH",
      "NORMAL"
    )
  )

  # -------------------------------------------------------
  # 7. Validate and parse collection dates
  # -------------------------------------------------------

  collection_dates <- suppressWarnings(
    as.Date(
      raw_data$collection_date,
      format = "%Y-%m-%d"
    )
  )

  valid_dates <- grepl(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}$",
    raw_data$collection_date
  ) & !is.na(collection_dates)

  valid_dates[is.na(valid_dates)] <- FALSE

  if (any(valid_dates)) {
    valid_dates[valid_dates] <-
      format(
        collection_dates[valid_dates],
        "%Y-%m-%d"
      ) == raw_data$collection_date[valid_dates]
  }

  if (!all(valid_dates)) {
    stop(
      "collection_date must contain valid YYYY-MM-DD dates.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 8. Derive study days
  # -------------------------------------------------------

  reference_dates <- suppressWarnings(
    as.Date(
      dm$RFSTDTC[dm_index],
      format = "%Y-%m-%d"
    )
  )

  study_days <- rep(
    NA_integer_,
    nrow(raw_data)
  )

  has_reference <- !is.na(reference_dates)

  differences <- as.integer(
    collection_dates[has_reference] -
      reference_dates[has_reference]
  )

  study_days[has_reference] <- ifelse(
    differences >= 0L,
    differences + 1L,
    differences
  )

  # -------------------------------------------------------
  # 9. Assemble LB dataset
  # -------------------------------------------------------

  lb <- data.frame(
    STUDYID = dm$STUDYID[dm_index],
    DOMAIN = rep("LB", nrow(raw_data)),
    USUBJID = dm$USUBJID[dm_index],
    LBSEQ = rep(NA_integer_, nrow(raw_data)),
    LBTESTCD = test_dictionary$LBTESTCD[test_index],
    LBTEST = test_dictionary$LBTEST[test_index],
    LBCAT = test_dictionary$LBCAT[test_index],
    LBSPEC = test_dictionary$LBSPEC[test_index],
    VISIT = raw_data$visit,
    VISITNUM = raw_data$visitnum,
    LBORRES = as.character(raw_data$result),
    LBORRESU = raw_data$unit,
    LBSTRESC = as.character(standardized_numeric),
    LBSTRESN = standardized_numeric,
    LBSTRESU = expected_units,
    LBORNRLO = lower_numeric,
    LBORNRHI = upper_numeric,
    LBSTNRLO = standardized_lower,
    LBSTNRHI = standardized_upper,
    LBNRIND = normal_indicator,
    LBDTC = format(collection_dates, "%Y-%m-%d"),
    LBDY = study_days,
    stringsAsFactors = FALSE
  )

  # -------------------------------------------------------
  # 10. Deterministic record ordering and LBSEQ
  # -------------------------------------------------------

  ordering <- order(
    lb$USUBJID,
    lb$VISITNUM,
    lb$LBTESTCD,
    raw_data$lab_id
  )

  lb <- lb[ordering, , drop = FALSE]

  lb$LBSEQ <- as.integer(
    sequence(
      rle(lb$USUBJID)$lengths
    )
  )

  rownames(lb) <- NULL

  # -------------------------------------------------------
  # 11. Final structural checks
  # -------------------------------------------------------

  if (anyDuplicated(lb[c("USUBJID", "LBSEQ")])) {
    stop(
      "Duplicate LB primary keys detected.",
      call. = FALSE
    )
  }

  if (!all(lb$USUBJID %in% dm$USUBJID)) {
    stop(
      "LB referential integrity check failed.",
      call. = FALSE
    )
  }

  lb
}
