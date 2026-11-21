
#' Validate an SDTM-like Laboratory Test Results Dataset
#'
#' Performs structural, referential, terminology, result,
#' unit, reference-range, normality, and temporal checks
#' on a synthetic LB dataset.
#'
#' The validation rules reflect the supported synthetic
#' laboratory dictionary and mapping conventions of this
#' project. They do not constitute a complete CDISC SDTM
#' compliance assessment.
#'
#' @param lb A data.frame containing mapped LB records.
#' @param dm A validated DM dataset used as reference.
#'
#' @return TRUE if all validation checks pass.
#' @export
validate_lb <- function(lb, dm) {

  # -------------------------------------------------------
  # 1. Validate input structure
  # -------------------------------------------------------

  if (!is.data.frame(lb) || nrow(lb) == 0L) {
    stop(
      "LB must be a non-empty data.frame.",
      call. = FALSE
    )
  }

  validate_dm(dm)

  required_lb <- c(
    "STUDYID",
    "DOMAIN",
    "USUBJID",
    "LBSEQ",
    "LBTESTCD",
    "LBTEST",
    "LBCAT",
    "LBSPEC",
    "VISIT",
    "VISITNUM",
    "LBORRES",
    "LBORRESU",
    "LBSTRESC",
    "LBSTRESN",
    "LBSTRESU",
    "LBORNRLO",
    "LBORNRHI",
    "LBSTNRLO",
    "LBSTNRHI",
    "LBNRIND",
    "LBDTC",
    "LBDY"
  )

  missing_lb <- setdiff(required_lb, names(lb))

  if (length(missing_lb) > 0L) {
    stop(
      paste(
        "Missing LB variables:",
        paste(missing_lb, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 2. Validate domain and text fields
  # -------------------------------------------------------

  if (
    !is.character(lb$DOMAIN) ||
    anyNA(lb$DOMAIN) ||
    any(lb$DOMAIN != "LB")
  ) {
    stop(
      "DOMAIN must be LB.",
      call. = FALSE
    )
  }

  check_text <- function(x, label) {

    if (
      !is.character(x) ||
      anyNA(x) ||
      any(!nzchar(trimws(x)))
    ) {
      stop(
        paste(
          label,
          "contains missing or invalid values."
        ),
        call. = FALSE
      )
    }

    invisible(TRUE)
  }

  text_fields <- c(
    "STUDYID",
    "USUBJID",
    "LBTESTCD",
    "LBTEST",
    "LBCAT",
    "LBSPEC",
    "VISIT",
    "LBORRES",
    "LBORRESU",
    "LBSTRESC",
    "LBSTRESU",
    "LBNRIND",
    "LBDTC"
  )

  for (field in text_fields) {
    check_text(lb[[field]], field)
  }

  # -------------------------------------------------------
  # 3. Validate referential integrity with DM
  # -------------------------------------------------------

  dm_index <- match(
    lb$USUBJID,
    dm$USUBJID
  )

  if (anyNA(dm_index)) {
    stop(
      "LB subjects not found in DM.",
      call. = FALSE
    )
  }

  if (any(
    lb$STUDYID != dm$STUDYID[dm_index]
  )) {
    stop(
      "STUDYID is inconsistent with DM.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 4. Validate LBSEQ and primary keys
  # -------------------------------------------------------

  if (
    !is.numeric(lb$LBSEQ) ||
    anyNA(lb$LBSEQ) ||
    any(!is.finite(lb$LBSEQ)) ||
    any(lb$LBSEQ <= 0) ||
    any(lb$LBSEQ != floor(lb$LBSEQ))
  ) {
    stop(
      "LBSEQ must contain positive integers.",
      call. = FALSE
    )
  }

  if (anyDuplicated(
    lb[c("USUBJID", "LBSEQ")]
  )) {
    stop(
      "Duplicate LB primary keys detected.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 5. Validate laboratory terminology
  # -------------------------------------------------------

  test_dictionary <- data.frame(
    LBTESTCD = c(
      "HGB",
      "WBC",
      "PLAT",
      "CREAT",
      "ALT",
      "CRP"
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
    LBSPEC = c(
      "BLOOD",
      "BLOOD",
      "BLOOD",
      "SERUM",
      "SERUM",
      "SERUM"
    ),
    LBSTRESU = c(
      "g/dL",
      "10^9/L",
      "10^9/L",
      "mg/dL",
      "U/L",
      "mg/L"
    ),
    stringsAsFactors = FALSE
  )

  test_index <- match(
    lb$LBTESTCD,
    test_dictionary$LBTESTCD
  )

  if (anyNA(test_index)) {
    stop(
      "Unsupported LBTESTCD values.",
      call. = FALSE
    )
  }

  check_mapping <- function(
    observed,
    expected,
    label
  ) {

    if (any(observed != expected)) {
      stop(
        paste(
          label,
          "is inconsistent with LBTESTCD."
        ),
        call. = FALSE
      )
    }

    invisible(TRUE)
  }

  check_mapping(
    lb$LBTEST,
    test_dictionary$LBTEST[test_index],
    "LBTEST"
  )

  check_mapping(
    lb$LBCAT,
    test_dictionary$LBCAT[test_index],
    "LBCAT"
  )

  check_mapping(
    lb$LBSPEC,
    test_dictionary$LBSPEC[test_index],
    "LBSPEC"
  )

  # -------------------------------------------------------
  # 6. Validate visits
  # -------------------------------------------------------

  if (
    !is.numeric(lb$VISITNUM) ||
    anyNA(lb$VISITNUM) ||
    any(!is.finite(lb$VISITNUM)) ||
    any(lb$VISITNUM < 0)
  ) {
    stop(
      "VISITNUM must contain non-negative numbers.",
      call. = FALSE
    )
  }

  visit_mapping <- unique(
    lb[c("VISIT", "VISITNUM")]
  )

  if (
    anyDuplicated(visit_mapping["VISIT"]) ||
    anyDuplicated(visit_mapping["VISITNUM"])
  ) {
    stop(
      "VISIT and VISITNUM mapping is inconsistent.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 7. Validate numeric results
  # -------------------------------------------------------

  check_numeric <- function(x, label) {

    if (
      !is.numeric(x) ||
      anyNA(x) ||
      any(!is.finite(x))
    ) {
      stop(
        paste(
          label,
          "must contain finite numeric values."
        ),
        call. = FALSE
      )
    }

    invisible(TRUE)
  }

  check_numeric(
    lb$LBSTRESN,
    "LBSTRESN"
  )

  original_numeric <- suppressWarnings(
    as.numeric(lb$LBORRES)
  )

  standardized_text <- suppressWarnings(
    as.numeric(lb$LBSTRESC)
  )

  if (
    anyNA(original_numeric) ||
    any(!is.finite(original_numeric))
  ) {
    stop(
      "LBORRES must contain numeric measurements.",
      call. = FALSE
    )
  }

  if (
    anyNA(standardized_text) ||
    any(!is.finite(standardized_text))
  ) {
    stop(
      "LBSTRESC must contain numeric text.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 8. Validate laboratory units
  # -------------------------------------------------------

  expected_units <-
    test_dictionary$LBSTRESU[test_index]

  if (any(
    lb$LBORRESU != expected_units
  )) {
    stop(
      "LBORRESU is incompatible with LBTESTCD.",
      call. = FALSE
    )
  }

  if (any(
    lb$LBSTRESU != expected_units
  )) {
    stop(
      "LBSTRESU is inconsistent with LBTESTCD.",
      call. = FALSE
    )
  }

  # Current synthetic mapping uses identical original
  # and standardized units. No conversion is applied.

  if (!isTRUE(all.equal(
    original_numeric,
    as.numeric(lb$LBSTRESN),
    tolerance = 1e-8,
    check.attributes = FALSE
  ))) {
    stop(
      "LBSTRESN is inconsistent with LBORRES.",
      call. = FALSE
    )
  }

  if (!isTRUE(all.equal(
    standardized_text,
    as.numeric(lb$LBSTRESN),
    tolerance = 1e-8,
    check.attributes = FALSE
  ))) {
    stop(
      "LBSTRESC is inconsistent with LBSTRESN.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 9. Validate reference ranges
  # -------------------------------------------------------

  reference_fields <- c(
    "LBORNRLO",
    "LBORNRHI",
    "LBSTNRLO",
    "LBSTNRHI"
  )

  for (field in reference_fields) {
    check_numeric(lb[[field]], field)
  }

  if (any(
    lb$LBORNRLO > lb$LBORNRHI
  )) {
    stop(
      "Original reference limits are inconsistent.",
      call. = FALSE
    )
  }

  if (any(
    lb$LBSTNRLO > lb$LBSTNRHI
  )) {
    stop(
      "Standardized reference limits are inconsistent.",
      call. = FALSE
    )
  }

  if (
    !isTRUE(all.equal(
      lb$LBORNRLO,
      lb$LBSTNRLO,
      tolerance = 1e-8,
      check.attributes = FALSE
    )) ||
    !isTRUE(all.equal(
      lb$LBORNRHI,
      lb$LBSTNRHI,
      tolerance = 1e-8,
      check.attributes = FALSE
    ))
  ) {
    stop(
      "Standardized reference limits are inconsistent with original limits.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 10. Validate normal-range indicators
  # -------------------------------------------------------

  allowed_indicators <- c(
    "LOW",
    "NORMAL",
    "HIGH"
  )

  if (any(
    !lb$LBNRIND %in% allowed_indicators
  )) {
    stop(
      "Unsupported LBNRIND values.",
      call. = FALSE
    )
  }

  expected_indicator <- ifelse(
    lb$LBSTRESN < lb$LBSTNRLO,
    "LOW",
    ifelse(
      lb$LBSTRESN > lb$LBSTNRHI,
      "HIGH",
      "NORMAL"
    )
  )

  if (any(
    lb$LBNRIND != expected_indicator
  )) {
    stop(
      "LBNRIND is inconsistent with reference ranges.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 11. Validate ISO 8601 collection dates
  # -------------------------------------------------------

  parsed_dates <- suppressWarnings(
    as.Date(
      lb$LBDTC,
      format = "%Y-%m-%d"
    )
  )

  valid_dates <- grepl(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}$",
    lb$LBDTC
  ) & !is.na(parsed_dates)

  valid_dates[is.na(valid_dates)] <- FALSE

  if (any(valid_dates)) {
    valid_dates[valid_dates] <-
      format(
        parsed_dates[valid_dates],
        "%Y-%m-%d"
      ) == lb$LBDTC[valid_dates]
  }

  if (!all(valid_dates)) {
    stop(
      "LBDTC must contain valid YYYY-MM-DD dates.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 12. Validate study-day derivation
  # -------------------------------------------------------

  reference_dates <- suppressWarnings(
    as.Date(
      dm$RFSTDTC[dm_index],
      format = "%Y-%m-%d"
    )
  )

  expected_days <- rep(
    NA_integer_,
    nrow(lb)
  )

  has_reference <- !is.na(reference_dates)

  differences <- as.integer(
    parsed_dates[has_reference] -
      reference_dates[has_reference]
  )

  expected_days[has_reference] <- ifelse(
    differences >= 0L,
    differences + 1L,
    differences
  )

  if (
    !is.numeric(lb$LBDY) ||
    !identical(
      is.na(lb$LBDY),
      is.na(expected_days)
    ) ||
    any(
      lb$LBDY[has_reference] !=
        expected_days[has_reference]
    )
  ) {
    stop(
      "LBDY is inconsistent with DM.RFSTDTC.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 13. Validation completed
  # -------------------------------------------------------

  TRUE
}
