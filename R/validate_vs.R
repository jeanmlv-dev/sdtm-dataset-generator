
#' Validate an SDTM-like Vital Signs Dataset
#'
#' Performs structural, referential, terminology, unit,
#' result, and temporal consistency checks on a VS dataset.
#'
#' @param vs A data.frame containing mapped VS records.
#' @param dm A validated DM dataset used as reference.
#'
#' @return TRUE if all validation checks pass.
#' @export
validate_vs <- function(vs, dm) {
  # 1. Validate input structure

  if (!is.data.frame(vs) || nrow(vs) == 0L) {
    stop(
      "VS must be a non-empty data.frame.",
      call. = FALSE
    )
  }

  validate_dm(dm)

  required_vs <- c(
    "STUDYID", "DOMAIN", "USUBJID", "VSSEQ",
    "VSTESTCD", "VSTEST", "VISIT", "VISITNUM",
    "VSORRES", "VSORRESU", "VSSTRESC",
    "VSSTRESN", "VSSTRESU", "VSDTC", "VSSTDY"
  )

  missing_vs <- setdiff(required_vs, names(vs))

  if (length(missing_vs) > 0L) {
    stop(
      paste(
        "Missing VS variables:",
        paste(missing_vs, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # 2. Validate identifiers and domain

  if (
    !is.character(vs$DOMAIN) ||
    anyNA(vs$DOMAIN) ||
    any(vs$DOMAIN != "VS")
  ) {
    stop("DOMAIN must be VS.", call. = FALSE)
  }

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

  text_fields <- c(
    "STUDYID", "USUBJID", "VSTESTCD", "VSTEST",
    "VISIT", "VSORRES", "VSORRESU",
    "VSSTRESC", "VSSTRESU", "VSDTC"
  )

  for (field in text_fields) {
    check_text(vs[[field]], field)
  }

  # 3. Validate referential integrity with DM

  dm_index <- match(vs$USUBJID, dm$USUBJID)

  if (anyNA(dm_index)) {
    stop(
      "VS subjects not found in DM.",
      call. = FALSE
    )
  }

  if (any(vs$STUDYID != dm$STUDYID[dm_index])) {
    stop(
      "STUDYID is inconsistent with DM.",
      call. = FALSE
    )
  }

  # 4. Validate VSSEQ

  if (
    !is.numeric(vs$VSSEQ) ||
    anyNA(vs$VSSEQ) ||
    any(!is.finite(vs$VSSEQ)) ||
    any(vs$VSSEQ <= 0) ||
    any(vs$VSSEQ != floor(vs$VSSEQ))
  ) {
    stop(
      "VSSEQ must contain positive integers.",
      call. = FALSE
    )
  }

  if (anyDuplicated(vs[c("USUBJID", "VSSEQ")]) > 0L) {
    stop(
      "Duplicate VS primary keys detected.",
      call. = FALSE
    )
  }

  # 5. Validate test terminology

  test_names <- c(
    SYSBP = "Systolic Blood Pressure",
    DIABP = "Diastolic Blood Pressure",
    HR = "Heart Rate",
    TEMP = "Temperature",
    WEIGHT = "Weight",
    HEIGHT = "Height"
  )

  if (any(!vs$VSTESTCD %in% names(test_names))) {
    stop(
      "Unsupported VSTESTCD values.",
      call. = FALSE
    )
  }

  expected_test <- unname(
    test_names[vs$VSTESTCD]
  )

  if (any(vs$VSTEST != expected_test)) {
    stop(
      "VSTEST is inconsistent with VSTESTCD.",
      call. = FALSE
    )
  }

  # 6. Validate visits

  if (
    !is.numeric(vs$VISITNUM) ||
    anyNA(vs$VISITNUM) ||
    any(!is.finite(vs$VISITNUM)) ||
    any(vs$VISITNUM < 0)
  ) {
    stop(
      "VISITNUM must contain non-negative numbers.",
      call. = FALSE
    )
  }

  visit_mapping <- unique(
    vs[c("VISIT", "VISITNUM")]
  )

  if (
    anyDuplicated(visit_mapping["VISIT"]) > 0L ||
    anyDuplicated(visit_mapping["VISITNUM"]) > 0L
  ) {
    stop(
      "VISIT and VISITNUM mapping is inconsistent.",
      call. = FALSE
    )
  }

  # 7. Validate results and units

  if (
    !is.numeric(vs$VSSTRESN) ||
    anyNA(vs$VSSTRESN) ||
    any(!is.finite(vs$VSSTRESN))
  ) {
    stop(
      "VSSTRESN must contain finite numeric values.",
      call. = FALSE
    )
  }

  allowed_units <- list(
    SYSBP = "mmHg",
    DIABP = "mmHg",
    HR = "beats/min",
    TEMP = c("C", "F"),
    WEIGHT = "kg",
    HEIGHT = "cm"
  )

  standard_units <- c(
    SYSBP = "mmHg",
    DIABP = "mmHg",
    HR = "beats/min",
    TEMP = "C",
    WEIGHT = "kg",
    HEIGHT = "cm"
  )

  valid_original_units <- mapply(
    function(code, unit) {
      unit %in% allowed_units[[code]]
    },
    vs$VSTESTCD,
    vs$VSORRESU,
    USE.NAMES = FALSE
  )

  if (!all(valid_original_units)) {
    stop(
      "VSORRESU is incompatible with VSTESTCD.",
      call. = FALSE
    )
  }

  expected_units <- unname(
    standard_units[vs$VSTESTCD]
  )

  if (any(vs$VSSTRESU != expected_units)) {
    stop(
      "VSSTRESU is inconsistent with VSTESTCD.",
      call. = FALSE
    )
  }

  original_numeric <- suppressWarnings(
    as.numeric(vs$VSORRES)
  )

  if (
    anyNA(original_numeric) ||
    any(!is.finite(original_numeric))
  ) {
    stop(
      "VSORRES must contain numeric measurements.",
      call. = FALSE
    )
  }

  expected_numeric <- original_numeric

  fahrenheit <- (
    vs$VSTESTCD == "TEMP" &
      vs$VSORRESU == "F"
  )

  expected_numeric[fahrenheit] <- (
    expected_numeric[fahrenheit] - 32
  ) * 5 / 9

  numeric_matches <- isTRUE(
    all.equal(
      as.numeric(vs$VSSTRESN),
      expected_numeric,
      tolerance = 1e-6,
      check.attributes = FALSE
    )
  )

  if (!numeric_matches) {
    stop(
      "VSSTRESN is inconsistent with original results.",
      call. = FALSE
    )
  }

  standardized_text <- suppressWarnings(
    as.numeric(vs$VSSTRESC)
  )

  if (
    anyNA(standardized_text) ||
    any(!is.finite(standardized_text))
  ) {
    stop(
      "VSSTRESC must contain numeric values.",
      call. = FALSE
    )
  }

  text_matches <- isTRUE(
    all.equal(
      standardized_text,
      as.numeric(vs$VSSTRESN),
      tolerance = 1e-6,
      check.attributes = FALSE
    )
  )

  if (!text_matches) {
    stop(
      "VSSTRESC is inconsistent with VSSTRESN.",
      call. = FALSE
    )
  }

  # 8. Validate ISO 8601 dates

  date_pattern <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"

  if (!all(grepl(date_pattern, vs$VSDTC))) {
    stop(
      "VSDTC must contain valid YYYY-MM-DD dates.",
      call. = FALSE
    )
  }

  parsed_dates <- suppressWarnings(
    as.Date(vs$VSDTC, format = "%Y-%m-%d")
  )

  if (anyNA(parsed_dates)) {
    stop(
      "VSDTC contains invalid calendar dates.",
      call. = FALSE
    )
  }

  normalized_dates <- format(
    parsed_dates,
    "%Y-%m-%d"
  )

  if (!all(normalized_dates == vs$VSDTC)) {
    stop(
      "VSDTC contains invalid calendar dates.",
      call. = FALSE
    )
  }

  # 9. Validate study days

  reference_dates <- suppressWarnings(
    as.Date(
      dm$RFSTDTC[dm_index],
      format = "%Y-%m-%d"
    )
  )

  expected_days <- rep(
    NA_integer_,
    nrow(vs)
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

  if (!is.numeric(vs$VSSTDY)) {
    stop(
      "VSSTDY must be numeric.",
      call. = FALSE
    )
  }

  if (!identical(
    is.na(vs$VSSTDY),
    is.na(expected_days)
  )) {
    stop(
      "VSSTDY is inconsistent with DM.RFSTDTC.",
      call. = FALSE
    )
  }

  if (any(
    vs$VSSTDY[has_reference] !=
      expected_days[has_reference]
  )) {
    stop(
      "VSSTDY is inconsistent with DM.RFSTDTC.",
      call. = FALSE
    )
  }

  TRUE
}
