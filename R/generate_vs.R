
#' Generate an SDTM-like Vital Signs Dataset
#'
#' Maps synthetic vital signs measurements into a standardized
#' vertical VS dataset, linked to a validated DM dataset.
#'
#' @param raw_data A data.frame of raw vital signs measurements.
#' @param dm A previously generated DM dataset.
#'
#' @return A tibble containing mapped VS variables.
#' @export
generate_vs <- function(raw_data, dm) {

  # 1. Input structure
  if (!is.data.frame(raw_data) || nrow(raw_data) == 0L) {
    stop("raw_data must be a non-empty data.frame.",
         call. = FALSE)
  }

  validate_dm(dm)

  required_raw <- c(
    "subject_id", "site_id", "visit", "visitnum",
    "test_code", "result", "unit", "assessment_date"
  )

  missing_raw <- setdiff(required_raw, names(raw_data))

  if (length(missing_raw) > 0L) {
    stop(
      paste("Missing required columns:",
            paste(missing_raw, collapse = ", ")),
      call. = FALSE
    )
  }

  # 2. Normalize input fields
  subject_id <- trimws(as.character(raw_data$subject_id))
  site_id <- trimws(as.character(raw_data$site_id))
  visit <- trimws(as.character(raw_data$visit))
  test_code <- toupper(trimws(as.character(raw_data$test_code)))
  unit <- trimws(as.character(raw_data$unit))
  date_text <- as.character(raw_data$assessment_date)

  check_text <- function(x, label) {
    if (anyNA(x) || any(!nzchar(x))) {
      stop(paste(label, "contains missing or empty values."),
           call. = FALSE)
    }
  }

  for (field in c("subject_id", "site_id", "visit",
                  "test_code", "unit", "date_text")) {
    check_text(get(field), field)
  }

  if (!is.numeric(raw_data$result) ||
        anyNA(raw_data$result) ||
        any(!is.finite(raw_data$result))) {
    stop("result must contain finite numeric values.",
         call. = FALSE)
  }

  if (!is.numeric(raw_data$visitnum) ||
        anyNA(raw_data$visitnum) ||
        any(!is.finite(raw_data$visitnum))) {
    stop("visitnum must contain finite numeric values.",
         call. = FALSE)
  }

  # 3. Validate supported test codes
  test_names <- c(
    SYSBP = "Systolic Blood Pressure",
    DIABP = "Diastolic Blood Pressure",
    HR = "Heart Rate",
    TEMP = "Temperature",
    WEIGHT = "Weight",
    HEIGHT = "Height"
  )

  if (any(!test_code %in% names(test_names))) {
    stop("Unsupported vital signs test code.", call. = FALSE)
  }

  # 4. Validate original units
  allowed_units <- list(
    SYSBP = "mmHg",
    DIABP = "mmHg",
    HR = "beats/min",
    TEMP = c("C", "F"),
    WEIGHT = "kg",
    HEIGHT = "cm"
  )

  valid_units <- mapply(
    function(code, value) value %in% allowed_units[[code]],
    test_code,
    unit
  )

  if (!all(valid_units)) {
    stop("Unsupported unit for vital signs test.",
         call. = FALSE)
  }

  # 5. Validate calendar dates
  parsed_dates <- suppressWarnings(
    as.Date(date_text, format = "%Y-%m-%d")
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
    stop("assessment_date must contain valid YYYY-MM-DD dates.",
         call. = FALSE)
  }

  # 6. Link subjects to DM
  raw_key <- paste(site_id, subject_id, sep = "\r")
  dm_key <- paste(dm$SITEID, dm$SUBJID, sep = "\r")

  if (anyDuplicated(dm_key)) {
    stop("DM contains duplicate site/subject keys.",
         call. = FALSE)
  }

  dm_index <- match(raw_key, dm_key)

  if (anyNA(dm_index)) {
    stop("VS subjects not found in DM.", call. = FALSE)
  }

  # 7. Reject duplicate source assessments
  assessment_key <- data.frame(
    USUBJID = dm$USUBJID[dm_index],
    VISITNUM = raw_data$visitnum,
    VSTESTCD = test_code,
    VSDTC = date_text
  )

  if (anyDuplicated(assessment_key)) {
    stop("Duplicate VS assessments detected.",
         call. = FALSE)
  }

  # 8. Standardize results
  standard_result <- as.numeric(raw_data$result)

  fahrenheit <- test_code == "TEMP" & unit == "F"

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

  # 9. Derive study days
  reference_text <- as.character(dm$RFSTDTC[dm_index])

  reference_dates <- suppressWarnings(
    as.Date(reference_text, format = "%Y-%m-%d")
  )

  study_day <- rep(NA_integer_, nrow(raw_data))

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

  # 10. Assemble VS
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
    VSSTRESC = as.character(signif(standard_result, 10)),
    VSSTRESN = standard_result,
    VSSTRESU = unname(standard_unit[test_code]),
    VSDTC = date_text,
    VSSTDY = study_day
  )

  # 11. Deterministic ordering and sequencing
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

  vs$VSSEQ <-
    stats::ave(
      seq_len(nrow(vs)),
      vs$USUBJID,
      FUN = seq_along
    )

  rownames(vs) <- NULL

  vs
}
