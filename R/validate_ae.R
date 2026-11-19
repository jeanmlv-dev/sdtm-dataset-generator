
#' Validate an SDTM-like Adverse Events Dataset
#'
#' Validates the structure, identifiers, controlled terminology,
#' temporal consistency, and optional DM referential integrity
#' of an Adverse Events (AE) dataset.
#'
#' This validator implements project-specific SDTM-like rules.
#' It does not establish full CDISC SDTM compliance.
#'
#' @param ae A data.frame containing mapped AE records.
#' @param dm Optional Demographics (DM) data.frame.
#'
#' @return TRUE if all validation checks pass.
#' @export
#'
#' @examples
#' \dontrun{
#' validate_ae(ae)
#' validate_ae(ae, dm)
#' }
validate_ae <- function(ae, dm = NULL) {

  # 1. Input and structural validation

  if (!is.data.frame(ae)) {
    stop("ae must be a data.frame.", call. = FALSE)
  }

  if (nrow(ae) == 0L) {
    stop("AE dataset must not be empty.", call. = FALSE)
  }

  required <- c(
    "STUDYID", "DOMAIN", "USUBJID", "AESEQ",
    "AETERM", "AESEV", "AESER", "AEREL",
    "AEOUT", "AESTDTC", "AEENDTC",
    "AESTDY", "AEENDY"
  )

  missing <- setdiff(required, names(ae))

  if (length(missing) > 0L) {
    stop(
      paste("Missing AE variables:", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }

  # 2. Required character identifiers

  required_text <- c(
    "STUDYID", "DOMAIN", "USUBJID", "AETERM"
  )

  for (variable in required_text) {
    values <- ae[[variable]]

    if (!is.character(values) ||
          anyNA(values) ||
          any(!nzchar(trimws(values)))) {
      stop(
        paste(variable, "must contain non-empty character values."),
        call. = FALSE
      )
    }
  }

  if (any(ae$DOMAIN != "AE")) {
    stop("DOMAIN must equal AE.", call. = FALSE)
  }

  if (length(unique(ae$STUDYID)) != 1L) {
    stop("AE must contain exactly one STUDYID.", call. = FALSE)
  }

  # 3. AE sequence and primary keys

  if (!is.numeric(ae$AESEQ) ||
        anyNA(ae$AESEQ) ||
        any(!is.finite(ae$AESEQ)) ||
        any(ae$AESEQ < 1 | ae$AESEQ != floor(ae$AESEQ))) {
    stop("AESEQ must contain positive integers.", call. = FALSE)
  }

  keys <- ae[c("STUDYID", "USUBJID", "AESEQ")]

  if (anyDuplicated(keys)) {
    stop("Duplicate AE primary keys detected.", call. = FALSE)
  }

  # 4. Controlled terminology

  allowed_values <- list(
    AESEV = c("MILD", "MODERATE", "SEVERE"),
    AESER = c("Y", "N"),
    AEREL = c(
      "RELATED", "NOT RELATED",
      "POSSIBLY RELATED", "UNLIKELY RELATED",
      "PROBABLY RELATED", "UNKNOWN"
    ),
    AEOUT = c(
      "RECOVERED/RESOLVED",
      "RECOVERING/RESOLVING",
      "NOT RECOVERED/NOT RESOLVED",
      "RECOVERED/RESOLVED WITH SEQUELAE",
      "FATAL",
      "UNKNOWN"
    )
  )

  for (variable in names(allowed_values)) {
    values <- ae[[variable]]

    if (!is.character(values) ||
          anyNA(values) ||
          any(!values %in% allowed_values[[variable]])) {
      stop(
        paste("Invalid controlled terminology in", variable),
        call. = FALSE
      )
    }
  }

  # 5. ISO 8601 complete calendar dates

  valid_iso_date <- function(values) {
    if (!is.character(values)) {
      return(rep(FALSE, length(values)))
    }

    result <- rep(FALSE, length(values))
    missing <- is.na(values) | values == ""
    result[missing] <- TRUE

    present <- which(!missing)

    if (length(present) > 0L) {
      candidate <- values[present]
      pattern_ok <- grepl(
        "^[0-9]{4}-[0-9]{2}-[0-9]{2}$",
        candidate
      )

      parsed <- suppressWarnings(
        as.Date(candidate, format = "%Y-%m-%d")
      )

      result[present] <- pattern_ok &
        !is.na(parsed) &
        !is.na(format(parsed, "%Y-%m-%d")) &
        format(parsed, "%Y-%m-%d") == candidate
    }

    result
  }

  for (variable in c("AESTDTC", "AEENDTC")) {
    if (!all(valid_iso_date(ae[[variable]]))) {
      stop(
        paste(variable, "contains invalid ISO 8601 dates."),
        call. = FALSE
      )
    }
  }

  # Start dates are mandatory in this project.
  if (anyNA(ae$AESTDTC) || any(ae$AESTDTC == "")) {
    stop("AESTDTC must not be missing.", call. = FALSE)
  }

  start_date <- as.Date(ae$AESTDTC)
  end_date <- as.Date(ae$AEENDTC)

  invalid_order <- !is.na(end_date) &
    end_date < start_date

  if (any(invalid_order)) {
    stop("AEENDTC cannot precede AESTDTC.", call. = FALSE)
  }

  # 6. Study day validation

  for (variable in c("AESTDY", "AEENDY")) {
    values <- ae[[variable]]

    if (!is.numeric(values) ||
          any(!is.na(values) &
                (!is.finite(values) |
                   values == 0 |
                   values != floor(values)))) {
      stop(
        paste(variable, "must contain non-zero integers or NA."),
        call. = FALSE
      )
    }
  }

  if (anyNA(ae$AESTDY)) {
    stop("AESTDY must not be missing.", call. = FALSE)
  }

  if (any(is.na(end_date) & !is.na(ae$AEENDY)) ||
        any(!is.na(end_date) & is.na(ae$AEENDY))) {
    stop(
      "AEENDY and AEENDTC must have consistent missingness.",
      call. = FALSE
    )
  }

  # Study days use the SDTM convention:
  # dates on/after RFSTDTC: date difference + 1
  # dates before RFSTDTC: date difference
  if (any(!is.na(end_date) &
            ae$AEENDY < ae$AESTDY)) {
    stop("AEENDY cannot precede AESTDY.", call. = FALSE)
  }

  # 7. Optional referential integrity with DM

  if (!is.null(dm)) {
    if (!is.data.frame(dm)) {
      stop("dm must be a data.frame.", call. = FALSE)
    }

    dm_required <- c("STUDYID", "USUBJID", "RFSTDTC")
    dm_missing <- setdiff(dm_required, names(dm))

    if (length(dm_missing) > 0L) {
      stop(
        paste(
          "Missing DM variables:",
          paste(dm_missing, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    validate_dm(dm)

    if (anyDuplicated(dm[c("STUDYID", "USUBJID")])) {
      stop("Duplicate DM subject keys detected.", call. = FALSE)
    }

    if (!is.character(dm$RFSTDTC) ||
          !all(valid_iso_date(dm$RFSTDTC))) {
      stop("DM RFSTDTC contains invalid dates.", call. = FALSE)
    }

    if (anyNA(dm$RFSTDTC) || any(dm$RFSTDTC == "")) {
      stop(
        "DM RFSTDTC is required to validate AE study days.",
        call. = FALSE
      )
    }

    ae_keys <- paste(ae$STUDYID, ae$USUBJID, sep = "\r")
    dm_keys <- paste(dm$STUDYID, dm$USUBJID, sep = "\r")

    positions <- match(ae_keys, dm_keys)

    if (anyNA(positions)) {
      stop(
        "AE contains subjects not found in DM.",
        call. = FALSE
      )
    }

    reference_date <- as.Date(dm$RFSTDTC[positions])

    calculate_day <- function(event_date, reference_date) {
      difference <- as.integer(event_date - reference_date)

      ifelse(
        is.na(difference),
        NA_integer_,
        difference + ifelse(difference >= 0L, 1L, 0L)
      )
    }

    expected_start <- calculate_day(
      start_date, reference_date
    )

    expected_end <- calculate_day(
      end_date, reference_date
    )

    if (!identical(
      as.numeric(ae$AESTDY),
      as.numeric(expected_start)
    )) {
      stop(
        "AESTDY is inconsistent with DM RFSTDTC.",
        call. = FALSE
      )
    }

    actual_end <- as.numeric(ae$AEENDY)
    expected_end <- as.numeric(expected_end)

    same_end <- (is.na(actual_end) & is.na(expected_end)) |
      (!is.na(actual_end) &
         !is.na(expected_end) &
         actual_end == expected_end)

    if (!all(same_end)) {
      stop(
        "AEENDY is inconsistent with DM RFSTDTC.",
        call. = FALSE
      )
    }
  }

  TRUE
}
