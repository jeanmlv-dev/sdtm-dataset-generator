
# Internal utility functions shared across SDTM domains.

# Validate required columns in a data frame.
check_required_columns <- function(data, required, label = "Dataset") {

  if (!is.data.frame(data)) {
    stop(
      paste0(label, " must be a data.frame."),
      call. = FALSE
    )
  }

  missing_columns <- setdiff(required, names(data))

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        label,
        " missing required columns: ",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


# Validate non-missing character identifiers.
check_required_values <- function(x, variable) {

  x <- as.character(x)

  if (anyNA(x) || any(!nzchar(trimws(x)))) {
    stop(
      paste0(variable, " contains missing or empty values."),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


# Parse and validate complete ISO 8601 calendar dates.
parse_iso_date <- function(x, variable, allow_missing = TRUE) {

  x <- as.character(x)

  x[!is.na(x) & !nzchar(trimws(x))] <- NA_character_

  if (!allow_missing && anyNA(x)) {
    stop(
      paste0(variable, " contains missing dates."),
      call. = FALSE
    )
  }

  parsed <- suppressWarnings(
    as.Date(x, format = "%Y-%m-%d")
  )

  valid <- is.na(x) |
    (
      !is.na(parsed) &
        grepl("^\\d{4}-\\d{2}-\\d{2}$", x) &
        format(parsed, "%Y-%m-%d") == x
    )

  if (!all(valid)) {
    stop(
      paste0(
        variable,
        " must contain valid YYYY-MM-DD dates."
      ),
      call. = FALSE
    )
  }

  parsed
}


# Calculate SDTM study day relative to RFSTDTC.
derive_study_day <- function(event_date, reference_date) {

  difference <- as.integer(event_date - reference_date)

  result <- ifelse(
    is.na(difference),
    NA_integer_,
    ifelse(
      difference >= 0L,
      difference + 1L,
      difference
    )
  )

  as.integer(result)
}


# Map raw values using an explicit lookup dictionary.
map_controlled_values <- function(x, lookup, variable) {

  x <- trimws(as.character(x))

  check_required_values(x, variable)

  unknown_values <- setdiff(unique(x), names(lookup))

  if (length(unknown_values) > 0L) {
    stop(
      paste0(
        variable,
        " contains unmapped values: ",
        paste(unknown_values, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  unname(lookup[x])
}
