
#' Validate the Structure of a DM Dataset
#'
#' Performs basic structural and data integrity checks
#' on a generated SDTM-like DM dataset.
#'
#' @param dm A data.frame containing mapped DM variables.
#'
#' @return TRUE when all checks pass.
#' @export
validate_dm <- function(dm) {

  if (!is.data.frame(dm)) {
    stop("dm must be a data.frame.", call. = FALSE)
  }

  required_variables <- c(
    "STUDYID", "DOMAIN", "USUBJID",
    "SUBJID", "SITEID", "RFSTDTC",
    "AGE", "AGEU", "SEX", "ARMCD", "ARM"
  )

  missing_variables <- setdiff(required_variables, names(dm))

  if (length(missing_variables) > 0L) {
    stop(
      paste(
        "Missing DM variables:",
        paste(missing_variables, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (nrow(dm) == 0L) {
    stop("DM dataset must not be empty.", call. = FALSE)
  }

  if (anyNA(dm$USUBJID) ||
        any(!nzchar(trimws(dm$USUBJID)))) {
    stop("USUBJID contains missing values.", call. = FALSE)
  }

  if (anyDuplicated(dm$USUBJID)) {
    stop("USUBJID must be unique.", call. = FALSE)
  }

  if (anyNA(dm$DOMAIN) || any(dm$DOMAIN != "DM")) {
    stop("DOMAIN must be DM.", call. = FALSE)
  }

  if (anyNA(dm$STUDYID) ||
        any(!nzchar(trimws(dm$STUDYID)))) {
    stop("STUDYID must not be missing.", call. = FALSE)
  }

  if (!is.numeric(dm$AGE) ||
        any(!is.na(dm$AGE) &
              (!is.finite(dm$AGE) | dm$AGE < 0))) {
    stop(
      "AGE must contain non-negative numeric values.",
      call. = FALSE
    )
  }

  if (anyNA(dm$AGEU) || any(dm$AGEU != "YEARS")) {
    stop("AGEU must be YEARS for this implementation.",
         call. = FALSE)
  }

  if (anyNA(dm$SEX) ||
        any(!dm$SEX %in% c("M", "F", "U"))) {
    stop("Invalid SEX values.", call. = FALSE)
  }

  if (anyNA(dm$ARMCD) ||
        any(!dm$ARMCD %in% c("PBO", "ACT"))) {
    stop("Invalid ARMCD values.", call. = FALSE)
  }

  TRUE
}
