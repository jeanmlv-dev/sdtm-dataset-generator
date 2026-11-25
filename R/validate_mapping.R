
#' Validate Metadata-Driven Mapping Specifications
#'
#' Performs structural and semantic validation of mapping metadata
#' before transforming raw clinical data into SDTM-like datasets.
#'
#' Validation includes:
#' - Required metadata columns and data types.
#' - Supported transformations and valid parameters.
#' - Unique target variables within a single domain.
#' - Source variable availability.
#' - Numeric conversion compatibility.
#' - Controlled terminology dictionary integrity and coverage.
#' - Required target completeness.
#'
#' This function does not modify the input data or execute arbitrary
#' expressions from metadata.
#'
#' @param mapping A mapping data.frame, normally from read_mapping().
#' @param raw_data Optional source data.frame to validate against.
#' @param terminology Named list of controlled terminology dictionaries.
#' @param study_id Optional study identifier required for study_id rules.
#'
#' @return TRUE if all validation checks pass.
#' @export
#'
#' @examples
#' path <- system.file(
#'   "mappings", "dm_mapping.csv",
#'   package = "sdtmgenerator"
#' )
#'
#' if (nzchar(path)) {
#'   mapping <- read_mapping(path)
#'   validate_mapping(mapping)
#' }
validate_mapping <- function(
  mapping,
  raw_data = NULL,
  terminology = list(),
  study_id = NULL
) {

  # -------------------------------------------------------
  # 1. Validate mapping structure
  # -------------------------------------------------------

  if (!is.data.frame(mapping) || nrow(mapping) == 0L) {
    stop(
      "mapping must be a non-empty data.frame.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "domain",
    "source_variable",
    "target_variable",
    "transformation",
    "parameter",
    "required"
  )

  missing_columns <- setdiff(
    required_columns,
    names(mapping)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste(
        "Missing mapping columns:",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (anyDuplicated(names(mapping))) {
    stop(
      "Duplicate mapping column names detected.",
      call. = FALSE
    )
  }

  character_columns <- c(
    "domain",
    "source_variable",
    "target_variable",
    "transformation",
    "parameter"
  )

  for (field in character_columns) {
    if (!is.character(mapping[[field]])) {
      stop(
        paste(field, "must be a character column."),
        call. = FALSE
      )
    }
  }

  if (!is.logical(mapping$required) ||
      anyNA(mapping$required)) {
    stop(
      "required must contain logical TRUE/FALSE values.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 2. Validate domain and target identifiers
  # -------------------------------------------------------

  if (anyNA(mapping$domain) ||
      any(!nzchar(trimws(mapping$domain)))) {
    stop(
      "domain contains missing or empty values.",
      call. = FALSE
    )
  }

  if (length(unique(mapping$domain)) != 1L) {
    stop(
      "Mapping must contain exactly one domain.",
      call. = FALSE
    )
  }

  if (anyNA(mapping$target_variable) ||
      any(!nzchar(trimws(mapping$target_variable)))) {
    stop(
      "target_variable contains missing or empty values.",
      call. = FALSE
    )
  }

  identifier_pattern <- "^[A-Z][A-Z0-9_]*$"

  if (any(!grepl(
    identifier_pattern,
    mapping$domain
  ))) {
    stop(
      "Invalid domain identifier.",
      call. = FALSE
    )
  }

  if (any(!grepl(
    identifier_pattern,
    mapping$target_variable
  ))) {
    stop(
      "Invalid target variable identifier.",
      call. = FALSE
    )
  }

  if (anyDuplicated(mapping$target_variable)) {
    stop(
      "Duplicate target variable mappings detected.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 3. Validate supported transformations
  # -------------------------------------------------------

  supported_transformations <- c(
    "direct",
    "numeric",
    "constant",
    "study_id",
    "terminology_map"
  )

  if (anyNA(mapping$transformation) ||
      any(!mapping$transformation %in%
          supported_transformations)) {
    stop(
      "Unsupported mapping transformation detected.",
      call. = FALSE
    )
  }

  needs_source <- mapping$transformation %in% c(
    "direct",
    "numeric",
    "terminology_map"
  )

  needs_parameter <- mapping$transformation %in% c(
    "constant",
    "terminology_map"
  )

  # -------------------------------------------------------
  # 4. Validate source variable definitions
  # -------------------------------------------------------

  source_missing <- is.na(mapping$source_variable) |
    !nzchar(trimws(mapping$source_variable))

  if (any(needs_source & source_missing)) {
    stop(
      "Source variable is required for this transformation.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 5. Validate transformation parameters
  # -------------------------------------------------------

  parameter_missing <- is.na(mapping$parameter) |
    !nzchar(trimws(mapping$parameter))

  if (any(needs_parameter & parameter_missing)) {
    stop(
      "Transformation parameter is required.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 6. Validate terminology dictionary structure
  # -------------------------------------------------------

  if (!is.list(terminology)) {
    stop(
      "terminology must be a named list.",
      call. = FALSE
    )
  }

  if (length(terminology) > 0L) {

    terminology_names <- names(terminology)

    if (is.null(terminology_names) ||
        anyNA(terminology_names) ||
        any(!nzchar(terminology_names)) ||
        anyDuplicated(terminology_names)) {
      stop(
        "Terminology dictionaries must have unique names.",
        call. = FALSE
      )
    }

    for (dictionary_name in terminology_names) {

      dictionary <- terminology[[dictionary_name]]

      if (!is.character(dictionary) ||
          is.null(names(dictionary)) ||
          anyNA(dictionary) ||
          anyNA(names(dictionary)) ||
          any(!nzchar(names(dictionary))) ||
          anyDuplicated(names(dictionary))) {
        stop(
          paste(
            "Invalid terminology dictionary:",
            dictionary_name
          ),
          call. = FALSE
        )
      }
    }
  }

  # -------------------------------------------------------
  # 7. Validate study identifier
  # -------------------------------------------------------

  if (!is.null(study_id)) {

    if (!is.character(study_id) ||
        length(study_id) != 1L ||
        is.na(study_id) ||
        !nzchar(trimws(study_id))) {
      stop(
        "study_id must be a non-empty character string.",
        call. = FALSE
      )
    }
  }

  # -------------------------------------------------------
  # 8. Validate source dataset
  # -------------------------------------------------------

  if (!is.null(raw_data)) {

    if (!is.data.frame(raw_data) ||
        nrow(raw_data) == 0L) {
      stop(
        "raw_data must be a non-empty data.frame.",
        call. = FALSE
      )
    }

    source_variables <- unique(
      mapping$source_variable[needs_source]
    )

    missing_sources <- setdiff(
      source_variables,
      names(raw_data)
    )

    if (length(missing_sources) > 0L) {
      stop(
        paste(
          "Source variables not found:",
          paste(missing_sources, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    # -----------------------------------------------------
    # 9. Validate numeric conversion
    # -----------------------------------------------------

    numeric_rules <- which(
      mapping$transformation == "numeric"
    )

    for (i in numeric_rules) {

      source <- mapping$source_variable[i]

      original <- as.character(raw_data[[source]])

      converted <- suppressWarnings(
        as.numeric(original)
      )

      invalid <- !is.na(original) &
        nzchar(trimws(original)) &
        (!is.finite(converted) | is.na(converted))

      if (any(invalid)) {
        stop(
          paste(
            "Numeric conversion failed for:",
            source
          ),
          call. = FALSE
        )
      }
    }

    # -----------------------------------------------------
    # 10. Validate terminology coverage
    # -----------------------------------------------------

    terminology_rules <- which(
      mapping$transformation == "terminology_map"
    )

    for (i in terminology_rules) {

      source <- mapping$source_variable[i]
      parameter <- mapping$parameter[i]

      if (!parameter %in% names(terminology)) {
        stop(
          paste(
            "Terminology dictionary not found:",
            parameter
          ),
          call. = FALSE
        )
      }

      dictionary <- terminology[[parameter]]

      source_values <- as.character(
        raw_data[[source]]
      )

      unknown_values <- !is.na(source_values) &
        !source_values %in% names(dictionary)

      if (any(unknown_values)) {
        stop(
          paste(
            "Unmapped terminology values in:",
            source
          ),
          call. = FALSE
        )
      }
    }

    # -----------------------------------------------------
    # 11. Validate required source completeness
    # -----------------------------------------------------

    required_source_rules <- which(
      needs_source & mapping$required
    )

    for (i in required_source_rules) {

      source <- mapping$source_variable[i]

      values <- raw_data[[source]]

      if (anyNA(values) ||
          (is.character(values) &&
           any(!nzchar(trimws(values))))) {
        stop(
          paste(
            "Required source contains missing values:",
            source
          ),
          call. = FALSE
        )
      }
    }
  }

  # -------------------------------------------------------
  # 12. Validation completed
  # -------------------------------------------------------

  TRUE
}
