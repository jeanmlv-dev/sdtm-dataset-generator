
#' Read Metadata-Driven Mapping Specifications
#'
#' Loads and validates a CSV mapping specification used to transform
#' raw clinical data into SDTM-like domain datasets.
#'
#' @param path Path to a CSV mapping specification.
#'
#' @return A tibble containing validated mapping metadata.
#' @export
#'
#' @examples
#' mapping_path <- system.file(
#'   "mappings", "dm_mapping.csv",
#'   package = "sdtmgenerator"
#' )
#' if (nzchar(mapping_path)) {
#'   mapping <- read_mapping(mapping_path)
#' }
read_mapping <- function(path) {

  if (!is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(trimws(path))) {
    stop("path must be a non-empty character string.",
         call. = FALSE)
  }

  if (!file.exists(path) || dir.exists(path)) {
    stop("Mapping file does not exist.", call. = FALSE)
  }

  mapping <- tryCatch(
    readr::read_csv(
      path,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE,
      name_repair = "minimal"
    ),
    error = function(e) {
      stop(
        paste("Unable to read mapping file:", conditionMessage(e)),
        call. = FALSE
      )
    }
  )

  required_columns <- c(
    "domain",
    "source_variable",
    "target_variable",
    "transformation",
    "parameter",
    "required"
  )

  missing_columns <- setdiff(required_columns, names(mapping))

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
    stop("Duplicate mapping column names detected.",
         call. = FALSE)
  }

  if (nrow(mapping) == 0L) {
    stop("Mapping specification must not be empty.",
         call. = FALSE)
  }

  mapping <- mapping[, required_columns]

  mandatory_fields <- c(
    "domain",
    "target_variable",
    "transformation",
    "required"
  )

  for (field in mandatory_fields) {
    values <- mapping[[field]]

    if (anyNA(values) || any(!nzchar(trimws(values)))) {
      stop(
        paste(field, "contains missing or empty values."),
        call. = FALSE
      )
    }

    mapping[[field]] <- trimws(values)
  }

  for (field in c("source_variable", "parameter")) {
    values <- trimws(mapping[[field]])
    values[!is.na(values) & !nzchar(values)] <- NA_character_
    mapping[[field]] <- values
  }

  mapping$domain <- toupper(mapping$domain)
  mapping$target_variable <- toupper(mapping$target_variable)

  supported_transformations <- c(
    "direct",
    "constant",
    "study_id",
    "numeric",
    "terminology_map"
  )

  if (any(!mapping$transformation %in% supported_transformations)) {
    stop("Unsupported mapping transformation detected.",
         call. = FALSE)
  }

  if (any(!mapping$required %in% c("TRUE", "FALSE"))) {
    stop("required must contain TRUE or FALSE.",
         call. = FALSE)
  }

  mapping$required <- mapping$required == "TRUE"

  if (anyDuplicated(mapping[c("domain", "target_variable")])) {
    stop("Duplicate domain and target variable mappings detected.",
         call. = FALSE)
  }

  needs_source <- mapping$transformation %in% c(
    "direct",
    "numeric",
    "terminology_map"
  )

  if (any(is.na(mapping$source_variable[needs_source]))) {
    stop("Source variable is required for this transformation.",
         call. = FALSE)
  }

  needs_parameter <- mapping$transformation %in% c(
    "constant",
    "terminology_map"
  )

  if (any(is.na(mapping$parameter[needs_parameter]))) {
    stop("Transformation parameter is required.",
         call. = FALSE)
  }

  mapping
}
