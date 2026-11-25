
#' Apply Metadata-Driven Variable Mapping
#'
#' Transforms raw clinical data according to a validated mapping
#' specification. Only explicitly supported transformations are
#' executed; arbitrary R expressions are not evaluated.
#'
#' @param raw_data A data.frame containing source clinical data.
#' @param mapping A mapping specification returned by read_mapping().
#' @param study_id A non-empty study identifier.
#' @param terminology Named list of named character vectors.
#'
#' @return A data.frame containing mapped target variables.
#' @export
apply_mapping <- function(
  raw_data,
  mapping,
  study_id,
  terminology = list()
) {

  if (!is.data.frame(raw_data) || nrow(raw_data) == 0L) {
    stop("raw_data must be a non-empty data.frame.",
         call. = FALSE)
  }

  if (!is.data.frame(mapping) || nrow(mapping) == 0L) {
    stop("mapping must be a non-empty data.frame.",
         call. = FALSE)
  }

  required_columns <- c(
    "domain", "source_variable", "target_variable",
    "transformation", "parameter", "required"
  )

  if (!all(required_columns %in% names(mapping))) {
    stop("Invalid mapping specification.", call. = FALSE)
  }

  if (!is.character(study_id) ||
      length(study_id) != 1L ||
      is.na(study_id) ||
      !nzchar(trimws(study_id))) {
    stop("study_id must be a non-empty string.",
         call. = FALSE)
  }

  if (!is.list(terminology)) {
    stop("terminology must be a named list.",
         call. = FALSE)
  }

  supported <- c(
    "direct", "numeric", "constant",
    "study_id", "terminology_map"
  )

  if (anyNA(mapping$transformation) ||
      any(!mapping$transformation %in% supported)) {
    stop("Unsupported mapping transformation detected.",
         call. = FALSE)
  }

  if (anyNA(mapping$target_variable) ||
      any(!nzchar(mapping$target_variable)) ||
      anyDuplicated(mapping$target_variable)) {
    stop("Target variables must be unique and non-empty.",
         call. = FALSE)
  }

  if (!is.logical(mapping$required) ||
      anyNA(mapping$required)) {
    stop("mapping$required must contain logical values.",
         call. = FALSE)
  }

  if (length(unique(mapping$domain)) != 1L ||
      anyNA(mapping$domain)) {
    stop("Mapping must contain exactly one domain.",
         call. = FALSE)
  }

  output <- data.frame(
    row.names = seq_len(nrow(raw_data))
  )

  for (i in seq_len(nrow(mapping))) {

    rule <- mapping[i, , drop = FALSE]

    target <- rule$target_variable
    source <- rule$source_variable
    transformation <- rule$transformation
    parameter <- rule$parameter

    if (transformation == "study_id") {

      values <- rep(study_id, nrow(raw_data))

    } else if (transformation == "constant") {

      if (is.na(parameter) || !nzchar(parameter)) {
        stop("Constant transformation requires a parameter.",
             call. = FALSE)
      }

      values <- rep(parameter, nrow(raw_data))

    } else {

      if (is.na(source) || !nzchar(source) ||
          !source %in% names(raw_data)) {
        stop(
          paste("Source variable not found:", source),
          call. = FALSE
        )
      }

      values <- raw_data[[source]]

      if (transformation == "numeric") {

        original <- as.character(values)

        values <- suppressWarnings(
          as.numeric(original)
        )

        invalid <- !is.na(original) &
          nzchar(trimws(original)) &
          is.na(values)

        if (any(invalid)) {
          stop(
            paste("Numeric conversion failed for:", source),
            call. = FALSE
          )
        }

      } else if (transformation == "terminology_map") {

        if (is.na(parameter) ||
            !parameter %in% names(terminology)) {
          stop(
            paste("Terminology dictionary not found:", parameter),
            call. = FALSE
          )
        }

        dictionary <- terminology[[parameter]]

        if (!is.character(dictionary) ||
            is.null(names(dictionary)) ||
            anyNA(names(dictionary)) ||
            any(!nzchar(names(dictionary))) ||
            anyDuplicated(names(dictionary)) ||
            anyNA(dictionary)) {
          stop("Invalid terminology dictionary.",
               call. = FALSE)
        }

        source_values <- as.character(values)

        unknown <- !is.na(source_values) &
          !source_values %in% names(dictionary)

        if (any(unknown)) {
          stop(
            paste("Unmapped terminology values in:", source),
            call. = FALSE
          )
        }

        values <- unname(dictionary[source_values])
      }
    }

    
    if (
      isTRUE(rule$required) &&
      (
        anyNA(values) ||
        (
          is.character(values) &&
          any(!nzchar(trimws(values)))
        )
      )
    ) {
      stop(
        paste("Required target contains missing values:", target),
        call. = FALSE
      )
    }

    output[[target]] <- values

  }

  output
}
