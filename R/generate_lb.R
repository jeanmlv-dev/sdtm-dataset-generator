
#' Generate an SDTM-like Laboratory Test Results Dataset
#'
#' Transforms synthetic raw laboratory measurements into a
#' standardized LB dataset using the DM domain as reference.
#'
#' Supports both legacy and metadata-driven mapping workflows.
#'
#' Features:
#' - Required-column and source-data validation
#' - Subject-level referential integrity with DM
#' - Metadata-driven variable mapping
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
#' @param mapping_path Optional path to a metadata mapping CSV file.
#'   When NULL, the legacy LB generation logic is used.
#'   When provided, the metadata-driven mapping workflow is enabled.
#'
#' @return A data.frame containing standardized LB records.
#'
#' @export
generate_lb <- function(raw_data, dm, mapping_path = NULL) {

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

  if (length(unique(dm$STUDYID)) != 1L) {
    stop(
      "DM must contain exactly one study.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 2. Validate source identifiers
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

  if (
    !inherits(raw_data$collection_date, "Date") &&
    !is.character(raw_data$collection_date)
  ) {
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

  dm_key <- paste(
    dm$SITEID,
    dm$SUBJID,
    sep = "|"
  )

  raw_key <- paste(
    raw_data$site_id,
    raw_data$subject_id,
    sep = "|"
  )

  if (anyDuplicated(dm_key)) {
    stop(
      "Duplicate subject keys detected in DM.",
      call. = FALSE
    )
  }

  dm_index <- match(
    raw_key,
    dm_key
  )

  if (anyNA(dm_index)) {
    stop(
      "LB subjects not found in DM.",
      call. = FALSE
    )
  }

  study_id <- as.character(
    unique(dm$STUDYID)
  )

  # -------------------------------------------------------
  # 4. Controlled laboratory terminology
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

    values <- suppressWarnings(
      as.numeric(x)
    )

    if (
      anyNA(values) ||
      any(!is.finite(values))
    ) {
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

  expected_units <- test_dictionary$LBSTRESU[
    test_index
  ]

  if (any(raw_data$unit != expected_units)) {
    stop(
      "Unsupported or inconsistent laboratory units.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 6. Validate and parse collection dates
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
  # 7. Select mapping workflow
  # -------------------------------------------------------

  if (is.null(mapping_path)) {

    # -----------------------------------------------------
    # 7.1 Legacy mapping workflow
    # -----------------------------------------------------

    mapped_lb <- data.frame(

      STUDYID = dm$STUDYID[dm_index],

      DOMAIN = rep(
        "LB",
        nrow(raw_data)
      ),

      LBTESTCD = test_dictionary$LBTESTCD[
        test_index
      ],

      LBTEST = test_dictionary$LBTEST[
        test_index
      ],

      LBCAT = test_dictionary$LBCAT[
        test_index
      ],

      LBSPEC = test_dictionary$LBSPEC[
        test_index
      ],

      VISIT = raw_data$visit,

      VISITNUM = raw_data$visitnum,

      LBORRES = as.character(
        raw_data$result
      ),

      LBORRESU = raw_data$unit,

      LBORNRLO = lower_numeric,

      LBORNRHI = upper_numeric,

      LBDTC = format(
        collection_dates,
        "%Y-%m-%d"
      ),

      stringsAsFactors = FALSE
    )

  } else {

    # -----------------------------------------------------
    # 7.2 Metadata-driven mapping workflow
    # -----------------------------------------------------

    mapping <- read_mapping(
      mapping_path
    )

    if (!all(mapping$domain == "LB")) {
      stop(
        "Mapping specification must contain only LB rules.",
        call. = FALSE
      )
    }

    required_targets <- c(
      "STUDYID",
      "DOMAIN",
      "LBTESTCD",
      "LBTEST",
      "LBCAT",
      "LBSPEC",
      "VISIT",
      "VISITNUM",
      "LBORRES",
      "LBORRESU",
      "LBORNRLO",
      "LBORNRHI",
      "LBDTC"
    )

    missing_targets <- setdiff(
      required_targets,
      mapping$target_variable
    )

    if (length(missing_targets) > 0L) {
      stop(
        paste(
          "Missing LB mapping targets:",
          paste(missing_targets, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    unexpected_targets <- setdiff(
      mapping$target_variable,
      required_targets
    )

    if (length(unexpected_targets) > 0L) {
      stop(
        paste(
          "Unsupported LB mapping targets:",
          paste(unexpected_targets, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    # -----------------------------------------------------
    # 7.3 Define terminology dictionaries
    # -----------------------------------------------------

    terminology <- list(

      lb_test = stats::setNames(
        test_dictionary$LBTEST,
        test_dictionary$LBTESTCD
      ),

      lb_category = stats::setNames(
        test_dictionary$LBCAT,
        test_dictionary$LBTESTCD
      ),

      lb_specimen = stats::setNames(
        test_dictionary$LBSPEC,
        test_dictionary$LBTESTCD
      )
    )

    # -----------------------------------------------------
    # 7.4 Prepare normalized mapping source
    # -----------------------------------------------------

    mapping_data <- raw_data

    mapping_data$result <- as.character(
      raw_data$result
    )

    mapping_data$lower_limit <- lower_numeric

    mapping_data$upper_limit <- upper_numeric

    mapping_data$collection_date <- format(
      collection_dates,
      "%Y-%m-%d"
    )

    # -----------------------------------------------------
    # 7.5 Validate mapping specification
    # -----------------------------------------------------

    validate_mapping(
      mapping = mapping,
      raw_data = mapping_data,
      terminology = terminology
    )

    # -----------------------------------------------------
    # 7.6 Execute metadata mapping engine
    # -----------------------------------------------------

    mapped_lb <- apply_mapping(
      raw_data = mapping_data,
      mapping = mapping,
      study_id = study_id,
      terminology = terminology
    )

    # -----------------------------------------------------
    # 7.7 Validate mapped structure
    # -----------------------------------------------------

    if (nrow(mapped_lb) != nrow(raw_data)) {
      stop(
        "Mapped LB record count does not match source data.",
        call. = FALSE
      )
    }

    if (
      anyNA(mapped_lb$STUDYID) ||
      anyNA(mapped_lb$DOMAIN) ||
      any(mapped_lb$STUDYID != study_id) ||
      any(mapped_lb$DOMAIN != "LB")
    ) {
      stop(
        "Mapped LB study or domain identifiers are inconsistent.",
        call. = FALSE
      )
    }

    mapped_lb <- as.data.frame(
      mapped_lb,
      stringsAsFactors = FALSE
    )

    mapped_lb <- mapped_lb[
      ,
      required_targets,
      drop = FALSE
    ]
  }

  # -------------------------------------------------------
  # 8. Validate mapped clinical fields
  # -------------------------------------------------------

  for (field in c(
    "LBTESTCD",
    "LBTEST",
    "LBCAT",
    "LBSPEC",
    "VISIT",
    "LBORRES",
    "LBORRESU",
    "LBDTC"
  )) {

    mapped_lb[[field]] <- as.character(
      mapped_lb[[field]]
    )

    check_text(
      mapped_lb[[field]],
      field
    )
  }

  mapped_test_index <- match(
    mapped_lb$LBTESTCD,
    test_dictionary$LBTESTCD
  )

  if (anyNA(mapped_test_index)) {
    stop(
      "Unsupported mapped laboratory test codes.",
      call. = FALSE
    )
  }

  if (
    any(
      mapped_lb$LBTEST !=
        test_dictionary$LBTEST[mapped_test_index]
    ) ||
    any(
      mapped_lb$LBCAT !=
        test_dictionary$LBCAT[mapped_test_index]
    ) ||
    any(
      mapped_lb$LBSPEC !=
        test_dictionary$LBSPEC[mapped_test_index]
    )
  ) {
    stop(
      "Mapped laboratory terminology is inconsistent.",
      call. = FALSE
    )
  }

  mapped_units <- test_dictionary$LBSTRESU[
    mapped_test_index
  ]

  if (any(mapped_lb$LBORRESU != mapped_units)) {
    stop(
      "Unsupported or inconsistent mapped laboratory units.",
      call. = FALSE
    )
  }

  mapped_visitnum <- parse_numeric(
    mapped_lb$VISITNUM,
    "VISITNUM"
  )

  if (any(mapped_visitnum < 0)) {
    stop(
      "Mapped VISITNUM must be non-negative.",
      call. = FALSE
    )
  }

  # Preserve the original numeric storage type when possible.
  if (
    is.integer(raw_data$visitnum) &&
    all(mapped_visitnum == trunc(mapped_visitnum)) &&
    all(mapped_visitnum <= .Machine$integer.max)
  ) {
    mapped_visitnum <- as.integer(mapped_visitnum)
  }

  # -------------------------------------------------------
  # 9. Standardize mapped results and reference ranges
  # -------------------------------------------------------

  mapped_result <- parse_numeric(
    mapped_lb$LBORRES,
    "LBORRES"
  )

  mapped_lower <- parse_numeric(
    mapped_lb$LBORNRLO,
    "LBORNRLO"
  )

  mapped_upper <- parse_numeric(
    mapped_lb$LBORNRHI,
    "LBORNRHI"
  )

  if (any(mapped_lower > mapped_upper)) {
    stop(
      "Mapped lower reference limits exceed upper limits.",
      call. = FALSE
    )
  }

  # All supported synthetic laboratory units already
  # correspond to their standardized units.
  # No unit conversion is required.

  standardized_numeric <- mapped_result

  standardized_lower <- mapped_lower

  standardized_upper <- mapped_upper

  # -------------------------------------------------------
  # 10. Derive normal-range indicators
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
  # 11. Validate mapped collection dates
  # -------------------------------------------------------

  mapped_dates <- suppressWarnings(
    as.Date(
      mapped_lb$LBDTC,
      format = "%Y-%m-%d"
    )
  )

  valid_mapped_dates <-
    grepl(
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}$",
      mapped_lb$LBDTC
    ) &
    !is.na(mapped_dates) &
    format(
      mapped_dates,
      "%Y-%m-%d"
    ) == mapped_lb$LBDTC

  valid_mapped_dates[
    is.na(valid_mapped_dates)
  ] <- FALSE

  if (!all(valid_mapped_dates)) {
    stop(
      "Mapped LBDTC must contain valid YYYY-MM-DD dates.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 12. Derive study days relative to DM.RFSTDTC
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

  has_reference <- !is.na(
    reference_dates
  )

  differences <- as.integer(

    mapped_dates[has_reference] -
      reference_dates[has_reference]
  )

  study_days[has_reference] <- ifelse(

    differences >= 0L,

    differences + 1L,

    differences
  )

  # -------------------------------------------------------
  # 13. Assemble final LB dataset
  # -------------------------------------------------------

  lb <- data.frame(

    STUDYID = mapped_lb$STUDYID,

    DOMAIN = mapped_lb$DOMAIN,

    USUBJID = dm$USUBJID[dm_index],

    LBSEQ = rep(
      NA_integer_,
      nrow(raw_data)
    ),

    LBTESTCD = mapped_lb$LBTESTCD,

    LBTEST = mapped_lb$LBTEST,

    LBCAT = mapped_lb$LBCAT,

    LBSPEC = mapped_lb$LBSPEC,

    VISIT = mapped_lb$VISIT,

    VISITNUM = mapped_visitnum,

    LBORRES = mapped_lb$LBORRES,

    LBORRESU = mapped_lb$LBORRESU,

    LBSTRESC = as.character(
      standardized_numeric
    ),

    LBSTRESN = standardized_numeric,

    LBSTRESU = mapped_units,

    LBORNRLO = mapped_lower,

    LBORNRHI = mapped_upper,

    LBSTNRLO = standardized_lower,

    LBSTNRHI = standardized_upper,

    LBNRIND = normal_indicator,

    LBDTC = format(
      mapped_dates,
      "%Y-%m-%d"
    ),

    LBDY = study_days,

    stringsAsFactors = FALSE
  )

  # -------------------------------------------------------
  # 14. Deterministic record ordering and LBSEQ
  # -------------------------------------------------------

  ordering <- order(

    lb$USUBJID,

    lb$VISITNUM,

    lb$LBTESTCD,

    raw_data$lab_id
  )

  lb <- lb[
    ordering,
    ,
    drop = FALSE
  ]

  lb$LBSEQ <- as.integer(
    sequence(
      rle(lb$USUBJID)$lengths
    )
  )

  rownames(lb) <- NULL

  # -------------------------------------------------------
  # 15. Final structural checks
  # -------------------------------------------------------

  if (nrow(lb) != nrow(raw_data)) {
    stop(
      "Raw and LB record counts do not match.",
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

  if (!all(lb$USUBJID %in% dm$USUBJID)) {
    stop(
      "LB referential integrity check failed.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 16. Return generated LB dataset
  # -------------------------------------------------------

  lb
}