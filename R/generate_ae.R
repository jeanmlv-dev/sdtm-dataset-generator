
#' Generate an SDTM-like Adverse Events (AE) Dataset
#'
#' Transforms synthetic raw adverse event records into a
#' standardized subset of CDISC SDTM AE variables using
#' a Demographics (DM) dataset as a subject reference.
#'
#' @param raw_data A data.frame containing raw adverse events.
#' @param dm A data.frame containing the generated DM dataset.
#' @param mapping_path Optional path to a metadata mapping CSV file.
#'   When NULL, the legacy AE generation logic is used.
#'   When provided, the metadata-driven mapping workflow is enabled.
#' 
#' @return A tibble containing mapped AE variables.
#'
#' @details
#' Required raw columns:
#' subject_id, site_id, event_id, event_term, severity,
#' serious, relationship, outcome, start_date, end_date.
#'
#' Each event must reference an existing DM participant.
#' Event identifiers must be unique within each subject.
#'
#' AESEQ is assigned within each subject after sorting by
#' event start date and source event identifier.
#'
#' Study days are derived relative to DM.RFSTDTC.
#' Complete ISO 8601 calendar dates are supported.
#'
#' Medical coding uses a synthetic demonstration dictionary.
#' This function does not establish full SDTM compliance
#' and does not perform official MedDRA coding.
#'
#' @export
generate_ae <- function(raw_data, dm, mapping_path = NULL) {

  # -------------------------------------------------------
  # 1. Validate input structure
  # -------------------------------------------------------

  required_raw <- c(
    "subject_id",
    "site_id",
    "event_id",
    "event_term",
    "severity",
    "serious",
    "relationship",
    "outcome",
    "start_date",
    "end_date"
  )

  check_required_columns(
    raw_data,
    required_raw,
    "Raw AE"
  )

  validate_dm(dm)

  required_dm <- c(
    "STUDYID",
    "USUBJID",
    "SUBJID",
    "SITEID",
    "RFSTDTC"
  )

  check_required_columns(
    dm,
    required_dm,
    "DM reference"
  )

  if (length(unique(dm$STUDYID)) != 1L) {
    stop(
      "DM must contain exactly one study.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 2. Validate source identifiers
  # -------------------------------------------------------

  subject_id <- as.character(raw_data$subject_id)
  site_id <- as.character(raw_data$site_id)
  event_id <- as.character(raw_data$event_id)

  check_required_values(subject_id, "subject_id")
  check_required_values(site_id, "site_id")
  check_required_values(event_id, "event_id")

  check_required_values(dm$SUBJID, "DM.SUBJID")
  check_required_values(dm$SITEID, "DM.SITEID")

  if (anyDuplicated(
    data.frame(
      SITEID = dm$SITEID,
      SUBJID = dm$SUBJID
    )
  )) {
    stop(
      "Duplicate subject keys detected in DM.",
      call. = FALSE
    )
  }

  if (anyDuplicated(
    data.frame(
      site_id = site_id,
      subject_id = subject_id,
      event_id = event_id
    )
  )) {
    stop(
      "Duplicate event_id values within a subject.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------
  # 3. Link adverse events to DM
  # -------------------------------------------------------

  dm_key <- paste(
    dm$SITEID,
    dm$SUBJID,
    sep = "|"
  )

  ae_key <- paste(
    site_id,
    subject_id,
    sep = "|"
  )

  dm_index <- match(ae_key, dm_key)

  if (anyNA(dm_index)) {

    unmatched <- unique(ae_key[is.na(dm_index)])

    stop(
      paste0(
        "AE subjects not found in DM: ",
        paste(unmatched, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  study_id <- as.character(dm$STUDYID[dm_index])
  usubjid <- as.character(dm$USUBJID[dm_index])

  reference_date <- parse_iso_date(
    dm$RFSTDTC[dm_index],
    "DM.RFSTDTC"
  )

  # -------------------------------------------------------
  # 4. Validate event dates
  # -------------------------------------------------------

  start_date <- parse_iso_date(
    raw_data$start_date,
    "start_date",
    allow_missing = FALSE
  )

  end_date <- parse_iso_date(
    raw_data$end_date,
    "end_date",
    allow_missing = TRUE
  )

  invalid_dates <- !is.na(end_date) &
    end_date < start_date

  if (any(invalid_dates)) {
    stop(
      "Event end date must not precede start date.",
      call. = FALSE
    )
  }

 
  # -------------------------------------------------------
  # 5. Define controlled terminology dictionaries
  # -------------------------------------------------------

  severity_lookup <- c(
    "Mild" = "MILD",
    "Moderate" = "MODERATE",
    "Severe" = "SEVERE"
  )

  serious_lookup <- c(
    "Yes" = "Y",
    "No" = "N"
  )

  relationship_lookup <- c(
    "Related" = "RELATED",
    "Not Related" = "NOT RELATED"
  )

  outcome_lookup <- c(
    "Recovered" = "RECOVERED/RESOLVED",
    "Recovering" = "RECOVERING/RESOLVING",
    "Not Recovered" = "NOT RECOVERED/NOT RESOLVED"
  )

  # Synthetic coding dictionary.
  # Not an official MedDRA implementation.

  term_lookup <- c(
    "Headache" = "HEADACHE",
    "Nausea" = "NAUSEA",
    "Fatigue" = "FATIGUE",
    "Abdominal pain" = "ABDOMINAL PAIN",
    "Diarrhea" = "DIARRHOEA",
    "Dizziness" = "DIZZINESS",
    "Pneumonia" = "PNEUMONIA"
  )

  # -------------------------------------------------------
  # 6. Execute legacy or metadata-driven mapping
  # -------------------------------------------------------

  if (is.null(mapping_path)) {

    # -----------------------------------------------------
    # 6.1. Legacy mapping
    # -----------------------------------------------------

    aeterm <- as.character(raw_data$event_term)

    aedecod <- map_controlled_values(
      aeterm,
      term_lookup,
      "event_term"
    )

    aesev <- map_controlled_values(
      raw_data$severity,
      severity_lookup,
      "severity"
    )

    aeser <- map_controlled_values(
      raw_data$serious,
      serious_lookup,
      "serious"
    )

    aerel <- map_controlled_values(
      raw_data$relationship,
      relationship_lookup,
      "relationship"
    )

    aeout <- map_controlled_values(
      raw_data$outcome,
      outcome_lookup,
      "outcome"
    )

    mapped_study_id <- study_id

    mapped_domain <- rep(
      "AE",
      nrow(raw_data)
    )

  } else {

    # -----------------------------------------------------
    # 6.2. Read metadata mapping specification
    # -----------------------------------------------------

    mapping <- read_mapping(mapping_path)

    # Ensure that the mapping specification belongs to AE.

    if (anyNA(mapping$domain) ||
        any(mapping$domain != "AE")) {
      stop(
        "Mapping specification must belong to AE.",
        call. = FALSE
      )
    }

    # -----------------------------------------------------
    # 6.3. Validate required mapping targets
    # -----------------------------------------------------

    required_targets <- c(
      "STUDYID",
      "DOMAIN",
      "AETERM",
      "AEDECOD",
      "AESEV",
      "AESER",
      "AEREL",
      "AEOUT",
      "AESTDTC",
      "AEENDTC"
    )

    missing_targets <- setdiff(
      required_targets,
      mapping$target_variable
    )

    if (length(missing_targets) > 0L) {
      stop(
        paste(
          "Missing AE mapping targets:",
          paste(
            missing_targets,
            collapse = ", "
          )
        ),
        call. = FALSE
      )
    }

    # Reject unsupported targets to avoid silently
    # ignoring metadata rules.

    unsupported_targets <- setdiff(
      mapping$target_variable,
      required_targets
    )

    if (length(unsupported_targets) > 0L) {
      stop(
        paste(
          "Unsupported AE mapping targets:",
          paste(
            unsupported_targets,
            collapse = ", "
          )
        ),
        call. = FALSE
      )
    }

    # -----------------------------------------------------
    # 6.4. Configure terminology dictionaries
    # -----------------------------------------------------

    terminology <- list(
      term = term_lookup,
      severity = severity_lookup,
      serious = serious_lookup,
      relationship = relationship_lookup,
      outcome = outcome_lookup
    )

    # -----------------------------------------------------
    # 6.5. Normalize source data
    # -----------------------------------------------------

    # Preserve the original source dataset.

    mapping_data <- raw_data

    mapping_data$subject_id <- subject_id
    mapping_data$site_id <- site_id
    mapping_data$event_id <- event_id

    mapping_data$event_term <- as.character(
      raw_data$event_term
    )

    mapping_data$severity <- as.character(
      raw_data$severity
    )

    mapping_data$serious <- as.character(
      raw_data$serious
    )

    mapping_data$relationship <- as.character(
      raw_data$relationship
    )

    mapping_data$outcome <- as.character(
      raw_data$outcome
    )

    mapping_data$start_date <- format(
      start_date,
      "%Y-%m-%d"
    )

    mapping_data$end_date <- format(
      end_date,
      "%Y-%m-%d"
    )

    # -----------------------------------------------------
    # 6.6. Validate metadata and source compatibility
    # -----------------------------------------------------

    validate_mapping(
      mapping = mapping,
      raw_data = mapping_data,
      terminology = terminology
    )

    # -----------------------------------------------------
    # 6.7. Execute metadata-driven mapping engine
    # -----------------------------------------------------

    mapped_ae <- apply_mapping(
      raw_data = mapping_data,
      mapping = mapping,
      study_id = as.character(
        unique(dm$STUDYID)
      ),
      terminology = terminology
    )

    # -----------------------------------------------------
    # 6.8. Validate mapped record counts
    # -----------------------------------------------------

    if (nrow(mapped_ae) != nrow(raw_data)) {
      stop(
        "Metadata mapping changed the AE record count.",
        call. = FALSE
      )
    }

    # -----------------------------------------------------
    # 6.9. Validate mapped identifiers
    # -----------------------------------------------------

    mapped_study_id <- as.character(
      mapped_ae$STUDYID
    )

    mapped_domain <- as.character(
      mapped_ae$DOMAIN
    )

    if (anyNA(mapped_study_id) ||
        any(mapped_study_id != study_id)) {
      stop(
        "Mapped STUDYID is inconsistent with DM.",
        call. = FALSE
      )
    }

    if (anyNA(mapped_domain) ||
        any(mapped_domain != "AE")) {
      stop(
        "Mapped DOMAIN must be AE.",
        call. = FALSE
      )
    }

    # -----------------------------------------------------
    # 6.10. Extract mapped AE variables
    # -----------------------------------------------------

    aeterm <- as.character(mapped_ae$AETERM)
    aedecod <- as.character(mapped_ae$AEDECOD)
    aesev <- as.character(mapped_ae$AESEV)
    aeser <- as.character(mapped_ae$AESER)
    aerel <- as.character(mapped_ae$AEREL)
    aeout <- as.character(mapped_ae$AEOUT)

    # -----------------------------------------------------
    # 6.11. Validate mapped terminology
    # -----------------------------------------------------

    check_required_values(aeterm, "AETERM")
    check_required_values(aedecod, "AEDECOD")
    check_required_values(aesev, "AESEV")
    check_required_values(aeser, "AESER")
    check_required_values(aerel, "AEREL")
    check_required_values(aeout, "AEOUT")

    if (any(!aedecod %in% unname(term_lookup))) {
      stop(
        "Unsupported mapped AEDECOD values.",
        call. = FALSE
      )
    }

    if (any(!aesev %in% unname(severity_lookup))) {
      stop(
        "Unsupported mapped AESEV values.",
        call. = FALSE
      )
    }

    if (any(!aeser %in% unname(serious_lookup))) {
      stop(
        "Unsupported mapped AESER values.",
        call. = FALSE
      )
    }

    if (any(!aerel %in% unname(relationship_lookup))) {
      stop(
        "Unsupported mapped AEREL values.",
        call. = FALSE
      )
    }

    if (any(!aeout %in% unname(outcome_lookup))) {
      stop(
        "Unsupported mapped AEOUT values.",
        call. = FALSE
      )
    }

    # -----------------------------------------------------
    # 6.12. Validate mapped event dates
    # -----------------------------------------------------

    start_date <- parse_iso_date(
      mapped_ae$AESTDTC,
      "AESTDTC",
      allow_missing = FALSE
    )

    end_date <- parse_iso_date(
      mapped_ae$AEENDTC,
      "AEENDTC",
      allow_missing = TRUE
    )

    invalid_dates <- !is.na(end_date) &
      end_date < start_date

    if (any(invalid_dates)) {
      stop(
        "Mapped event end date must not precede start date.",
        call. = FALSE
      )
    }
  }

  # -------------------------------------------------------
  # 7. Derive clinical variables and build AE dataset
  # -------------------------------------------------------

  aestdy <- derive_study_day(
    start_date,
    reference_date
  )

  aeendy <- derive_study_day(
    end_date,
    reference_date
  )

  ae <- tibble::tibble(
    STUDYID = mapped_study_id,
    DOMAIN = mapped_domain,
    USUBJID = usubjid,
    AESEQ = rep(NA_integer_, nrow(raw_data)),
    AETERM = aeterm,
    AEDECOD = aedecod,
    AESEV = aesev,
    AESER = aeser,
    AEREL = aerel,
    AEOUT = aeout,
    AESTDTC = format(start_date, "%Y-%m-%d"),
    AEENDTC = format(end_date, "%Y-%m-%d"),
    AESTDY = aestdy,
    AEENDY = aeendy
  )

  # -------------------------------------------------------
  # 8. Sort and derive deterministic AESEQ
  # -------------------------------------------------------

  sort_index <- order(
    ae$STUDYID,
    ae$USUBJID,
    ae$AESTDTC,
    event_id,
    method = "radix"
  )

  ae <- ae[sort_index, , drop = FALSE]

  if (nrow(ae) > 0L) {

    subject_group <- interaction(
      ae$STUDYID,
      ae$USUBJID,
      drop = TRUE
    )

    ae$AESEQ <- as.integer(
      stats::ave(
        seq_len(nrow(ae)),
        subject_group,
        FUN = seq_along
      )
    )
  }

  # -------------------------------------------------------
  # 9. Final integrity checks
  # -------------------------------------------------------

  if (nrow(ae) != nrow(raw_data)) {
    stop(
      "Raw and AE record counts do not match.",
      call. = FALSE
    )
  }

  if (anyDuplicated(
    ae[c("STUDYID", "USUBJID", "AESEQ")]
  )) {
    stop(
      "Duplicate AE primary keys detected.",
      call. = FALSE
    )
  }

  ae
}
