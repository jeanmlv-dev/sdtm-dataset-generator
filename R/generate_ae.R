
#' Generate an SDTM-like Adverse Events (AE) Dataset
#'
#' Transforms synthetic raw adverse event records into a
#' standardized subset of CDISC SDTM AE variables using
#' a Demographics (DM) dataset as a subject reference.
#'
#' @param raw_data A data.frame containing raw adverse events.
#' @param dm A data.frame containing the generated DM dataset.
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
generate_ae <- function(raw_data, dm) {

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
  # 5. Apply explicit controlled value mappings
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

  # -------------------------------------------------------
  # 6. Derive study days
  # -------------------------------------------------------

  aestdy <- derive_study_day(
    start_date,
    reference_date
  )

  aeendy <- derive_study_day(
    end_date,
    reference_date
  )

  # -------------------------------------------------------
  # 7. Build AE dataset
  # -------------------------------------------------------

  ae <- tibble::tibble(
    STUDYID = study_id,
    DOMAIN = rep("AE", nrow(raw_data)),
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
