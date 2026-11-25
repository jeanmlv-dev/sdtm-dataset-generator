
# ============================================================
# Metadata-Driven AE Integration Tests
# ============================================================
#
# Purpose:
# Validate the integration between the metadata-driven mapping
# engine and the SDTM-like Adverse Events (AE) domain.
#
# Test coverage:
# - Backward compatibility
# - Metadata-driven generation
# - Legacy and metadata output equivalence
# - Dataset structure and data types
# - Referential integrity
# - Primary key uniqueness
# - Controlled terminology
# - Temporal consistency
# - Invalid metadata handling
# - Source data immutability
# - Reproducibility
#
# ============================================================


# ------------------------------------------------------------
# 1. Test fixtures
# ------------------------------------------------------------

ae_metadata_file <- function(...) {
  path <- system.file(
    ...,
    package = "sdtmgenerator",
    mustWork = TRUE
  )

  path
}


make_ae_metadata_fixture <- function() {
  study_id <- "STUDY001"

  raw_dm <- readr::read_csv(
    file = ae_metadata_file(
      "extdata",
      "raw_dm.csv"
    ),
    col_types = readr::cols(
      subject_id = readr::col_character(),
      site_id = readr::col_character(),
      .default = readr::col_guess()
    ),
    show_col_types = FALSE
  )

  raw_ae <- readr::read_csv(
    file = ae_metadata_file(
      "extdata",
      "raw_ae.csv"
    ),
    col_types = readr::cols(
      subject_id = readr::col_character(),
      site_id = readr::col_character(),
      event_id = readr::col_character(),
      start_date = readr::col_character(),
      end_date = readr::col_character(),
      .default = readr::col_guess()
    ),
    show_col_types = FALSE
  )

  dm <- generate_dm(
    raw_data = raw_dm,
    study_id = study_id
  )

  mapping_path <- ae_metadata_file(
    "mappings",
    "ae_mapping.csv"
  )

  list(
    study_id = study_id,
    raw_dm = raw_dm,
    raw_ae = raw_ae,
    dm = dm,
    mapping_path = mapping_path
  )
}


write_ae_metadata_fixture <- function(mapping) {
  path <- tempfile(
    pattern = "ae_mapping_",
    fileext = ".csv"
  )

  readr::write_csv(
    mapping,
    file = path,
    na = ""
  )

  path
}


# ------------------------------------------------------------
# 2. Backward compatibility
# ------------------------------------------------------------

test_that("AE legacy generation remains supported", {
  fixture <- make_ae_metadata_fixture()

  ae <- generate_ae(
    raw_data = fixture$raw_ae,
    dm = fixture$dm
  )

  expect_s3_class(ae, "data.frame")

  expect_gt(nrow(ae), 0L)

  expect_equal(
    nrow(ae),
    nrow(fixture$raw_ae)
  )

  expect_true(
    validate_ae(ae, fixture$dm)
  )
})


test_that("AE NULL mapping preserves legacy behavior", {
  fixture <- make_ae_metadata_fixture()

  ae_default <- generate_ae(
    raw_data = fixture$raw_ae,
    dm = fixture$dm
  )

  ae_null <- generate_ae(
    raw_data = fixture$raw_ae,
    dm = fixture$dm,
    mapping_path = NULL
  )

  expect_identical(
    ae_default,
    ae_null
  )
})


# ------------------------------------------------------------
# 3. Metadata-driven generation
# ------------------------------------------------------------

test_that("AE supports metadata-driven generation", {
  fixture <- make_ae_metadata_fixture()

  ae <- generate_ae(
    raw_data = fixture$raw_ae,
    dm = fixture$dm,
    mapping_path = fixture$mapping_path
  )

  expect_s3_class(ae, "data.frame")

  expect_equal(
    nrow(ae),
    nrow(fixture$raw_ae)
  )

  expect_true(
    all(ae$DOMAIN == "AE")
  )

  expect_true(
    all(ae$STUDYID == fixture$study_id)
  )

  expect_true(
    validate_ae(ae, fixture$dm)
  )
})


# ------------------------------------------------------------
# 4. Legacy and metadata output equivalence
# ------------------------------------------------------------

test_that("AE metadata output matches legacy output", {
  fixture <- make_ae_metadata_fixture()

  ae_legacy <- generate_ae(
    raw_data = fixture$raw_ae,
    dm = fixture$dm
  )

  ae_metadata <- generate_ae(
    raw_data = fixture$raw_ae,
    dm = fixture$dm,
    mapping_path = fixture$mapping_path
  )

  expect_identical(
    ae_metadata,
    ae_legacy
  )
})


test_that("AE metadata preserves variable order", {
  fixture <- make_ae_metadata_fixture()

  ae_legacy <- generate_ae(
    fixture$raw_ae,
    fixture$dm
  )

  ae_metadata <- generate_ae(
    fixture$raw_ae,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  expect_identical(
    names(ae_metadata),
    names(ae_legacy)
  )
})


test_that("AE metadata preserves dataset dimensions", {
  fixture <- make_ae_metadata_fixture()

  ae_legacy <- generate_ae(
    fixture$raw_ae,
    fixture$dm
  )

  ae_metadata <- generate_ae(
    fixture$raw_ae,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  expect_identical(
    dim(ae_metadata),
    dim(ae_legacy)
  )
})


# ------------------------------------------------------------
# 5. Dataset structure and data types
# ------------------------------------------------------------

test_that("AE metadata preserves expected data types", {
  fixture <- make_ae_metadata_fixture()

  ae <- generate_ae(
    fixture$raw_ae,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  expect_type(ae$STUDYID, "character")
  expect_type(ae$DOMAIN, "character")
  expect_type(ae$USUBJID, "character")
  expect_type(ae$AETERM, "character")

  expect_true(is.numeric(ae$AESEQ))

  expect_true(
    all(is.finite(ae$AESEQ))
  )

  expect_true(
    all(ae$AESEQ > 0)
  )

  expect_true(
    all(ae$AESEQ == floor(ae$AESEQ))
  )
})


# ------------------------------------------------------------
# 6. Referential integrity
# ------------------------------------------------------------

test_that("AE metadata preserves DM referential integrity", {
  fixture <- make_ae_metadata_fixture()

  ae <- generate_ae(
    fixture$raw_ae,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  expect_true(
    all(ae$USUBJID %in% fixture$dm$USUBJID)
  )

  dm_index <- match(
    ae$USUBJID,
    fixture$dm$USUBJID
  )

  expect_false(anyNA(dm_index))

  expect_identical(
    ae$STUDYID,
    fixture$dm$STUDYID[dm_index]
  )
})


test_that("AE metadata rejects subjects absent from DM", {
  fixture <- make_ae_metadata_fixture()

  invalid_data <- fixture$raw_ae

  invalid_data$subject_id[1] <- "999999"

  expect_error(
    generate_ae(
      raw_data = invalid_data,
      dm = fixture$dm,
      mapping_path = fixture$mapping_path
    )
  )
})


# ------------------------------------------------------------
# 7. Primary key integrity
# ------------------------------------------------------------

test_that("AE metadata preserves unique primary keys", {
  fixture <- make_ae_metadata_fixture()

  ae <- generate_ae(
    fixture$raw_ae,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  primary_keys <- ae[
    c("USUBJID", "AESEQ")
  ]

  expect_equal(
    anyDuplicated(primary_keys),
    0L
  )
})


test_that("AE metadata preserves subject-level sequencing", {
  fixture <- make_ae_metadata_fixture()

  ae <- generate_ae(
    fixture$raw_ae,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  subject_sequences <- split(
    ae$AESEQ,
    ae$USUBJID
  )

  for (sequence in subject_sequences) {
    expect_equal(
      sort(as.numeric(sequence)),
      as.numeric(seq_along(sequence))
    )
  }
})


# ------------------------------------------------------------
# 8. Controlled terminology
# ------------------------------------------------------------

test_that("AE metadata preserves controlled terminology", {
  fixture <- make_ae_metadata_fixture()

  ae_legacy <- generate_ae(
    fixture$raw_ae,
    fixture$dm
  )

  ae_metadata <- generate_ae(
    fixture$raw_ae,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  terminology_variables <- c(
    "AESEV",
    "AESER",
    "AEREL",
    "AEOUT"
  )

  expect_true(
    all(terminology_variables %in% names(ae_metadata))
  )

  for (variable in terminology_variables) {
    expect_identical(
      ae_metadata[[variable]],
      ae_legacy[[variable]]
    )
  }
})


test_that("AE metadata rejects invalid severity values", {
  fixture <- make_ae_metadata_fixture()

  invalid_data <- fixture$raw_ae

  invalid_data$severity[1] <- "INVALID_SEVERITY"

  expect_error(
    generate_ae(
      raw_data = invalid_data,
      dm = fixture$dm,
      mapping_path = fixture$mapping_path
    )
  )
})


# ------------------------------------------------------------
# 9. Temporal consistency
# ------------------------------------------------------------

test_that("AE metadata preserves event dates and study days", {
  fixture <- make_ae_metadata_fixture()

  ae_legacy <- generate_ae(
    fixture$raw_ae,
    fixture$dm
  )

  ae_metadata <- generate_ae(
    fixture$raw_ae,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  temporal_variables <- c(
    "AESTDTC",
    "AEENDTC",
    "AESTDY",
    "AEENDY"
  )

  expect_true(
    all(temporal_variables %in% names(ae_metadata))
  )

  for (variable in temporal_variables) {
    expect_identical(
      ae_metadata[[variable]],
      ae_legacy[[variable]]
    )
  }
})


test_that("AE metadata rejects invalid event dates", {
  fixture <- make_ae_metadata_fixture()

  invalid_data <- fixture$raw_ae

  invalid_data$start_date[1] <- "2026-02-30"

  expect_error(
    generate_ae(
      raw_data = invalid_data,
      dm = fixture$dm,
      mapping_path = fixture$mapping_path
    )
  )
})


# ------------------------------------------------------------
# 10. Metadata validation and error handling
# ------------------------------------------------------------

test_that("AE metadata rejects nonexistent mapping files", {
  fixture <- make_ae_metadata_fixture()

  invalid_path <- tempfile(
    pattern = "missing_ae_mapping_",
    fileext = ".csv"
  )

  expect_error(
    generate_ae(
      raw_data = fixture$raw_ae,
      dm = fixture$dm,
      mapping_path = invalid_path
    )
  )
})


test_that("AE metadata rejects missing mapping columns", {
  fixture <- make_ae_metadata_fixture()

  mapping <- read_mapping(
    fixture$mapping_path
  )

  mapping$target_variable <- NULL

  mapping_path <- write_ae_metadata_fixture(
    mapping
  )

  on.exit(
    unlink(mapping_path),
    add = TRUE
  )

  expect_error(
    generate_ae(
      raw_data = fixture$raw_ae,
      dm = fixture$dm,
      mapping_path = mapping_path
    )
  )
})


test_that("AE metadata rejects duplicate mapping targets", {
  fixture <- make_ae_metadata_fixture()

  mapping <- read_mapping(
    fixture$mapping_path
  )

  invalid_mapping <- rbind(
    mapping,
    mapping[1, , drop = FALSE]
  )

  mapping_path <- write_ae_metadata_fixture(
    invalid_mapping
  )

  on.exit(
    unlink(mapping_path),
    add = TRUE
  )

  expect_error(
    generate_ae(
      raw_data = fixture$raw_ae,
      dm = fixture$dm,
      mapping_path = mapping_path
    )
  )
})


test_that("AE metadata rejects missing required targets", {
  fixture <- make_ae_metadata_fixture()

  mapping <- read_mapping(
    fixture$mapping_path
  )

  expect_true(
    "AETERM" %in% mapping$target_variable
  )

  invalid_mapping <- mapping[
    mapping$target_variable != "AETERM",
    ,
    drop = FALSE
  ]

  mapping_path <- write_ae_metadata_fixture(
    invalid_mapping
  )

  on.exit(
    unlink(mapping_path),
    add = TRUE
  )

  expect_error(
    generate_ae(
      raw_data = fixture$raw_ae,
      dm = fixture$dm,
      mapping_path = mapping_path
    )
  )
})


test_that("AE metadata rejects unsupported transformations", {
  fixture <- make_ae_metadata_fixture()

  mapping <- read_mapping(
    fixture$mapping_path
  )

  mapping$transformation[1] <- "unsupported_transform"

  mapping_path <- write_ae_metadata_fixture(
    mapping
  )

  on.exit(
    unlink(mapping_path),
    add = TRUE
  )

  expect_error(
    generate_ae(
      raw_data = fixture$raw_ae,
      dm = fixture$dm,
      mapping_path = mapping_path
    )
  )
})


test_that("AE metadata rejects missing source variables", {
  fixture <- make_ae_metadata_fixture()

  invalid_data <- fixture$raw_ae

  invalid_data$event_term <- NULL

  expect_error(
    generate_ae(
      raw_data = invalid_data,
      dm = fixture$dm,
      mapping_path = fixture$mapping_path
    )
  )
})


# ------------------------------------------------------------
# 11. Source data immutability
# ------------------------------------------------------------

test_that("AE metadata does not modify source datasets", {
  fixture <- make_ae_metadata_fixture()

  original_ae <- fixture$raw_ae

  original_dm <- fixture$dm

  generate_ae(
    raw_data = fixture$raw_ae,
    dm = fixture$dm,
    mapping_path = fixture$mapping_path
  )

  expect_identical(
    fixture$raw_ae,
    original_ae
  )

  expect_identical(
    fixture$dm,
    original_dm
  )
})


# ------------------------------------------------------------
# 12. Reproducibility
# ------------------------------------------------------------

test_that("AE metadata generation is reproducible", {
  fixture <- make_ae_metadata_fixture()

  ae_first <- generate_ae(
    fixture$raw_ae,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  ae_second <- generate_ae(
    fixture$raw_ae,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  expect_identical(
    ae_first,
    ae_second
  )
})


# ============================================================
# Metadata-driven behavior
# ============================================================

test_that("AE metadata rules control the generated output", {

  fixture <- make_ae_metadata_fixture()

  original_mapping_path <- system.file(
    "mappings",
    "ae_mapping.csv",
    package = "sdtmgenerator",
    mustWork = TRUE
  )

  mapping <- read_mapping(original_mapping_path)

  # Generate the reference dataset using the original metadata.
  reference_ae <- generate_ae(
    raw_data = fixture$raw_ae,
    dm = fixture$dm,
    mapping_path = original_mapping_path
  )

  # Modify a valid declarative mapping rule.
  # This is an artificial test rule, not a clinical mapping
  # recommendation.
  target_index <- which(
    mapping$target_variable == "AETERM"
  )

  expect_length(target_index, 1L)

  mapping$transformation[target_index] <- "constant"
  mapping$source_variable[target_index] <- NA_character_
  mapping$parameter[target_index] <- "METADATA TEST TERM"

  modified_mapping_path <- tempfile(
    fileext = ".csv"
  )

  on.exit(
    unlink(modified_mapping_path),
    add = TRUE
  )

  readr::write_csv(
    mapping,
    modified_mapping_path
  )

  # Generate AE using the modified metadata specification.
  modified_ae <- generate_ae(
    raw_data = fixture$raw_ae,
    dm = fixture$dm,
    mapping_path = modified_mapping_path
  )

  # Confirm that the metadata rule changed the target variable.
  expect_true(
    all(modified_ae$AETERM == "METADATA TEST TERM")
  )

  expect_false(
    identical(
      reference_ae$AETERM,
      modified_ae$AETERM
    )
  )

  # Record counts and dataset structure must remain unchanged.
  expect_equal(
    nrow(modified_ae),
    nrow(reference_ae)
  )

  expect_identical(
    names(modified_ae),
    names(reference_ae)
  )

  # All other variables must remain unchanged.
  unchanged_columns <- setdiff(
    names(reference_ae),
    "AETERM"
  )

  expect_identical(
    modified_ae[unchanged_columns],
    reference_ae[unchanged_columns]
  )
})

# ============================================================
# End of Metadata-Driven AE Integration Tests
# ============================================================
