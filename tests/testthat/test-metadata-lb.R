# ============================================================
# Metadata-Driven LB Integration Tests
# ============================================================
#
# Project: sdtm-dataset-generator
# Domain: Laboratory Test Results (LB)
#
# Purpose:
#   Validate the integration between the metadata-driven
#   mapping engine and the LB domain generator.
#
# Coverage:
#   - Legacy workflow compatibility
#   - Metadata-driven workflow equivalence
#   - Dataset structure and record preservation
#   - Subject-level referential integrity
#   - Controlled laboratory terminology
#   - Original and standardized results
#   - Laboratory reference ranges
#   - Normal-range indicators
#   - Reference-range boundary conditions
#   - Study-day derivation
#   - Deterministic sequencing
#   - Invalid metadata rejection
#   - Source data immutability
#   - Metadata-driven output behavior
#
# All clinical data used in these tests are synthetic.
#
# ============================================================


# ------------------------------------------------------------
# 1. Test fixture
# ------------------------------------------------------------

make_metadata_lb_fixture <- function() {

  study_id <- "STUDY001"

  # Synthetic demographics source data.

  raw_dm <- data.frame(
    subject_id = c("001", "002", "003"),
    site_id = c("101", "101", "102"),
    age = c(35, 48, 52),
    sex = c("M", "F", "M"),
    arm = c("Placebo", "Active", "Active"),
    rfstdtc = c(
      "2026-01-01",
      "2026-01-02",
      "2026-01-03"
    ),
    stringsAsFactors = FALSE
  )

  dm <- generate_dm(
    raw_data = raw_dm,
    study_id = study_id
  )

  # Synthetic laboratory measurements.
  #
  # Includes:
  #   - All six supported laboratory tests
  #   - Multiple subjects and visits
  #   - LOW, NORMAL and HIGH results
  #   - Results exactly at reference limits
  #   - Pre-treatment and post-treatment assessments

  raw_lb <- data.frame(

    subject_id = c(
      "001", "001", "001", "001",
      "001", "001", "001", "001",
      "002", "002", "002", "003"
    ),

    site_id = c(
      rep("101", 11),
      "102"
    ),

    lab_id = sprintf(
      "LB%04d",
      seq_len(12)
    ),

    lab_test = c(
      "HGB",
      "WBC",
      "PLAT",
      "CREAT",
      "ALT",
      "CRP",
      "HGB",
      "CRP",
      "HGB",
      "WBC",
      "ALT",
      "CREAT"
    ),

    result = c(
      11.0,
      6.5,
      250,
      0.9,
      45,
      5.0,
      12.0,
      7.2,
      17.5,
      4.0,
      0.0,
      1.3
    ),

    unit = c(
      "g/dL",
      "10^9/L",
      "10^9/L",
      "mg/dL",
      "U/L",
      "mg/L",
      "g/dL",
      "mg/L",
      "g/dL",
      "10^9/L",
      "U/L",
      "mg/dL"
    ),

    lower_limit = c(
      12.0,
      4.0,
      150,
      0.6,
      0.0,
      0.0,
      12.0,
      0.0,
      12.0,
      4.0,
      0.0,
      0.6
    ),

    upper_limit = c(
      17.5,
      11.0,
      450,
      1.3,
      40,
      5.0,
      17.5,
      5.0,
      17.5,
      11.0,
      40,
      1.3
    ),

    collection_date = c(
      rep("2026-01-01", 6),
      rep("2026-01-29", 2),
      rep("2026-01-02", 2),
      "2026-01-01",
      "2026-01-03"
    ),

    visit = c(
      rep("Baseline", 6),
      rep("Week 4", 2),
      rep("Baseline", 2),
      "Screening",
      "Baseline"
    ),

    visitnum = c(
      rep(1, 6),
      rep(2, 2),
      rep(1, 2),
      0,
      1
    ),

    stringsAsFactors = FALSE
  )

  # Resolve the mapping specification from the package.
  #
  # system.file() supports both installed packages and
  # development workflows using devtools::load_all().

  mapping_path <- system.file(
    "mappings",
    "lb_mapping.csv",
    package = "sdtmgenerator",
    mustWork = TRUE
  )

  list(
    study_id = study_id,
    raw_dm = raw_dm,
    dm = dm,
    raw_lb = raw_lb,
    mapping_path = mapping_path
  )
}


# ------------------------------------------------------------
# 2. Mapping specification helpers
# ------------------------------------------------------------

# Read the production LB mapping specification.

read_lb_mapping_fixture <- function(fixture) {

  readr::read_csv(
    fixture$mapping_path,
    show_col_types = FALSE
  )
}


# Write a temporary mapping specification.
#
# Temporary files allow metadata rules to be modified
# without changing the production mapping file.

write_lb_mapping_fixture <- function(mapping) {

  mapping_path <- tempfile(
    pattern = "lb_mapping_",
    fileext = ".csv"
  )

  readr::write_csv(
    mapping,
    mapping_path,
    na = "NA"
  )

  mapping_path
}


# Generate LB using the metadata-driven workflow.

generate_metadata_lb_fixture <- function(fixture) {

  generate_lb(
    raw_data = fixture$raw_lb,
    dm = fixture$dm,
    mapping_path = fixture$mapping_path
  )
}


# ============================================================
# 3. Legacy compatibility
# ============================================================

test_that("LB legacy workflow remains available", {

  fixture <- make_metadata_lb_fixture()

  lb <- generate_lb(
    raw_data = fixture$raw_lb,
    dm = fixture$dm
  )

  expect_s3_class(lb, "data.frame")

  expect_equal(
    nrow(lb),
    nrow(fixture$raw_lb)
  )

  expect_equal(
    ncol(lb),
    22L
  )

  expect_true(
    validate_lb(lb, fixture$dm)
  )
})


test_that("LB explicit NULL mapping preserves legacy behavior", {

  fixture <- make_metadata_lb_fixture()

  lb_default <- generate_lb(
    raw_data = fixture$raw_lb,
    dm = fixture$dm
  )

  lb_null <- generate_lb(
    raw_data = fixture$raw_lb,
    dm = fixture$dm,
    mapping_path = NULL
  )

  expect_identical(
    lb_default,
    lb_null
  )
})


# ============================================================
# 4. Metadata-driven generation and equivalence
# ============================================================

test_that("LB metadata-driven workflow generates valid data", {

  fixture <- make_metadata_lb_fixture()

  lb <- generate_metadata_lb_fixture(fixture)

  expect_s3_class(lb, "data.frame")

  expect_equal(
    nrow(lb),
    nrow(fixture$raw_lb)
  )

  expect_equal(
    ncol(lb),
    22L
  )

  expect_true(
    validate_lb(lb, fixture$dm)
  )
})


test_that("LB metadata and legacy workflows are identical", {

  fixture <- make_metadata_lb_fixture()

  lb_legacy <- generate_lb(
    raw_data = fixture$raw_lb,
    dm = fixture$dm
  )

  lb_metadata <- generate_metadata_lb_fixture(fixture)

  expect_identical(
    lb_legacy,
    lb_metadata
  )
})


# ============================================================
# 5. Output structure
# ============================================================

test_that("LB metadata preserves the expected dataset structure", {

  fixture <- make_metadata_lb_fixture()

  lb <- generate_metadata_lb_fixture(fixture)

  expected_variables <- c(
    "STUDYID",
    "DOMAIN",
    "USUBJID",
    "LBSEQ",
    "LBTESTCD",
    "LBTEST",
    "LBCAT",
    "LBSPEC",
    "VISIT",
    "VISITNUM",
    "LBORRES",
    "LBORRESU",
    "LBSTRESC",
    "LBSTRESN",
    "LBSTRESU",
    "LBORNRLO",
    "LBORNRHI",
    "LBSTNRLO",
    "LBSTNRHI",
    "LBNRIND",
    "LBDTC",
    "LBDY"
  )

  expect_identical(
    names(lb),
    expected_variables
  )

  expect_equal(ncol(lb), 22L)

  expect_true(is.character(lb$STUDYID))
  expect_true(is.character(lb$DOMAIN))
  expect_true(is.character(lb$USUBJID))
  expect_true(is.integer(lb$LBSEQ))
  expect_true(is.character(lb$LBTESTCD))
  expect_true(is.character(lb$LBTEST))
  expect_true(is.numeric(lb$VISITNUM))
  expect_true(is.character(lb$LBORRES))
  expect_true(is.character(lb$LBSTRESC))
  expect_true(is.numeric(lb$LBSTRESN))
  expect_true(is.numeric(lb$LBORNRLO))
  expect_true(is.numeric(lb$LBORNRHI))
  expect_true(is.numeric(lb$LBSTNRLO))
  expect_true(is.numeric(lb$LBSTNRHI))
  expect_true(is.character(lb$LBDTC))
  expect_true(is.integer(lb$LBDY))
})


# ============================================================
# 6. Record preservation and identifiers
# ============================================================

test_that("LB metadata preserves source records", {

  fixture <- make_metadata_lb_fixture()

  lb <- generate_metadata_lb_fixture(fixture)

  expect_equal(
    nrow(lb),
    nrow(fixture$raw_lb)
  )

  expect_equal(
    length(unique(lb$USUBJID)),
    length(unique(fixture$raw_lb$subject_id))
  )

  expect_setequal(
    unique(lb$LBTESTCD),
    unique(fixture$raw_lb$lab_test)
  )

  expect_equal(
    sort(as.numeric(lb$LBORRES)),
    sort(fixture$raw_lb$result)
  )
})


test_that("LB metadata preserves study and domain identifiers", {

  fixture <- make_metadata_lb_fixture()

  lb <- generate_metadata_lb_fixture(fixture)

  expect_true(
    all(lb$STUDYID == fixture$study_id)
  )

  expect_true(
    all(lb$DOMAIN == "LB")
  )

  expect_false(
    anyNA(lb$STUDYID)
  )

  expect_false(
    anyNA(lb$USUBJID)
  )
})


# ============================================================
# 7. Referential integrity
# ============================================================

test_that("LB metadata maintains referential integrity with DM", {

  fixture <- make_metadata_lb_fixture()

  lb <- generate_metadata_lb_fixture(fixture)

  expect_true(
    all(lb$USUBJID %in% fixture$dm$USUBJID)
  )

  expect_equal(
    anyDuplicated(
      lb[c("STUDYID", "USUBJID", "LBSEQ")]
    ),
    0L
  )
})


test_that("LB metadata rejects subjects absent from DM", {

  fixture <- make_metadata_lb_fixture()

  invalid_data <- fixture$raw_lb

  invalid_data$subject_id[1] <- "999"

  expect_error(
    generate_lb(
      raw_data = invalid_data,
      dm = fixture$dm,
      mapping_path = fixture$mapping_path
    ),
    "not found in DM"
  )
})


test_that("LB metadata rejects duplicate source identifiers", {

  fixture <- make_metadata_lb_fixture()

  invalid_data <- fixture$raw_lb

  invalid_data$lab_id[2] <- invalid_data$lab_id[1]

  expect_error(
    generate_lb(
      raw_data = invalid_data,
      dm = fixture$dm,
      mapping_path = fixture$mapping_path
    ),
    "Duplicate"
  )
})


# ============================================================
# 8. Controlled laboratory terminology
# ============================================================

test_that("LB metadata maps laboratory test names correctly", {

  fixture <- make_metadata_lb_fixture()

  lb <- generate_metadata_lb_fixture(fixture)

  expected_names <- c(
    HGB = "Hemoglobin",
    WBC = "Leukocytes",
    PLAT = "Platelets",
    CREAT = "Creatinine",
    ALT = "Alanine Aminotransferase",
    CRP = "C-Reactive Protein"
  )

  expect_identical(
    lb$LBTEST,
    unname(expected_names[lb$LBTESTCD])
  )
})


test_that("LB metadata maps laboratory categories correctly", {

  fixture <- make_metadata_lb_fixture()

  lb <- generate_metadata_lb_fixture(fixture)

  expected_categories <- c(
    HGB = "HEMATOLOGY",
    WBC = "HEMATOLOGY",
    PLAT = "HEMATOLOGY",
    CREAT = "CHEMISTRY",
    ALT = "CHEMISTRY",
    CRP = "CHEMISTRY"
  )

  expect_identical(
    lb$LBCAT,
    unname(expected_categories[lb$LBTESTCD])
  )
})


test_that("LB metadata maps specimen types correctly", {

  fixture <- make_metadata_lb_fixture()

  lb <- generate_metadata_lb_fixture(fixture)

  expected_specimens <- c(
    HGB = "BLOOD",
    WBC = "BLOOD",
    PLAT = "BLOOD",
    CREAT = "SERUM",
    ALT = "SERUM",
    CRP = "SERUM"
  )

  expect_identical(
    lb$LBSPEC,
    unname(expected_specimens[lb$LBTESTCD])
  )
})


test_that("LB metadata rejects unsupported laboratory tests", {

  fixture <- make_metadata_lb_fixture()

  invalid_data <- fixture$raw_lb

  invalid_data$lab_test[1] <- "UNKNOWN"

  expect_error(
    generate_lb(
      raw_data = invalid_data,
      dm = fixture$dm,
      mapping_path = fixture$mapping_path
    )
  )
})


# ============================================================
# 9. Original and standardized results
# ============================================================

test_that("LB metadata preserves original numeric results", {

  fixture <- make_metadata_lb_fixture()

  lb <- generate_metadata_lb_fixture(fixture)

  expect_equal(
    sort(as.numeric(lb$LBORRES)),
    sort(fixture$raw_lb$result)
  )

  expect_equal(
    lb$LBSTRESN,
    as.numeric(lb$LBORRES)
  )

  expect_identical(
    lb$LBSTRESC,
    as.character(lb$LBSTRESN)
  )
})


test_that("LB metadata preserves standardized laboratory units", {

  fixture <- make_metadata_lb_fixture()

  lb <- generate_metadata_lb_fixture(fixture)

  expected_units <- c(
    HGB = "g/dL",
    WBC = "10^9/L",
    PLAT = "10^9/L",
    CREAT = "mg/dL",
    ALT = "U/L",
    CRP = "mg/L"
  )

  expect_identical(
    lb$LBSTRESU,
    unname(expected_units[lb$LBTESTCD])
  )

  expect_identical(
    lb$LBORRESU,
    lb$LBSTRESU
  )
})


test_that("LB metadata rejects unsupported laboratory units", {

  fixture <- make_metadata_lb_fixture()

  invalid_data <- fixture$raw_lb

  invalid_data$unit[1] <- "INVALID_UNIT"

  expect_error(
    generate_lb(
      raw_data = invalid_data,
      dm = fixture$dm,
      mapping_path = fixture$mapping_path
    )
  )
})


test_that("LB metadata rejects invalid numeric results", {

  fixture <- make_metadata_lb_fixture()

  invalid_data <- fixture$raw_lb

  invalid_data$result[1] <- Inf

  expect_error(
    generate_lb(
      raw_data = invalid_data,
      dm = fixture$dm,
      mapping_path = fixture$mapping_path
    )
  )
})


# ============================================================
# 10. Laboratory reference ranges
# ============================================================

test_that("LB metadata preserves valid reference ranges", {

  fixture <- make_metadata_lb_fixture()

  lb <- generate_metadata_lb_fixture(fixture)

  expect_true(
    all(lb$LBORNRLO <= lb$LBORNRHI)
  )

  expect_true(
    all(lb$LBSTNRLO <= lb$LBSTNRHI)
  )

  expect_equal(
    lb$LBORNRLO,
    lb$LBSTNRLO
  )

  expect_equal(
    lb$LBORNRHI,
    lb$LBSTNRHI
  )
})


test_that("LB metadata rejects inverted reference ranges", {

  fixture <- make_metadata_lb_fixture()

  invalid_data <- fixture$raw_lb

  invalid_data$lower_limit[1] <- 20
  invalid_data$upper_limit[1] <- 10

  expect_error(
    generate_lb(
      raw_data = invalid_data,
      dm = fixture$dm,
      mapping_path = fixture$mapping_path
    )
  )
})


# ============================================================
# 11. Normal-range indicators
# ============================================================

test_that("LB metadata derives normal-range indicators correctly", {

  fixture <- make_metadata_lb_fixture()

  lb <- generate_metadata_lb_fixture(fixture)

  expected_indicator <- ifelse(
    lb$LBSTRESN < lb$LBSTNRLO,
    "LOW",
    ifelse(
      lb$LBSTRESN > lb$LBSTNRHI,
      "HIGH",
      "NORMAL"
    )
  )

  expect_identical(
    lb$LBNRIND,
    expected_indicator
  )

  expect_setequal(
    unique(lb$LBNRIND),
    c("LOW", "NORMAL", "HIGH")
  )

  expect_equal(
    sum(lb$LBNRIND == "LOW"),
    1L
  )

  expect_equal(
    sum(lb$LBNRIND == "HIGH"),
    2L
  )

  expect_equal(
    sum(lb$LBNRIND == "NORMAL"),
    9L
  )
})


test_that("LB metadata classifies lower-limit results as NORMAL", {

  fixture <- make_metadata_lb_fixture()

  lb <- generate_metadata_lb_fixture(fixture)

  at_lower_limit <- lb$LBSTRESN == lb$LBSTNRLO

  expect_true(
    any(at_lower_limit)
  )

  expect_true(
    all(lb$LBNRIND[at_lower_limit] == "NORMAL")
  )
})


test_that("LB metadata classifies upper-limit results as NORMAL", {

  fixture <- make_metadata_lb_fixture()

  lb <- generate_metadata_lb_fixture(fixture)

  at_upper_limit <- lb$LBSTRESN == lb$LBSTNRHI

  expect_true(
    any(at_upper_limit)
  )

  expect_true(
    all(lb$LBNRIND[at_upper_limit] == "NORMAL")
  )
})


# ============================================================
# 12. Collection dates and study days
# ============================================================

test_that("LB metadata preserves ISO 8601 collection dates", {

  fixture <- make_metadata_lb_fixture()

  lb <- generate_metadata_lb_fixture(fixture)

  expect_true(
    all(
      grepl(
        "^[0-9]{4}-[0-9]{2}-[0-9]{2}$",
        lb$LBDTC
      )
    )
  )

  expect_false(
    anyNA(as.Date(lb$LBDTC))
  )
})


test_that("LB metadata supports Date-class source dates", {

  fixture <- make_metadata_lb_fixture()

  lb_character <- generate_metadata_lb_fixture(fixture)

  fixture$raw_lb$collection_date <- as.Date(
    fixture$raw_lb$collection_date
  )

  lb_date <- generate_metadata_lb_fixture(fixture)

  expect_identical(
    lb_character,
    lb_date
  )
})


test_that("LB metadata derives study days relative to DM", {

  fixture <- make_metadata_lb_fixture()

  lb <- generate_metadata_lb_fixture(fixture)

  dm_index <- match(
    lb$USUBJID,
    fixture$dm$USUBJID
  )

  reference_dates <- as.Date(
    fixture$dm$RFSTDTC[dm_index]
  )

  collection_dates <- as.Date(lb$LBDTC)

  differences <- as.integer(
    collection_dates - reference_dates
  )

  expected_days <- as.integer(
    ifelse(
      differences >= 0L,
      differences + 1L,
      differences
    )
  )

  expect_identical(
    lb$LBDY,
    expected_days
  )

  expect_true(
    any(lb$LBDY == 1L)
  )

  expect_true(
    any(lb$LBDY == -1L)
  )

  expect_true(
    any(lb$LBDY == 29L)
  )

  expect_false(
    any(lb$LBDY == 0L)
  )
})


test_that("LB metadata supports missing reference dates", {

  fixture <- make_metadata_lb_fixture()

  fixture$dm$RFSTDTC[1] <- NA_character_

  lb <- generate_metadata_lb_fixture(fixture)

  affected_subject <- fixture$dm$USUBJID[1]

  affected_records <- lb$USUBJID == affected_subject

  expect_true(
    any(affected_records)
  )

  expect_true(
    all(is.na(lb$LBDY[affected_records]))
  )

  expect_false(
    anyNA(lb$LBDY[!affected_records])
  )
})


test_that("LB metadata rejects invalid calendar dates", {

  fixture <- make_metadata_lb_fixture()

  invalid_data <- fixture$raw_lb

  invalid_data$collection_date[1] <- "2026-02-30"

  expect_error(
    generate_lb(
      raw_data = invalid_data,
      dm = fixture$dm,
      mapping_path = fixture$mapping_path
    )
  )
})


# ============================================================
# 13. Deterministic ordering and sequencing
# ============================================================

test_that("LB metadata generates consecutive subject sequences", {

  fixture <- make_metadata_lb_fixture()

  lb <- generate_metadata_lb_fixture(fixture)

  subject_records <- split(
    lb$LBSEQ,
    lb$USUBJID
  )

  for (sequence_values in subject_records) {

    expect_identical(
      sequence_values,
      seq_along(sequence_values)
    )
  }
})


test_that("LB metadata output is independent of source row order", {

  fixture <- make_metadata_lb_fixture()

  lb_original <- generate_metadata_lb_fixture(fixture)

  # Reverse the source row order.

  reversed_data <- fixture$raw_lb[
    rev(seq_len(nrow(fixture$raw_lb))),
    ,
    drop = FALSE
  ]

  lb_reversed <- generate_lb(
    raw_data = reversed_data,
    dm = fixture$dm,
    mapping_path = fixture$mapping_path
  )

  expect_identical(
    lb_original,
    lb_reversed
  )
})


test_that("LB metadata produces reproducible results", {

  fixture <- make_metadata_lb_fixture()

  lb_first <- generate_metadata_lb_fixture(fixture)

  lb_second <- generate_metadata_lb_fixture(fixture)

  expect_identical(
    lb_first,
    lb_second
  )
})


# ============================================================
# 14. Invalid metadata rejection
# ============================================================

test_that("LB metadata rejects nonexistent mapping files", {

  fixture <- make_metadata_lb_fixture()

  invalid_path <- tempfile(
    fileext = ".csv"
  )

  expect_false(
    file.exists(invalid_path)
  )

  expect_error(
    generate_lb(
      raw_data = fixture$raw_lb,
      dm = fixture$dm,
      mapping_path = invalid_path
    )
  )
})


test_that("LB metadata rejects missing mapping columns", {

  fixture <- make_metadata_lb_fixture()

  mapping <- read_lb_mapping_fixture(fixture)

  mapping$transformation <- NULL

  mapping_path <- write_lb_mapping_fixture(mapping)

  on.exit(
    unlink(mapping_path),
    add = TRUE
  )

  expect_error(
    generate_lb(
      raw_data = fixture$raw_lb,
      dm = fixture$dm,
      mapping_path = mapping_path
    )
  )
})


test_that("LB metadata rejects duplicate mapping targets", {

  fixture <- make_metadata_lb_fixture()

  mapping <- read_lb_mapping_fixture(fixture)

  invalid_mapping <- rbind(
    mapping,
    mapping[1, , drop = FALSE]
  )

  mapping_path <- write_lb_mapping_fixture(
    invalid_mapping
  )

  on.exit(
    unlink(mapping_path),
    add = TRUE
  )

  expect_error(
    generate_lb(
      raw_data = fixture$raw_lb,
      dm = fixture$dm,
      mapping_path = mapping_path
    )
  )
})


test_that("LB metadata rejects missing required targets", {

  fixture <- make_metadata_lb_fixture()

  mapping <- read_lb_mapping_fixture(fixture)

  expect_true(
    "LBTESTCD" %in% mapping$target_variable
  )

  invalid_mapping <- mapping[
    mapping$target_variable != "LBTESTCD",
    ,
    drop = FALSE
  ]

  mapping_path <- write_lb_mapping_fixture(
    invalid_mapping
  )

  on.exit(
    unlink(mapping_path),
    add = TRUE
  )

  expect_error(
    generate_lb(
      raw_data = fixture$raw_lb,
      dm = fixture$dm,
      mapping_path = mapping_path
    )
  )
})


test_that("LB metadata rejects unsupported transformations", {

  fixture <- make_metadata_lb_fixture()

  mapping <- read_lb_mapping_fixture(fixture)

  mapping$transformation[1] <- "unsupported_transform"

  mapping_path <- write_lb_mapping_fixture(mapping)

  on.exit(
    unlink(mapping_path),
    add = TRUE
  )

  expect_error(
    generate_lb(
      raw_data = fixture$raw_lb,
      dm = fixture$dm,
      mapping_path = mapping_path
    )
  )
})


test_that("LB metadata rejects nonexistent source variables", {

  fixture <- make_metadata_lb_fixture()

  mapping <- read_lb_mapping_fixture(fixture)

  target_index <- which(
    mapping$target_variable == "LBTESTCD"
  )

  expect_length(
    target_index,
    1L
  )

  mapping$source_variable[target_index] <-
    "nonexistent_source_variable"

  mapping_path <- write_lb_mapping_fixture(mapping)

  on.exit(
    unlink(mapping_path),
    add = TRUE
  )

  expect_error(
    generate_lb(
      raw_data = fixture$raw_lb,
      dm = fixture$dm,
      mapping_path = mapping_path
    )
  )
})


test_that("LB metadata rejects inconsistent domain identifiers", {

  fixture <- make_metadata_lb_fixture()

  mapping <- read_lb_mapping_fixture(fixture)

  target_index <- which(
    mapping$target_variable == "DOMAIN"
  )

  expect_length(
    target_index,
    1L
  )

  mapping$parameter[target_index] <- "AE"

  mapping_path <- write_lb_mapping_fixture(mapping)

  on.exit(
    unlink(mapping_path),
    add = TRUE
  )

  expect_error(
    generate_lb(
      raw_data = fixture$raw_lb,
      dm = fixture$dm,
      mapping_path = mapping_path
    )
  )
})


# ============================================================
# 15. Source data validation
# ============================================================

test_that("LB metadata rejects missing raw source columns", {

  fixture <- make_metadata_lb_fixture()

  invalid_data <- fixture$raw_lb

  invalid_data$result <- NULL

  expect_error(
    generate_lb(
      raw_data = invalid_data,
      dm = fixture$dm,
      mapping_path = fixture$mapping_path
    )
  )
})


test_that("LB metadata rejects empty source datasets", {

  fixture <- make_metadata_lb_fixture()

  empty_data <- fixture$raw_lb[
    FALSE,
    ,
    drop = FALSE
  ]

  expect_error(
    generate_lb(
      raw_data = empty_data,
      dm = fixture$dm,
      mapping_path = fixture$mapping_path
    )
  )
})


test_that("LB metadata rejects missing collection dates", {

  fixture <- make_metadata_lb_fixture()

  invalid_data <- fixture$raw_lb

  invalid_data$collection_date[1] <- NA_character_

  expect_error(
    generate_lb(
      raw_data = invalid_data,
      dm = fixture$dm,
      mapping_path = fixture$mapping_path
    )
  )
})


# ============================================================
# 16. Source data immutability
# ============================================================

test_that("LB metadata does not modify source laboratory data", {

  fixture <- make_metadata_lb_fixture()

  original_data <- fixture$raw_lb

  generate_metadata_lb_fixture(fixture)

  expect_identical(
    fixture$raw_lb,
    original_data
  )
})


test_that("LB metadata does not modify the DM reference dataset", {

  fixture <- make_metadata_lb_fixture()

  original_dm <- fixture$dm

  generate_metadata_lb_fixture(fixture)

  expect_identical(
    fixture$dm,
    original_dm
  )
})


# ============================================================
# 17. Metadata-driven behavior
# ============================================================

test_that("LB metadata rules control the generated output", {

  fixture <- make_metadata_lb_fixture()

  reference_lb <- generate_metadata_lb_fixture(fixture)

  mapping <- read_lb_mapping_fixture(fixture)

  # Modify the VISIT mapping rule.
  #
  # The source visit variable is replaced by a constant.
  # This demonstrates that the external metadata rules
  # actively control the generated output.

  target_index <- which(
    mapping$target_variable == "VISIT"
  )

  expect_length(
    target_index,
    1L
  )

  mapping$source_variable[target_index] <- NA_character_

  mapping$transformation[target_index] <- "constant"

  mapping$parameter[target_index] <- "Scheduled Visit"

  mapping_path <- write_lb_mapping_fixture(mapping)

  on.exit(
    unlink(mapping_path),
    add = TRUE
  )

  modified_lb <- generate_lb(
    raw_data = fixture$raw_lb,
    dm = fixture$dm,
    mapping_path = mapping_path
  )

  # Confirm that the metadata modification affects VISIT.

  expect_true(
    all(modified_lb$VISIT == "Scheduled Visit")
  )

  expect_false(
    identical(
      reference_lb$VISIT,
      modified_lb$VISIT
    )
  )

  # Confirm that unrelated variables remain unchanged.

  unchanged_columns <- setdiff(
    names(reference_lb),
    "VISIT"
  )

  expect_identical(
    modified_lb[unchanged_columns],
    reference_lb[unchanged_columns]
  )

  expect_equal(
    nrow(modified_lb),
    nrow(reference_lb)
  )
})


# ============================================================
# End of Metadata-Driven LB Integration Tests
# ============================================================