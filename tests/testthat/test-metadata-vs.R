
# ============================================================
# Metadata-Driven VS Integration Tests
# ============================================================
#
# Project: SDTM Dataset Generator
# Domain: VS - Vital Signs
#
# Purpose:
#   Validate the integration between the metadata-driven
#   mapping engine and the VS domain generator.
#
# Coverage:
#   - Backward compatibility
#   - Legacy and metadata equivalence
#   - Structural integrity
#   - Referential integrity
#   - Controlled terminology
#   - Clinical unit conversions
#   - Study-day derivations
#   - Deterministic sequencing
#   - Metadata configuration validation
#   - Metadata-driven behavior
#   - Source data immutability
#
# ============================================================


# ------------------------------------------------------------
# 1. Test fixtures and helper functions
# ------------------------------------------------------------

metadata_vs_package_file <- function(...) {

  path <- system.file(
    ...,
    package = "sdtmgenerator",
    mustWork = TRUE
  )

  path
}


make_metadata_vs_fixture <- function() {

  raw_dm <- readr::read_csv(
    metadata_vs_package_file(
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

  raw_vs <- readr::read_csv(
    metadata_vs_package_file(
      "extdata",
      "raw_vs.csv"
    ),
    col_types = readr::cols(
      subject_id = readr::col_character(),
      site_id = readr::col_character(),
      .default = readr::col_guess()
    ),
    show_col_types = FALSE
  )

  study_id <- "STUDY001"

  dm <- generate_dm(
    raw_data = raw_dm,
    study_id = study_id
  )

  mapping_path <- metadata_vs_package_file(
    "mappings",
    "vs_mapping.csv"
  )

  list(
    raw_dm = raw_dm,
    raw_vs = raw_vs,
    dm = dm,
    study_id = study_id,
    mapping_path = mapping_path
  )
}


write_metadata_vs_mapping <- function(mapping) {

  path <- tempfile(
    pattern = "vs_mapping_",
    fileext = ".csv"
  )

  readr::write_csv(
    mapping,
    path,
    na = "NA"
  )

  path
}


# ------------------------------------------------------------
# 2. Mapping specification
# ------------------------------------------------------------

test_that("VS metadata mapping file is available", {

  fixture <- make_metadata_vs_fixture()

  expect_true(
    file.exists(fixture$mapping_path)
  )

  mapping <- read_mapping(
    fixture$mapping_path
  )

  expect_s3_class(mapping, "data.frame")

  expect_true(nrow(mapping) > 0L)

  expect_true(
    all(mapping$domain == "VS")
  )
})


test_that("VS mapping contains all expected metadata targets", {

  fixture <- make_metadata_vs_fixture()

  mapping <- read_mapping(
    fixture$mapping_path
  )

  expected_targets <- c(
    "STUDYID",
    "DOMAIN",
    "VSTESTCD",
    "VSTEST",
    "VISIT",
    "VISITNUM",
    "VSORRES",
    "VSORRESU",
    "VSDTC"
  )

  expect_true(
    all(
      expected_targets %in%
        mapping$target_variable
    )
  )

  expect_equal(
    anyDuplicated(mapping$target_variable),
    0L
  )
})


test_that("VS STUDYID uses the study_id transformation", {

  fixture <- make_metadata_vs_fixture()

  mapping <- read_mapping(
    fixture$mapping_path
  )

  study_rule <- mapping[
    mapping$target_variable == "STUDYID",
    ,
    drop = FALSE
  ]

  expect_equal(
    nrow(study_rule),
    1L
  )

  expect_identical(
    study_rule$transformation,
    "study_id"
  )
})


# ------------------------------------------------------------
# 3. Backward compatibility
# ------------------------------------------------------------

test_that("VS legacy generation remains available", {

  fixture <- make_metadata_vs_fixture()

  vs <- generate_vs(
    raw_data = fixture$raw_vs,
    dm = fixture$dm
  )

  expect_s3_class(vs, "tbl_df")

  expect_equal(
    nrow(vs),
    nrow(fixture$raw_vs)
  )

  expect_equal(
    ncol(vs),
    15L
  )

  expect_true(
    validate_vs(vs, fixture$dm)
  )
})


test_that("Explicit NULL mapping preserves VS legacy behavior", {

  fixture <- make_metadata_vs_fixture()

  vs_default <- generate_vs(
    raw_data = fixture$raw_vs,
    dm = fixture$dm
  )

  vs_null <- generate_vs(
    raw_data = fixture$raw_vs,
    dm = fixture$dm,
    mapping_path = NULL
  )

  expect_identical(
    vs_default,
    vs_null
  )
})


# ------------------------------------------------------------
# 4. Metadata-driven generation and equivalence
# ------------------------------------------------------------

test_that("VS metadata-driven generation succeeds", {

  fixture <- make_metadata_vs_fixture()

  vs <- generate_vs(
    raw_data = fixture$raw_vs,
    dm = fixture$dm,
    mapping_path = fixture$mapping_path
  )

  expect_s3_class(vs, "tbl_df")

  expect_equal(
    nrow(vs),
    nrow(fixture$raw_vs)
  )

  expect_equal(
    ncol(vs),
    15L
  )

  expect_true(
    validate_vs(vs, fixture$dm)
  )
})


test_that("VS legacy and metadata outputs are identical", {

  fixture <- make_metadata_vs_fixture()

  vs_legacy <- generate_vs(
    raw_data = fixture$raw_vs,
    dm = fixture$dm
  )

  vs_metadata <- generate_vs(
    raw_data = fixture$raw_vs,
    dm = fixture$dm,
    mapping_path = fixture$mapping_path
  )

  expect_identical(
    vs_metadata,
    vs_legacy
  )

  expect_identical(
    names(vs_metadata),
    names(vs_legacy)
  )

  expect_identical(
    dim(vs_metadata),
    dim(vs_legacy)
  )
})


# ------------------------------------------------------------
# 5. Structural integrity
# ------------------------------------------------------------

test_that("Metadata-driven VS preserves the expected schema", {

  fixture <- make_metadata_vs_fixture()

  vs <- generate_vs(
    fixture$raw_vs,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  expected_columns <- c(
    "STUDYID",
    "DOMAIN",
    "USUBJID",
    "VSSEQ",
    "VSTESTCD",
    "VSTEST",
    "VISIT",
    "VISITNUM",
    "VSORRES",
    "VSORRESU",
    "VSSTRESC",
    "VSSTRESN",
    "VSSTRESU",
    "VSDTC",
    "VSSTDY"
  )

  expect_identical(
    names(vs),
    expected_columns
  )

  expect_equal(
    nrow(vs),
    nrow(fixture$raw_vs)
  )

  expect_equal(
    ncol(vs),
    length(expected_columns)
  )
})


test_that("Metadata-driven VS preserves variable types", {

  fixture <- make_metadata_vs_fixture()

  vs <- generate_vs(
    fixture$raw_vs,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  character_variables <- c(
    "STUDYID",
    "DOMAIN",
    "USUBJID",
    "VSTESTCD",
    "VSTEST",
    "VISIT",
    "VSORRES",
    "VSORRESU",
    "VSSTRESC",
    "VSSTRESU",
    "VSDTC"
  )

  for (variable in character_variables) {

    expect_type(
      vs[[variable]],
      "character"
    )
  }

  expect_true(is.numeric(vs$VSSEQ))

  expect_true(is.numeric(vs$VISITNUM))

  expect_true(is.numeric(vs$VSSTRESN))

  expect_true(is.numeric(vs$VSSTDY))
})


test_that("Metadata-driven VS preserves study and domain identifiers", {

  fixture <- make_metadata_vs_fixture()

  vs <- generate_vs(
    fixture$raw_vs,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  expect_true(
    all(vs$STUDYID == fixture$study_id)
  )

  expect_true(
    all(vs$DOMAIN == "VS")
  )

  expect_false(anyNA(vs$STUDYID))

  expect_false(anyNA(vs$DOMAIN))
})


# ------------------------------------------------------------
# 6. Referential integrity
# ------------------------------------------------------------

test_that("Metadata-driven VS subjects exist in DM", {

  fixture <- make_metadata_vs_fixture()

  vs <- generate_vs(
    fixture$raw_vs,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  expect_true(
    all(
      vs$USUBJID %in%
        fixture$dm$USUBJID
    )
  )

  expect_false(anyNA(vs$USUBJID))
})


test_that("Metadata-driven VS rejects unmatched subjects", {

  fixture <- make_metadata_vs_fixture()

  invalid_data <- fixture$raw_vs

  invalid_data$subject_id[1] <- "999999"

  expect_error(
    generate_vs(
      invalid_data,
      fixture$dm,
      mapping_path = fixture$mapping_path
    ),
    "not found in DM"
  )
})


# ------------------------------------------------------------
# 7. Controlled terminology
# ------------------------------------------------------------

test_that("Metadata-driven VS maps supported test terminology", {

  fixture <- make_metadata_vs_fixture()

  vs <- generate_vs(
    fixture$raw_vs,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  expected_names <- c(
    SYSBP = "Systolic Blood Pressure",
    DIABP = "Diastolic Blood Pressure",
    HR = "Heart Rate",
    TEMP = "Temperature",
    WEIGHT = "Weight",
    HEIGHT = "Height"
  )

  expect_true(
    all(
      vs$VSTESTCD %in%
        names(expected_names)
    )
  )

  expect_identical(
    vs$VSTEST,
    unname(expected_names[vs$VSTESTCD])
  )
})


test_that("Metadata-driven VS rejects unsupported test codes", {

  fixture <- make_metadata_vs_fixture()

  invalid_data <- fixture$raw_vs

  invalid_data$test_code[1] <- "INVALID"

  expect_error(
    generate_vs(
      invalid_data,
      fixture$dm,
      mapping_path = fixture$mapping_path
    ),
    "Unsupported vital signs test code"
  )
})


# ------------------------------------------------------------
# 8. Clinical result standardization
# ------------------------------------------------------------

test_that("Metadata-driven VS preserves original results", {

  fixture <- make_metadata_vs_fixture()

  vs <- generate_vs(
    fixture$raw_vs,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  expect_false(anyNA(vs$VSORRES))

  expect_false(anyNA(vs$VSORRESU))

  expect_equal(
    sort(as.numeric(vs$VSORRES)),
    sort(as.numeric(fixture$raw_vs$result))
  )

  expect_equal(
    sort(vs$VSORRESU),
    sort(trimws(fixture$raw_vs$unit))
  )
})


test_that("Metadata-driven VS converts Fahrenheit to Celsius", {

  fixture <- make_metadata_vs_fixture()

  vs <- generate_vs(
    fixture$raw_vs,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  temperature_f <- vs[
    vs$VSTESTCD == "TEMP" &
      vs$VSORRESU == "F",
    ,
    drop = FALSE
  ]

  expect_true(
    nrow(temperature_f) > 0L
  )

  expected_result <- (
    as.numeric(temperature_f$VSORRES) - 32
  ) * 5 / 9

  expect_equal(
    temperature_f$VSSTRESN,
    expected_result,
    tolerance = 1e-8
  )

  expect_true(
    all(temperature_f$VSSTRESU == "C")
  )
})


test_that("Metadata-driven VS preserves Celsius measurements", {

  fixture <- make_metadata_vs_fixture()

  vs <- generate_vs(
    fixture$raw_vs,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  temperature_c <- vs[
    vs$VSTESTCD == "TEMP" &
      vs$VSORRESU == "C",
    ,
    drop = FALSE
  ]

  expect_true(
    nrow(temperature_c) > 0L
  )

  expect_equal(
    temperature_c$VSSTRESN,
    as.numeric(temperature_c$VSORRES)
  )

  expect_true(
    all(temperature_c$VSSTRESU == "C")
  )
})


test_that("Metadata-driven VS preserves non-temperature results", {

  fixture <- make_metadata_vs_fixture()

  vs <- generate_vs(
    fixture$raw_vs,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  non_temperature <- vs[
    vs$VSTESTCD != "TEMP",
    ,
    drop = FALSE
  ]

  expect_equal(
    non_temperature$VSSTRESN,
    as.numeric(non_temperature$VSORRES)
  )

  expect_identical(
    non_temperature$VSSTRESU,
    non_temperature$VSORRESU
  )
})


test_that("Metadata-driven VS standardized results are consistent", {

  fixture <- make_metadata_vs_fixture()

  vs <- generate_vs(
    fixture$raw_vs,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  expect_true(
    all(is.finite(vs$VSSTRESN))
  )

  expect_equal(
    as.numeric(vs$VSSTRESC),
    vs$VSSTRESN,
    tolerance = 1e-8
  )
})


test_that("Metadata-driven VS rejects unsupported units", {

  fixture <- make_metadata_vs_fixture()

  invalid_data <- fixture$raw_vs

  invalid_data$unit[1] <- "INVALID"

  expect_error(
    generate_vs(
      invalid_data,
      fixture$dm,
      mapping_path = fixture$mapping_path
    ),
    "Unsupported unit"
  )
})


test_that("Metadata-driven VS rejects non-finite results", {

  fixture <- make_metadata_vs_fixture()

  invalid_data <- fixture$raw_vs

  invalid_data$result[1] <- Inf

  expect_error(
    generate_vs(
      invalid_data,
      fixture$dm,
      mapping_path = fixture$mapping_path
    ),
    "finite numeric values"
  )
})


# ------------------------------------------------------------
# 9. Temporal derivations
# ------------------------------------------------------------

test_that("Metadata-driven VS derives correct study days", {

  fixture <- make_metadata_vs_fixture()

  vs <- generate_vs(
    fixture$raw_vs,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  dm_index <- match(
    vs$USUBJID,
    fixture$dm$USUBJID
  )

  reference_dates <- as.Date(
    fixture$dm$RFSTDTC[dm_index],
    format = "%Y-%m-%d"
  )

  assessment_dates <- as.Date(
    vs$VSDTC,
    format = "%Y-%m-%d"
  )

  difference <- as.integer(
    assessment_dates - reference_dates
  )

  expected_days <- ifelse(
    difference >= 0L,
    difference + 1L,
    difference
  )

  expect_equal(
    vs$VSSTDY,
    expected_days
  )

  expect_false(
    any(vs$VSSTDY == 0L, na.rm = TRUE)
  )
})


test_that("Metadata-driven VS assigns study day one at reference date", {

  fixture <- make_metadata_vs_fixture()

  raw_vs <- fixture$raw_vs[
    1L,
    ,
    drop = FALSE
  ]

  dm_index <- match(
    paste(raw_vs$site_id, raw_vs$subject_id),
    paste(fixture$dm$SITEID, fixture$dm$SUBJID)
  )

  raw_vs$assessment_date <- as.character(
    fixture$dm$RFSTDTC[dm_index]
  )

  vs <- generate_vs(
    raw_vs,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  expect_equal(
    vs$VSSTDY,
    1L
  )
})


test_that("Metadata-driven VS supports negative study days", {

  fixture <- make_metadata_vs_fixture()

  raw_vs <- fixture$raw_vs[
    1L,
    ,
    drop = FALSE
  ]

  dm_index <- match(
    paste(raw_vs$site_id, raw_vs$subject_id),
    paste(fixture$dm$SITEID, fixture$dm$SUBJID)
  )

  reference_date <- as.Date(
    fixture$dm$RFSTDTC[dm_index]
  )

  raw_vs$assessment_date <- as.character(
    reference_date - 1L
  )

  vs <- generate_vs(
    raw_vs,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  expect_equal(
    vs$VSSTDY,
    -1L
  )
})


test_that("Metadata-driven VS handles missing reference dates", {

  fixture <- make_metadata_vs_fixture()

  raw_vs <- fixture$raw_vs[
    1L,
    ,
    drop = FALSE
  ]

  dm <- fixture$dm

  dm_index <- match(
    paste(raw_vs$site_id, raw_vs$subject_id),
    paste(dm$SITEID, dm$SUBJID)
  )

  dm$RFSTDTC[dm_index] <- NA_character_

  vs <- generate_vs(
    raw_vs,
    dm,
    mapping_path = fixture$mapping_path
  )

  expect_true(
    is.na(vs$VSSTDY)
  )

  expect_true(
    validate_vs(vs, dm)
  )
})


test_that("Metadata-driven VS rejects invalid assessment dates", {

  fixture <- make_metadata_vs_fixture()

  invalid_data <- fixture$raw_vs

  invalid_data$assessment_date <- as.character(
    invalid_data$assessment_date
  )

  invalid_data$assessment_date[1] <- "2026-02-30"

  expect_error(
    generate_vs(
      invalid_data,
      fixture$dm,
      mapping_path = fixture$mapping_path
    ),
    "assessment_date"
  )
})


# ------------------------------------------------------------
# 10. Primary keys and deterministic sequencing
# ------------------------------------------------------------

test_that("Metadata-driven VS primary keys are unique", {

  fixture <- make_metadata_vs_fixture()

  vs <- generate_vs(
    fixture$raw_vs,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  primary_keys <- vs[
    c("STUDYID", "USUBJID", "VSSEQ")
  ]

  expect_equal(
    anyDuplicated(primary_keys),
    0L
  )

  expect_false(anyNA(vs$VSSEQ))
})


test_that("Metadata-driven VS sequence restarts for each subject", {

  fixture <- make_metadata_vs_fixture()

  vs <- generate_vs(
    fixture$raw_vs,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  subject_sequences <- split(
    vs$VSSEQ,
    vs$USUBJID
  )

  for (sequence in subject_sequences) {

    expect_equal(
      as.integer(sequence),
      seq_len(length(sequence))
    )
  }
})


test_that("Metadata-driven VS rejects duplicate assessments", {

  fixture <- make_metadata_vs_fixture()

  invalid_data <- rbind(
    fixture$raw_vs,
    fixture$raw_vs[1L, , drop = FALSE]
  )

  expect_error(
    generate_vs(
      invalid_data,
      fixture$dm,
      mapping_path = fixture$mapping_path
    ),
    "Duplicate VS assessments"
  )
})


test_that("Metadata-driven VS is independent of source row order", {

  fixture <- make_metadata_vs_fixture()

  original_vs <- generate_vs(
    fixture$raw_vs,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  reversed_data <- fixture$raw_vs[
    rev(seq_len(nrow(fixture$raw_vs))),
    ,
    drop = FALSE
  ]

  reordered_vs <- generate_vs(
    reversed_data,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  expect_identical(
    original_vs,
    reordered_vs
  )
})


# ------------------------------------------------------------
# 11. Source data validation
# ------------------------------------------------------------

test_that("Metadata-driven VS rejects missing source columns", {

  fixture <- make_metadata_vs_fixture()

  invalid_data <- fixture$raw_vs

  invalid_data$result <- NULL

  expect_error(
    generate_vs(
      invalid_data,
      fixture$dm,
      mapping_path = fixture$mapping_path
    ),
    "Missing required columns"
  )
})


test_that("Metadata-driven VS rejects empty source datasets", {

  fixture <- make_metadata_vs_fixture()

  empty_data <- fixture$raw_vs[
    0L,
    ,
    drop = FALSE
  ]

  expect_error(
    generate_vs(
      empty_data,
      fixture$dm,
      mapping_path = fixture$mapping_path
    ),
    "non-empty data.frame"
  )
})


test_that("Metadata-driven VS rejects missing numeric results", {

  fixture <- make_metadata_vs_fixture()

  invalid_data <- fixture$raw_vs

  invalid_data$result[1] <- NA_real_

  expect_error(
    generate_vs(
      invalid_data,
      fixture$dm,
      mapping_path = fixture$mapping_path
    ),
    "finite numeric values"
  )
})


# ------------------------------------------------------------
# 12. Metadata configuration validation
# ------------------------------------------------------------

test_that("VS metadata rejects nonexistent mapping files", {

  fixture <- make_metadata_vs_fixture()

  invalid_path <- tempfile(
    fileext = ".csv"
  )

  expect_false(
    file.exists(invalid_path)
  )

  expect_error(
    generate_vs(
      fixture$raw_vs,
      fixture$dm,
      mapping_path = invalid_path
    )
  )
})


test_that("VS metadata rejects missing mapping columns", {

  fixture <- make_metadata_vs_fixture()

  mapping <- read_mapping(
    fixture$mapping_path
  )

  mapping$target_variable <- NULL

  mapping_path <- write_metadata_vs_mapping(
    mapping
  )

  on.exit(
    unlink(mapping_path),
    add = TRUE
  )

  expect_error(
    generate_vs(
      fixture$raw_vs,
      fixture$dm,
      mapping_path = mapping_path
    )
  )
})


test_that("VS metadata rejects duplicate mapping targets", {

  fixture <- make_metadata_vs_fixture()

  mapping <- read_mapping(
    fixture$mapping_path
  )

  invalid_mapping <- rbind(
    mapping,
    mapping[1L, , drop = FALSE]
  )

  mapping_path <- write_metadata_vs_mapping(
    invalid_mapping
  )

  on.exit(
    unlink(mapping_path),
    add = TRUE
  )

  expect_error(
    generate_vs(
      fixture$raw_vs,
      fixture$dm,
      mapping_path = mapping_path
    )
  )
})


test_that("VS metadata rejects missing required targets", {

  fixture <- make_metadata_vs_fixture()

  mapping <- read_mapping(
    fixture$mapping_path
  )

  expect_true(
    "VSTESTCD" %in%
      mapping$target_variable
  )

  invalid_mapping <- mapping[
    mapping$target_variable != "VSTESTCD",
    ,
    drop = FALSE
  ]

  mapping_path <- write_metadata_vs_mapping(
    invalid_mapping
  )

  on.exit(
    unlink(mapping_path),
    add = TRUE
  )

  expect_error(
    generate_vs(
      fixture$raw_vs,
      fixture$dm,
      mapping_path = mapping_path
    )
  )
})


test_that("VS metadata rejects unsupported transformations", {

  fixture <- make_metadata_vs_fixture()

  mapping <- read_mapping(
    fixture$mapping_path
  )

  index <- which(
    mapping$target_variable == "VSTESTCD"
  )

  mapping$transformation[index] <-
    "unsupported_transformation"

  mapping_path <- write_metadata_vs_mapping(
    mapping
  )

  on.exit(
    unlink(mapping_path),
    add = TRUE
  )

  expect_error(
    generate_vs(
      fixture$raw_vs,
      fixture$dm,
      mapping_path = mapping_path
    )
  )
})


test_that("VS metadata rejects nonexistent source variables", {

  fixture <- make_metadata_vs_fixture()

  mapping <- read_mapping(
    fixture$mapping_path
  )

  index <- which(
    mapping$target_variable == "VISIT"
  )

  mapping$source_variable[index] <-
    "nonexistent_source"

  mapping_path <- write_metadata_vs_mapping(
    mapping
  )

  on.exit(
    unlink(mapping_path),
    add = TRUE
  )

  expect_error(
    generate_vs(
      fixture$raw_vs,
      fixture$dm,
      mapping_path = mapping_path
    )
  )
})


test_that("VS metadata rejects unknown terminology dictionaries", {

  fixture <- make_metadata_vs_fixture()

  mapping <- read_mapping(
    fixture$mapping_path
  )

  index <- which(
    mapping$target_variable == "VSTEST"
  )

  mapping$parameter[index] <-
    "nonexistent_dictionary"

  mapping_path <- write_metadata_vs_mapping(
    mapping
  )

  on.exit(
    unlink(mapping_path),
    add = TRUE
  )

  expect_error(
    generate_vs(
      fixture$raw_vs,
      fixture$dm,
      mapping_path = mapping_path
    )
  )
})


test_that("VS metadata rejects inconsistent study identifiers", {

  fixture <- make_metadata_vs_fixture()

  mapping <- read_mapping(
    fixture$mapping_path
  )

  index <- which(
    mapping$target_variable == "STUDYID"
  )

  mapping$transformation[index] <- "constant"

  mapping$parameter[index] <- "INVALID_STUDY"

  mapping_path <- write_metadata_vs_mapping(
    mapping
  )

  on.exit(
    unlink(mapping_path),
    add = TRUE
  )

  expect_error(
    generate_vs(
      fixture$raw_vs,
      fixture$dm,
      mapping_path = mapping_path
    )
  )
})


test_that("VS metadata rejects inconsistent domain identifiers", {

  fixture <- make_metadata_vs_fixture()

  mapping <- read_mapping(
    fixture$mapping_path
  )

  index <- which(
    mapping$target_variable == "DOMAIN"
  )

  mapping$parameter[index] <- "INVALID"

  mapping_path <- write_metadata_vs_mapping(
    mapping
  )

  on.exit(
    unlink(mapping_path),
    add = TRUE
  )

  expect_error(
    generate_vs(
      fixture$raw_vs,
      fixture$dm,
      mapping_path = mapping_path
    )
  )
})


# ------------------------------------------------------------
# 13. Metadata-driven behavior
# ------------------------------------------------------------

test_that("VS metadata rules control the generated output", {

  fixture <- make_metadata_vs_fixture()

  vs_legacy <- generate_vs(
    fixture$raw_vs,
    fixture$dm
  )

  mapping <- read_mapping(
    fixture$mapping_path
  )

  index <- which(
    mapping$target_variable == "VISIT"
  )

  expect_equal(
    length(index),
    1L
  )

  # Replace the direct VISIT mapping with a constant.
  # This is an intentional synthetic test modification.

  mapping$source_variable[index] <- NA_character_

  mapping$transformation[index] <- "constant"

  mapping$parameter[index] <- "CUSTOM VISIT"

  mapping_path <- write_metadata_vs_mapping(
    mapping
  )

  on.exit(
    unlink(mapping_path),
    add = TRUE
  )

  vs_metadata <- generate_vs(
    fixture$raw_vs,
    fixture$dm,
    mapping_path = mapping_path
  )

  expect_true(
    all(vs_metadata$VISIT == "CUSTOM VISIT")
  )

  expect_false(
    identical(
      vs_metadata$VISIT,
      vs_legacy$VISIT
    )
  )

  expect_equal(
    nrow(vs_metadata),
    nrow(vs_legacy)
  )

  expect_identical(
    vs_metadata$USUBJID,
    vs_legacy$USUBJID
  )

  expect_identical(
    vs_metadata$VSSTRESN,
    vs_legacy$VSSTRESN
  )
})


# ------------------------------------------------------------
# 14. Deterministic behavior
# ------------------------------------------------------------

test_that("Metadata-driven VS produces reproducible results", {

  fixture <- make_metadata_vs_fixture()

  vs_first <- generate_vs(
    fixture$raw_vs,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  vs_second <- generate_vs(
    fixture$raw_vs,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  expect_identical(
    vs_first,
    vs_second
  )
})


# ------------------------------------------------------------
# 15. Source data immutability
# ------------------------------------------------------------

test_that("Metadata-driven VS does not modify source data", {

  fixture <- make_metadata_vs_fixture()

  original_raw_vs <- fixture$raw_vs

  original_dm <- fixture$dm

  generate_vs(
    fixture$raw_vs,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  expect_identical(
    fixture$raw_vs,
    original_raw_vs
  )

  expect_identical(
    fixture$dm,
    original_dm
  )
})


# ------------------------------------------------------------
# 16. Final integration validation
# ------------------------------------------------------------

test_that("Metadata-driven VS passes complete domain validation", {

  fixture <- make_metadata_vs_fixture()

  vs <- generate_vs(
    fixture$raw_vs,
    fixture$dm,
    mapping_path = fixture$mapping_path
  )

  expect_true(
    validate_dm(fixture$dm)
  )

  expect_true(
    validate_vs(vs, fixture$dm)
  )

  expect_equal(
    nrow(vs),
    nrow(fixture$raw_vs)
  )

  expect_equal(
    anyDuplicated(
      vs[c("STUDYID", "USUBJID", "VSSEQ")]
    ),
    0L
  )
})


# ============================================================
# End of Metadata-Driven VS Integration Tests
# ============================================================
