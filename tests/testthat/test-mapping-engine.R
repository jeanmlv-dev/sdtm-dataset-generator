
# ============================================================
# Metadata-Driven Mapping Engine - Automated Tests
#
# Package: sdtmgenerator
#
# Tests:
#   1. Mapping metadata ingestion
#   2. Direct transformations
#   3. Constant transformations
#   4. Controlled terminology transformations
#   5. Required target variables
#   6. Invalid metadata and source data
#   7. Mapping validation
#   8. Deterministic output
#
# All fixtures are synthetic and isolated.
# ============================================================


# ------------------------------------------------------------
# 1. Test fixtures
# ------------------------------------------------------------

make_mapping_raw_data <- function() {
  data.frame(
    subject_id = c("001", "002", "003", "004"),
    site_id = c("101", "101", "102", "102"),
    age = c(34, 45, 52, 29),
    sex = c("M", "F", "Male", "Female"),
    arm = c("Active", "Placebo", "Active", "Placebo"),
    stringsAsFactors = FALSE
  )
}


make_mapping_metadata <- function() {
  data.frame(
    domain = rep("DM", 7L),
    source_variable = c(
      NA_character_,
      NA_character_,
      "subject_id",
      "site_id",
      "age",
      "sex",
      "arm"
    ),
    target_variable = c(
      "STUDYID",
      "DOMAIN",
      "USUBJID",
      "SITEID",
      "AGE",
      "SEX",
      "ARM"
    ),
    transformation = c(
      "constant",
      "constant",
      "direct",
      "direct",
      "direct",
      "terminology_map",
      "direct"
    ),
    parameter = c(
      "STUDY001",
      "DM",
      NA_character_,
      NA_character_,
      NA_character_,
      "sex",
      NA_character_
    ),
    required = rep(TRUE, 7L),
    stringsAsFactors = FALSE
  )
}


make_mapping_terminology <- function() {
  list(
    sex = c(
      "M" = "M",
      "F" = "F",
      "Male" = "M",
      "Female" = "F"
    )
  )
}


make_mapping_fixture <- function() {
  list(
    raw_data = make_mapping_raw_data(),
    mapping = make_mapping_metadata(),
    terminology = make_mapping_terminology(),
    study_id = "STUDY001"
  )
}


# ------------------------------------------------------------
# 2. Mapping ingestion
# ------------------------------------------------------------

test_that("Mapping engine reads valid metadata", {
  fixture <- make_mapping_fixture()

  mapping_path <- withr::local_tempfile(
    fileext = ".csv"
  )

  readr::write_csv(
    fixture$mapping,
    mapping_path
  )

  result <- read_mapping(mapping_path)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 7L)

  expect_true(
    all(c(
      "domain",
      "source_variable",
      "target_variable",
      "transformation",
      "parameter",
      "required"
    ) %in% names(result))
  )

  expect_equal(
    result$target_variable,
    fixture$mapping$target_variable
  )
})


test_that("Mapping engine rejects missing metadata files", {
  missing_path <- tempfile(fileext = ".csv")

  expect_error(
    read_mapping(missing_path)
  )
})


test_that("Mapping engine rejects incomplete metadata", {
  fixture <- make_mapping_fixture()

  invalid_mapping <- fixture$mapping
  invalid_mapping$target_variable <- NULL

  mapping_path <- withr::local_tempfile(
    fileext = ".csv"
  )

  readr::write_csv(
    invalid_mapping,
    mapping_path
  )

  expect_error(
    read_mapping(mapping_path)
  )
})


# ------------------------------------------------------------
# 3. Mapping validation - positive scenario
# ------------------------------------------------------------

test_that("Valid mapping metadata passes validation", {
  fixture <- make_mapping_fixture()

  result <- validate_mapping(
    mapping = fixture$mapping,
    raw_data = fixture$raw_data,
    terminology = fixture$terminology,
    study_id = fixture$study_id
  )

  expect_true(result)
})


# ------------------------------------------------------------
# 4. Direct and constant transformations
# ------------------------------------------------------------

test_that("Mapping engine applies direct transformations", {
  fixture <- make_mapping_fixture()

  result <- apply_mapping(
    raw_data = fixture$raw_data,
    mapping = fixture$mapping,
    study_id = fixture$study_id,
    terminology = fixture$terminology
  )

  expect_equal(
    result$USUBJID,
    fixture$raw_data$subject_id
  )

  expect_equal(
    result$SITEID,
    fixture$raw_data$site_id
  )

  expect_equal(
    result$AGE,
    fixture$raw_data$age
  )

  expect_equal(
    result$ARM,
    fixture$raw_data$arm
  )
})


test_that("Mapping engine applies constant transformations", {
  fixture <- make_mapping_fixture()

  result <- apply_mapping(
    raw_data = fixture$raw_data,
    mapping = fixture$mapping,
    study_id = fixture$study_id,
    terminology = fixture$terminology
  )

  expect_true(
    all(result$STUDYID == "STUDY001")
  )

  expect_true(
    all(result$DOMAIN == "DM")
  )
})


# ------------------------------------------------------------
# 5. Controlled terminology
# ------------------------------------------------------------

test_that("Mapping engine applies controlled terminology", {
  fixture <- make_mapping_fixture()

  result <- apply_mapping(
    raw_data = fixture$raw_data,
    mapping = fixture$mapping,
    study_id = fixture$study_id,
    terminology = fixture$terminology
  )

  expect_equal(
    result$SEX,
    c("M", "F", "M", "F")
  )
})


test_that("Mapping engine rejects unmapped terminology", {
  fixture <- make_mapping_fixture()

  fixture$raw_data$sex[1] <- "UNKNOWN"

  expect_error(
    apply_mapping(
      raw_data = fixture$raw_data,
      mapping = fixture$mapping,
      study_id = fixture$study_id,
      terminology = fixture$terminology
    )
  )
})


# ------------------------------------------------------------
# 6. Required variables
# ------------------------------------------------------------

test_that("Mapping engine rejects missing required source variables", {
  fixture <- make_mapping_fixture()

  fixture$raw_data$subject_id <- NULL

  expect_error(
    validate_mapping(
      mapping = fixture$mapping,
      raw_data = fixture$raw_data,
      terminology = fixture$terminology,
      study_id = fixture$study_id
    )
  )
})


test_that("Mapping engine rejects missing required source values", {
  fixture <- make_mapping_fixture()

  fixture$raw_data$subject_id[1] <- NA_character_

  expect_error(
    validate_mapping(
      mapping = fixture$mapping,
      raw_data = fixture$raw_data,
      terminology = fixture$terminology,
      study_id = fixture$study_id
    )
  )
})


# ------------------------------------------------------------
# 7. Invalid mapping metadata
# ------------------------------------------------------------

test_that("Mapping engine rejects unsupported transformations", {
  fixture <- make_mapping_fixture()

  fixture$mapping$transformation[5] <- "invalid_rule"

  expect_error(
    validate_mapping(
      mapping = fixture$mapping,
      raw_data = fixture$raw_data,
      terminology = fixture$terminology,
      study_id = fixture$study_id
    )
  )
})


test_that("Mapping engine rejects duplicate target variables", {
  fixture <- make_mapping_fixture()

  fixture$mapping$target_variable[7] <- "AGE"

  expect_error(
    validate_mapping(
      mapping = fixture$mapping,
      raw_data = fixture$raw_data,
      terminology = fixture$terminology,
      study_id = fixture$study_id
    )
  )
})


test_that("Mapping engine rejects missing terminology dictionaries", {
  fixture <- make_mapping_fixture()

  expect_error(
    validate_mapping(
      mapping = fixture$mapping,
      raw_data = fixture$raw_data,
      terminology = list(),
      study_id = fixture$study_id
    )
  )
})


test_that("Mapping engine rejects missing transformation parameters", {
  fixture <- make_mapping_fixture()

  fixture$mapping$parameter[6] <- NA_character_

  expect_error(
    validate_mapping(
      mapping = fixture$mapping,
      raw_data = fixture$raw_data,
      terminology = fixture$terminology,
      study_id = fixture$study_id
    )
  )
})


# ------------------------------------------------------------
# 8. Output integrity
# ------------------------------------------------------------

test_that("Mapping engine preserves source row count", {
  fixture <- make_mapping_fixture()

  result <- apply_mapping(
    raw_data = fixture$raw_data,
    mapping = fixture$mapping,
    study_id = fixture$study_id,
    terminology = fixture$terminology
  )

  expect_equal(
    nrow(result),
    nrow(fixture$raw_data)
  )

  expect_equal(
    ncol(result),
    nrow(fixture$mapping)
  )
})


test_that("Mapping engine produces expected target variables", {
  fixture <- make_mapping_fixture()

  result <- apply_mapping(
    raw_data = fixture$raw_data,
    mapping = fixture$mapping,
    study_id = fixture$study_id,
    terminology = fixture$terminology
  )

  expect_identical(
    names(result),
    fixture$mapping$target_variable
  )
})


test_that("Mapping engine produces deterministic results", {
  fixture <- make_mapping_fixture()

  first_result <- apply_mapping(
    raw_data = fixture$raw_data,
    mapping = fixture$mapping,
    study_id = fixture$study_id,
    terminology = fixture$terminology
  )

  second_result <- apply_mapping(
    raw_data = fixture$raw_data,
    mapping = fixture$mapping,
    study_id = fixture$study_id,
    terminology = fixture$terminology
  )

  expect_identical(
    first_result,
    second_result
  )
})


# ------------------------------------------------------------
# 9. Source immutability
# ------------------------------------------------------------

test_that("Mapping engine does not modify source data", {
  fixture <- make_mapping_fixture()

  original_data <- fixture$raw_data

  apply_mapping(
    raw_data = fixture$raw_data,
    mapping = fixture$mapping,
    study_id = fixture$study_id,
    terminology = fixture$terminology
  )

  expect_identical(
    fixture$raw_data,
    original_data
  )
})


# ============================================================
# End of mapping engine tests
# ============================================================
