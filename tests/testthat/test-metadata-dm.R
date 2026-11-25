
# ============================================================
# Metadata-Driven DM Integration Tests
#
# Package: sdtmgenerator
#
# Tests:
#   1. Metadata mapping can be loaded
#   2. Legacy and metadata-driven outputs are identical
#   3. Metadata-driven DM passes domain validation
#   4. Repeated executions are deterministic
#   5. Input data remains unchanged
#   6. Missing mapping files are rejected
#   7. Incomplete mappings are rejected
#   8. Invalid mapping definitions are rejected
#   9. Legacy behavior remains backward compatible
#  10. Invalid source data is rejected
# ============================================================


# ------------------------------------------------------------
# 1. Reusable fixture
# ------------------------------------------------------------

make_metadata_dm_fixture <- function() {

  dm_path <- testthat::test_path(
    "fixtures", "raw_dm.csv"
  )

  # Use the package's installed example data when the
  # fixture is not available in the test directory.
  if (!file.exists(dm_path)) {
    dm_path <- system.file(
      "extdata", "raw_dm.csv",
      package = "sdtmgenerator"
    )
  }

  if (!nzchar(dm_path) || !file.exists(dm_path)) {
    stop("DM test fixture was not found.", call. = FALSE)
  }

  raw_dm <- readr::read_csv(
    dm_path,
    col_types = readr::cols(
      subject_id = readr::col_character(),
      site_id = readr::col_character(),
      .default = readr::col_guess()
    ),
    show_col_types = FALSE
  )

  list(
    raw_data = raw_dm,
    study_id = "STUDY001"
  )
}


# ------------------------------------------------------------
# 2. Reusable metadata fixture
# ------------------------------------------------------------

make_dm_mapping_fixture <- function() {

  # The mapping is represented in memory to keep tests
  # independent of the project's working directory.
  data.frame(
    domain = rep("DM", 11L),

    source_variable = c(
      NA, NA, "subject_id", "subject_id",
      "site_id", "rfstdtc", "age", NA,
      "sex", "arm", "arm"
    ),

    target_variable = c(
      "STUDYID", "DOMAIN", "USUBJID", "SUBJID",
      "SITEID", "RFSTDTC", "AGE", "AGEU",
      "SEX", "ARMCD", "ARM"
    ),

    transformation = c(
      "constant", "constant", "direct", "direct",
      "direct", "direct", "direct", "constant",
      "terminology_map", "terminology_map", "direct"
    ),

    parameter = c(
      "study_id", "DM", NA, NA,
      NA, NA, NA, "YEARS",
      "sex", "arm", NA
    ),

    required = c(
      TRUE, TRUE, TRUE, TRUE,
      TRUE, FALSE, FALSE, TRUE,
      TRUE, TRUE, TRUE
    ),

    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------
# 3. Temporary mapping helper
# ------------------------------------------------------------

write_dm_mapping_fixture <- function(mapping) {

  path <- tempfile(fileext = ".csv")

  readr::write_csv(
    mapping,
    path,
    na = "NA"
  )

  path
}


# ------------------------------------------------------------
# 4. Mapping file can be loaded
# ------------------------------------------------------------

test_that("DM metadata mapping can be loaded", {

  mapping_path <- write_dm_mapping_fixture(
    make_dm_mapping_fixture()
  )

  on.exit(unlink(mapping_path), add = TRUE)

  mapping <- read_mapping(mapping_path)

  expect_s3_class(mapping, "data.frame")
  expect_equal(nrow(mapping), 11L)
  expect_true(all(mapping$domain == "DM"))

  expect_setequal(
    mapping$target_variable,
    c(
      "STUDYID", "DOMAIN", "USUBJID",
      "SUBJID", "SITEID", "RFSTDTC",
      "AGE", "AGEU", "SEX", "ARMCD", "ARM"
    )
  )
})


# ------------------------------------------------------------
# 5. Legacy and metadata-driven equivalence
# ------------------------------------------------------------

test_that(
  "Metadata-driven DM matches legacy DM exactly",
  {

    fixture <- make_metadata_dm_fixture()

    mapping_path <- write_dm_mapping_fixture(
      make_dm_mapping_fixture()
    )

    on.exit(unlink(mapping_path), add = TRUE)

    dm_legacy <- generate_dm(
      raw_data = fixture$raw_data,
      study_id = fixture$study_id
    )

    dm_metadata <- generate_dm(
      raw_data = fixture$raw_data,
      study_id = fixture$study_id,
      mapping_path = mapping_path
    )

    expect_identical(
      dm_metadata,
      dm_legacy
    )
  }
)


# ------------------------------------------------------------
# 6. Domain validation
# ------------------------------------------------------------

test_that(
  "Metadata-driven DM passes DM validation",
  {

    fixture <- make_metadata_dm_fixture()

    mapping_path <- write_dm_mapping_fixture(
      make_dm_mapping_fixture()
    )

    on.exit(unlink(mapping_path), add = TRUE)

    dm <- generate_dm(
      raw_data = fixture$raw_data,
      study_id = fixture$study_id,
      mapping_path = mapping_path
    )

    expect_true(validate_dm(dm))
    expect_true(all(dm$DOMAIN == "DM"))
    expect_true(all(dm$STUDYID == fixture$study_id))

    expect_equal(
      nrow(dm),
      nrow(fixture$raw_data)
    )

    expect_equal(
      length(unique(dm$USUBJID)),
      nrow(dm)
    )
  }
)


# ------------------------------------------------------------
# 7. Deterministic execution
# ------------------------------------------------------------

test_that(
  "Metadata-driven DM generation is deterministic",
  {

    fixture <- make_metadata_dm_fixture()

    mapping_path <- write_dm_mapping_fixture(
      make_dm_mapping_fixture()
    )

    on.exit(unlink(mapping_path), add = TRUE)

    first_run <- generate_dm(
      fixture$raw_data,
      fixture$study_id,
      mapping_path = mapping_path
    )

    second_run <- generate_dm(
      fixture$raw_data,
      fixture$study_id,
      mapping_path = mapping_path
    )

    expect_identical(
      first_run,
      second_run
    )
  }
)


# ------------------------------------------------------------
# 8. Input immutability
# ------------------------------------------------------------

test_that(
  "Metadata-driven DM does not modify source data",
  {

    fixture <- make_metadata_dm_fixture()

    original_data <- fixture$raw_data

    mapping_path <- write_dm_mapping_fixture(
      make_dm_mapping_fixture()
    )

    on.exit(unlink(mapping_path), add = TRUE)

    generate_dm(
      raw_data = fixture$raw_data,
      study_id = fixture$study_id,
      mapping_path = mapping_path
    )

    expect_identical(
      fixture$raw_data,
      original_data
    )
  }
)


# ------------------------------------------------------------
# 9. Missing mapping file
# ------------------------------------------------------------

test_that(
  "Metadata-driven DM rejects missing mapping files",
  {

    fixture <- make_metadata_dm_fixture()

    missing_path <- tempfile(
      pattern = "missing_dm_mapping_",
      fileext = ".csv"
    )

    expect_error(
      generate_dm(
        raw_data = fixture$raw_data,
        study_id = fixture$study_id,
        mapping_path = missing_path
      )
    )
  }
)


# ------------------------------------------------------------
# 10. Incomplete mapping
# ------------------------------------------------------------

test_that(
  "Metadata-driven DM rejects missing target variables",
  {

    fixture <- make_metadata_dm_fixture()

    mapping <- make_dm_mapping_fixture()

    mapping <- mapping[
      mapping$target_variable != "ARMCD",
      ,
      drop = FALSE
    ]

    mapping_path <- write_dm_mapping_fixture(mapping)

    on.exit(unlink(mapping_path), add = TRUE)

    expect_error(
      generate_dm(
        raw_data = fixture$raw_data,
        study_id = fixture$study_id,
        mapping_path = mapping_path
      ),
      "Missing DM mapping targets"
    )
  }
)


# ------------------------------------------------------------
# 11. Invalid mapping definition
# ------------------------------------------------------------

test_that(
  "Metadata-driven DM rejects unsupported transformations",
  {

    fixture <- make_metadata_dm_fixture()

    mapping <- make_dm_mapping_fixture()

    mapping$transformation[
      mapping$target_variable == "AGE"
    ] <- "unsupported_transformation"

    mapping_path <- write_dm_mapping_fixture(mapping)

    on.exit(unlink(mapping_path), add = TRUE)

    expect_error(
      generate_dm(
        raw_data = fixture$raw_data,
        study_id = fixture$study_id,
        mapping_path = mapping_path
      )
    )
  }
)


# ------------------------------------------------------------
# 12. Backward compatibility
# ------------------------------------------------------------

test_that(
  "Legacy DM remains compatible with NULL mapping",
  {

    fixture <- make_metadata_dm_fixture()

    dm_default <- generate_dm(
      fixture$raw_data,
      fixture$study_id
    )

    dm_explicit_null <- generate_dm(
      fixture$raw_data,
      fixture$study_id,
      mapping_path = NULL
    )

    expect_identical(
      dm_default,
      dm_explicit_null
    )
  }
)


# ------------------------------------------------------------
# 13. Invalid source data
# ------------------------------------------------------------

test_that(
  "Metadata-driven DM rejects duplicate subjects",
  {

    fixture <- make_metadata_dm_fixture()

    invalid_data <- fixture$raw_data

    invalid_data$subject_id[2] <-
      invalid_data$subject_id[1]

    mapping_path <- write_dm_mapping_fixture(
      make_dm_mapping_fixture()
    )

    on.exit(unlink(mapping_path), add = TRUE)

    expect_error(
      generate_dm(
        raw_data = invalid_data,
        study_id = fixture$study_id,
        mapping_path = mapping_path
      ),
      "Duplicate subject_id"
    )
  }
)


# ============================================================
# End of Metadata-Driven DM Integration Tests
# ============================================================
