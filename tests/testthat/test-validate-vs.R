
# Tests for SDTM-like VS validation

# Locate example datasets in the installed package or source tree.
vs_fixture_path <- function(filename) {
  path <- system.file(
    "extdata",
    filename,
    package = "sdtmgenerator"
  )

  if (!nzchar(path)) {
    path <- testthat::test_path(
      "..", "..", "inst", "extdata", filename
    )
  }

  if (!file.exists(path)) {
    stop(
      paste("VS fixture file not found:", filename),
      call. = FALSE
    )
  }

  path
}

make_valid_vs_fixture <- function() {
  dm_path <- testthat::test_path(
    "..", "..", "inst", "extdata", "raw_dm.csv"
  )

  vs_path <- testthat::test_path(
    "..", "..", "inst", "extdata", "raw_vs.csv"
  )

  raw_dm <- readr::read_csv(
    vs_fixture_path("raw_dm.csv"),
    col_types = readr::cols(
      subject_id = readr::col_character(),
      site_id = readr::col_character(),
      .default = readr::col_guess()
    ),
    show_col_types = FALSE
  )

  raw_vs <- readr::read_csv(
    vs_fixture_path("raw_vs.csv"),
    col_types = readr::cols(
      subject_id = readr::col_character(),
      site_id = readr::col_character(),
      .default = readr::col_guess()
    ),
    show_col_types = FALSE
  )

  dm <- generate_dm(raw_dm, "STUDY001")
  vs <- generate_vs(raw_vs, dm)

  list(dm = dm, vs = vs)
}

test_that("valid VS data passes validation", {
  fixture <- make_valid_vs_fixture()

  expect_true(
    validate_vs(fixture$vs, fixture$dm)
  )
})

test_that("VS validation rejects empty datasets", {
  fixture <- make_valid_vs_fixture()

  expect_error(
    validate_vs(
      fixture$vs[0, ],
      fixture$dm
    ),
    "non-empty data.frame"
  )
})

test_that("VS validation rejects missing variables", {
  fixture <- make_valid_vs_fixture()
  vs <- fixture$vs

  vs$VSSTRESN <- NULL

  expect_error(
    validate_vs(vs, fixture$dm),
    "Missing VS variables"
  )
})

test_that("VS validation rejects invalid domain", {
  fixture <- make_valid_vs_fixture()
  vs <- fixture$vs

  vs$DOMAIN[1] <- "AE"

  expect_error(
    validate_vs(vs, fixture$dm),
    "DOMAIN must be VS"
  )
})

test_that("VS validation rejects missing identifiers", {
  fixture <- make_valid_vs_fixture()
  vs <- fixture$vs

  vs$USUBJID[1] <- NA_character_

  expect_error(
    validate_vs(vs, fixture$dm),
    "USUBJID contains missing or invalid values"
  )
})

test_that("VS validation rejects subjects absent from DM", {
  fixture <- make_valid_vs_fixture()
  vs <- fixture$vs

  vs$USUBJID[1] <- "STUDY001-999-999"

  expect_error(
    validate_vs(vs, fixture$dm),
    "VS subjects not found in DM"
  )
})

test_that("VS validation rejects inconsistent study IDs", {
  fixture <- make_valid_vs_fixture()
  vs <- fixture$vs

  vs$STUDYID[1] <- "OTHER_STUDY"

  expect_error(
    validate_vs(vs, fixture$dm),
    "STUDYID is inconsistent with DM"
  )
})

test_that("VS validation rejects invalid VSSEQ", {
  fixture <- make_valid_vs_fixture()
  vs <- fixture$vs

  vs$VSSEQ[1] <- -1

  expect_error(
    validate_vs(vs, fixture$dm),
    "VSSEQ must contain positive integers"
  )
})

test_that("VS validation rejects duplicate primary keys", {
  fixture <- make_valid_vs_fixture()
  vs <- fixture$vs

  vs <- rbind(vs, vs[1, , drop = FALSE])

  expect_error(
    validate_vs(vs, fixture$dm),
    "Duplicate VS primary keys"
  )
})

test_that("VS validation rejects unsupported test codes", {
  fixture <- make_valid_vs_fixture()
  vs <- fixture$vs

  vs$VSTESTCD[1] <- "INVALID"

  expect_error(
    validate_vs(vs, fixture$dm),
    "Unsupported VSTESTCD"
  )
})

test_that("VS validation rejects inconsistent test names", {
  fixture <- make_valid_vs_fixture()
  vs <- fixture$vs

  vs$VSTEST[1] <- "Incorrect Test"

  expect_error(
    validate_vs(vs, fixture$dm),
    "VSTEST is inconsistent"
  )
})

test_that("VS validation rejects invalid visit numbers", {
  fixture <- make_valid_vs_fixture()
  vs <- fixture$vs

  vs$VISITNUM[1] <- -1

  expect_error(
    validate_vs(vs, fixture$dm),
    "VISITNUM must contain non-negative numbers"
  )
})

test_that("VS validation rejects inconsistent visit mapping", {
  fixture <- make_valid_vs_fixture()
  vs <- fixture$vs

  # Introduce a new visit label for an existing visit number.
  vs$VISIT[1] <- "UNEXPECTED VISIT"

  expect_error(
    validate_vs(vs, fixture$dm),
    "VISIT and VISITNUM mapping is inconsistent"
  )
})

test_that("VS validation rejects invalid numeric results", {
  fixture <- make_valid_vs_fixture()
  vs <- fixture$vs

  vs$VSSTRESN[1] <- Inf

  expect_error(
    validate_vs(vs, fixture$dm),
    "VSSTRESN must contain finite numeric values"
  )
})

test_that("VS validation rejects incompatible units", {
  fixture <- make_valid_vs_fixture()
  vs <- fixture$vs

  vs$VSORRESU[1] <- "INVALID_UNIT"

  expect_error(
    validate_vs(vs, fixture$dm),
    "VSORRESU is incompatible"
  )
})

test_that("VS validation rejects incorrect standard units", {
  fixture <- make_valid_vs_fixture()
  vs <- fixture$vs

  vs$VSSTRESU[1] <- "INVALID_UNIT"

  expect_error(
    validate_vs(vs, fixture$dm),
    "VSSTRESU is inconsistent"
  )
})

test_that("VS validation rejects inconsistent numeric conversions", {
  fixture <- make_valid_vs_fixture()
  vs <- fixture$vs

  vs$VSSTRESN[1] <- vs$VSSTRESN[1] + 10

  expect_error(
    validate_vs(vs, fixture$dm),
    "VSSTRESN is inconsistent"
  )
})

test_that("VS validation rejects inconsistent character results", {
  fixture <- make_valid_vs_fixture()
  vs <- fixture$vs

  vs$VSSTRESC[1] <- "99999"

  expect_error(
    validate_vs(vs, fixture$dm),
    "VSSTRESC is inconsistent"
  )
})

test_that("VS validation rejects invalid dates", {
  fixture <- make_valid_vs_fixture()
  vs <- fixture$vs

  vs$VSDTC[1] <- "2026-02-30"

  expect_error(
    validate_vs(vs, fixture$dm),
    "VSDTC"
  )
})

test_that("VS validation rejects inconsistent study days", {
  fixture <- make_valid_vs_fixture()
  vs <- fixture$vs

  if (is.na(vs$VSSTDY[1])) {
    skip("First VS record has no reference date.")
  }

  vs$VSSTDY[1] <- vs$VSSTDY[1] + 1

  expect_error(
    validate_vs(vs, fixture$dm),
    "VSSTDY is inconsistent"
  )
})

test_that("VS validation rejects inconsistent missing study days", {
  fixture <- make_valid_vs_fixture()
  vs <- fixture$vs

  index <- which(!is.na(vs$VSSTDY))[1]

  if (is.na(index)) {
    skip("No VS records have reference dates.")
  }

  vs$VSSTDY[index] <- NA_real_

  expect_error(
    validate_vs(vs, fixture$dm),
    "VSSTDY is inconsistent"
  )
})
