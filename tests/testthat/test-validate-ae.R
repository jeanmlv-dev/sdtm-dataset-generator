
make_valid_ae_fixture <- function() {
  raw_dm <- readr::read_csv(
    system.file(
      "extdata", "raw_dm.csv",
      package = "sdtmgenerator"
    ),
    col_types = readr::cols(
      subject_id = readr::col_character(),
      site_id = readr::col_character(),
      .default = readr::col_guess()
    ),
    show_col_types = FALSE
  )

  dm <- generate_dm(raw_dm, "STUDY001")

  raw_ae <- readr::read_csv(
    system.file(
      "extdata", "raw_ae.csv",
      package = "sdtmgenerator"
    ),
    col_types = readr::cols(
      subject_id = readr::col_character(),
      site_id = readr::col_character(),
      .default = readr::col_guess()
    ),
    show_col_types = FALSE
  )

  ae <- generate_ae(raw_ae, dm)

  list(ae = ae, dm = dm)
}

test_that("valid AE dataset passes validation", {
  fixture <- make_valid_ae_fixture()

  expect_true(validate_ae(fixture$ae, fixture$dm))
})

test_that("missing AE variables are rejected", {
  fixture <- make_valid_ae_fixture()
  ae <- fixture$ae

  ae$AESEQ <- NULL

  expect_error(
    validate_ae(ae),
    "Missing AE variables"
  )
})

test_that("duplicate AE primary keys are rejected", {
  fixture <- make_valid_ae_fixture()
  ae <- fixture$ae

  ae <- rbind(ae, ae[1, , drop = FALSE])

  expect_error(
    validate_ae(ae),
    "Duplicate AE primary keys"
  )
})

test_that("invalid AE severity is rejected", {
  fixture <- make_valid_ae_fixture()
  ae <- fixture$ae

  ae$AESEV[1] <- "EXTREME"

  expect_error(
    validate_ae(ae),
    "Invalid controlled terminology"
  )
})

test_that("end date before start date is rejected", {
  fixture <- make_valid_ae_fixture()
  ae <- fixture$ae

  ae$AEENDTC[1] <- "2020-01-01"

  expect_error(
    validate_ae(ae),
    "AEENDTC cannot precede"
  )
})

test_that("invalid calendar dates are rejected", {
  fixture <- make_valid_ae_fixture()
  ae <- fixture$ae

  ae$AESTDTC[1] <- "2026-02-30"

  expect_error(
    validate_ae(ae),
    "invalid ISO 8601 dates"
  )
})

test_that("AE subjects must exist in DM", {
  fixture <- make_valid_ae_fixture()
  ae <- fixture$ae

  ae$USUBJID[1] <- "STUDY001-999-999"

  expect_error(
    validate_ae(ae, fixture$dm),
    "subjects not found in DM"
  )
})


test_that("incorrect study days are rejected", {
  fixture <- make_valid_ae_fixture()
  ae <- fixture$ae

  # Introduce an incorrect study day without
  # violating the chronological order of the event.
  ae$AESTDY[1] <- ae$AESTDY[1] - 1L

  expect_error(
    validate_ae(ae, fixture$dm),
    "AESTDY is inconsistent"
  )
})
