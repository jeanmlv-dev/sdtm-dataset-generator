
# ============================================================
# Automated validation tests for the SDTM-like LB domain
# ============================================================

make_valid_lb_fixture <- function() {
  raw_dm <- readr::read_csv(
    testthat::test_path("fixtures", "raw_dm.csv"),
    col_types = readr::cols(
      subject_id = readr::col_character(),
      site_id = readr::col_character(),
      .default = readr::col_guess()
    ),
    show_col_types = FALSE
  )

  raw_lb <- readr::read_csv(
    testthat::test_path("fixtures", "raw_lb.csv"),
    col_types = readr::cols(
      subject_id = readr::col_character(),
      site_id = readr::col_character(),
      .default = readr::col_guess()
    ),
    show_col_types = FALSE
  )

  dm <- generate_dm(raw_dm, "STUDY001")
  lb <- generate_lb(raw_lb, dm)

  list(dm = dm, lb = lb)
}

test_that("valid LB data passes validation", {
  fixture <- make_valid_lb_fixture()

  expect_true(
    validate_lb(fixture$lb, fixture$dm)
  )

  expect_equal(nrow(fixture$lb), 60L)
  expect_equal(ncol(fixture$lb), 22L)
})

test_that("LB validation rejects empty datasets", {
  fixture <- make_valid_lb_fixture()

  expect_error(
    validate_lb(fixture$lb[0, ], fixture$dm),
    "non-empty"
  )
})

test_that("LB validation rejects missing required variables", {
  fixture <- make_valid_lb_fixture()

  invalid_lb <- fixture$lb
  invalid_lb$LBTESTCD <- NULL

  expect_error(
    validate_lb(invalid_lb, fixture$dm),
    "Missing LB variables"
  )
})

test_that("LB validation rejects invalid domain", {
  fixture <- make_valid_lb_fixture()

  invalid_lb <- fixture$lb
  invalid_lb$DOMAIN[1] <- "AE"

  expect_error(
    validate_lb(invalid_lb, fixture$dm),
    "DOMAIN"
  )
})

test_that("LB validation rejects duplicate primary keys", {
  fixture <- make_valid_lb_fixture()

  invalid_lb <- fixture$lb
  invalid_lb$LBSEQ[2] <- invalid_lb$LBSEQ[1]
  invalid_lb$USUBJID[2] <- invalid_lb$USUBJID[1]

  expect_error(
    validate_lb(invalid_lb, fixture$dm),
    "Duplicate LB primary keys"
  )
})

test_that("LB validation rejects subjects absent from DM", {
  fixture <- make_valid_lb_fixture()

  invalid_lb <- fixture$lb
  invalid_lb$USUBJID[1] <- "STUDY001-999-999"

  expect_error(
    validate_lb(invalid_lb, fixture$dm),
    "LB subjects not found in DM"
  )
})

test_that("LB validation rejects unsupported laboratory tests", {
  fixture <- make_valid_lb_fixture()

  invalid_lb <- fixture$lb
  invalid_lb$LBTESTCD[1] <- "INVALID"

  expect_error(
    validate_lb(invalid_lb, fixture$dm),
    "LBTESTCD|Unsupported"
  )
})

test_that("LB validation rejects invalid numeric results", {
  fixture <- make_valid_lb_fixture()

  invalid_lb <- fixture$lb
  invalid_lb$LBSTRESN[1] <- Inf

  expect_error(
    validate_lb(invalid_lb, fixture$dm),
    "LBSTRESN"
  )
})

test_that("LB validation rejects inconsistent reference indicators", {
  fixture <- make_valid_lb_fixture()

  invalid_lb <- fixture$lb
  invalid_lb$LBNRIND[1] <- "INVALID"

  expect_error(
    validate_lb(invalid_lb, fixture$dm),
    "LBNRIND"
  )
})

test_that("LB validation rejects inconsistent standardized results", {
  fixture <- make_valid_lb_fixture()

  invalid_lb <- fixture$lb
  invalid_lb$LBSTRESN[1] <- -999

  expect_error(
    validate_lb(invalid_lb, fixture$dm),
    "LBSTRESN"
  )
})

test_that("LB validation rejects invalid collection dates", {
  fixture <- make_valid_lb_fixture()

  invalid_lb <- fixture$lb
  invalid_lb$LBDTC[1] <- "2026-99-99"

  expect_error(
    validate_lb(invalid_lb, fixture$dm),
    "LBDTC"
  )
})

test_that("LB validation rejects inconsistent study days", {
  fixture <- make_valid_lb_fixture()

  invalid_lb <- fixture$lb
  invalid_lb$LBDY[1] <- 999

  expect_error(
    validate_lb(invalid_lb, fixture$dm),
    "LBDY"
  )
})
