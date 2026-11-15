
make_raw_dm <- function() {
  data.frame(
    subject_id = c("001", "002", "003"),
    site_id = c("101", "101", "102"),
    age = c(35, 48, 62),
    sex = c("M", "F", "M"),
    arm = c("Placebo", "Active", "Placebo"),
    rfstdtc = c(
      "2026-01-01",
      "2026-01-02",
      "2026-01-03"
    ),
    stringsAsFactors = FALSE
  )
}

test_that("DM generation produces expected variables", {
  dm <- generate_dm(make_raw_dm(), "STUDY001")

  expect_s3_class(dm, "data.frame")
  expect_equal(nrow(dm), 3L)
  expect_true(all(c(
    "STUDYID", "DOMAIN", "USUBJID",
    "SUBJID", "SITEID", "RFSTDTC",
    "AGE", "AGEU", "SEX", "ARMCD", "ARM"
  ) %in% names(dm)))
})

test_that("DM identifiers are correctly generated", {
  dm <- generate_dm(make_raw_dm(), "STUDY001")

  expect_equal(dm$USUBJID[1], "STUDY001-101-001")
  expect_equal(dm$SUBJID[1], "001")
  expect_equal(dm$SITEID[1], "101")
})

test_that("DM domain and study identifiers are consistent", {
  dm <- generate_dm(make_raw_dm(), "STUDY001")

  expect_true(all(dm$DOMAIN == "DM"))
  expect_true(all(dm$STUDYID == "STUDY001"))
})

test_that("DM arm mapping is correct", {
  dm <- generate_dm(make_raw_dm(), "STUDY001")

  expect_equal(dm$ARMCD, c("PBO", "ACT", "PBO"))
})

test_that("DM validation succeeds for valid data", {
  dm <- generate_dm(make_raw_dm(), "STUDY001")

  expect_true(validate_dm(dm))
})

test_that("DM generation rejects duplicate subjects", {
  raw <- make_raw_dm()
  raw$subject_id[2] <- raw$subject_id[1]

  expect_error(
    generate_dm(raw, "STUDY001"),
    "Duplicate subject_id"
  )
})

test_that("DM generation rejects invalid sex values", {
  raw <- make_raw_dm()
  raw$sex[1] <- "INVALID"

  expect_error(
    generate_dm(raw, "STUDY001"),
    "sex must contain"
  )
})

test_that("DM generation rejects missing columns", {
  raw <- make_raw_dm()
  raw$site_id <- NULL

  expect_error(
    generate_dm(raw, "STUDY001"),
    "Missing required columns"
  )
})

test_that("DM generation rejects invalid dates", {
  raw <- make_raw_dm()
  raw$rfstdtc[1] <- "2026-02-30"

  expect_error(
    generate_dm(raw, "STUDY001"),
    "rfstdtc must contain"
  )
})

test_that("DM validation rejects duplicate USUBJID", {
  dm <- generate_dm(make_raw_dm(), "STUDY001")
  dm$USUBJID[2] <- dm$USUBJID[1]

  expect_error(
    validate_dm(dm),
    "USUBJID must be unique"
  )
})
