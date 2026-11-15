
make_pipeline_input <- function() {

  raw <- data.frame(
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

  input_path <- tempfile(fileext = ".csv")

  readr::write_csv(raw, input_path)

  input_path
}


test_that("DM pipeline generates dataset and validation report", {

  input_path <- make_pipeline_input()
  output_dir <- tempfile("dm_output_")

  result <- run_dm_pipeline(
    input_path = input_path,
    study_id = "STUDY001",
    output_dir = output_dir
  )

  expect_s3_class(result$dm, "data.frame")

  expect_equal(nrow(result$dm), 3L)
  expect_equal(ncol(result$dm), 11L)

  expect_true(file.exists(result$dm_path))
  expect_true(file.exists(result$report_path))

  expect_equal(nrow(result$validation_report), 7L)

  expect_true(
    all(result$validation_report$STATUS == "PASS")
  )

  unlink(input_path)
  unlink(output_dir, recursive = TRUE)
})


test_that("DM pipeline preserves leading zeros in identifiers", {

  input_path <- make_pipeline_input()
  output_dir <- tempfile("dm_output_")

  result <- run_dm_pipeline(
    input_path,
    "STUDY001",
    output_dir
  )

  expect_equal(
    result$dm$SUBJID,
    c("001", "002", "003")
  )

  expect_equal(
    result$dm$USUBJID[1],
    "STUDY001-101-001"
  )

  exported_dm <- readr::read_csv(
    result$dm_path,
    col_types = readr::cols(
      .default = readr::col_character()
    ),
    show_col_types = FALSE
  )

  expect_equal(exported_dm$SUBJID[1], "001")

  unlink(input_path)
  unlink(output_dir, recursive = TRUE)
})


test_that("DM pipeline rejects a missing input file", {

  expect_error(
    run_dm_pipeline(
      tempfile(fileext = ".csv"),
      "STUDY001",
      tempfile("dm_output_")
    ),
    "Input CSV file does not exist"
  )
})


test_that("DM pipeline rejects invalid raw clinical data", {

  input_path <- make_pipeline_input()
  output_dir <- tempfile("dm_output_")

  raw <- readr::read_csv(
    input_path,
    col_types = readr::cols(
      subject_id = readr::col_character(),
      site_id = readr::col_character()
    ),
    show_col_types = FALSE
  )

  raw$subject_id[2] <- raw$subject_id[1]

  readr::write_csv(raw, input_path)

  expect_error(
    run_dm_pipeline(
      input_path,
      "STUDY001",
      output_dir
    ),
    "Duplicate subject_id"
  )

  expect_false(
    file.exists(file.path(output_dir, "dm.csv"))
  )

  unlink(input_path)
  unlink(output_dir, recursive = TRUE)
})


test_that("DM pipeline rejects CSV parsing errors", {

  input_path <- tempfile(fileext = ".csv")
  output_dir <- tempfile("dm_output_")

  writeLines(
    c(
      "subject_id,site_id,age,sex,arm,rfstdtc",
      "001,101,invalid,M,Placebo,2026-01-01"
    ),
    input_path
  )

  expect_error(
    suppressWarnings(
      run_dm_pipeline(
        input_path,
        "STUDY001",
        output_dir
      )
    ),
    "Input CSV contains parsing errors"
  )

  expect_false(
    file.exists(file.path(output_dir, "dm.csv"))
  )

  unlink(input_path)
  unlink(output_dir, recursive = TRUE)
})


test_that("DM pipeline produces consistent record counts", {

  input_path <- make_pipeline_input()
  output_dir <- tempfile("dm_output_")

  result <- run_dm_pipeline(
    input_path,
    "STUDY001",
    output_dir
  )

  expect_true(
    all(result$validation_report$RAW_RECORDS == 3L)
  )

  expect_true(
    all(result$validation_report$OUTPUT_RECORDS == 3L)
  )

  expect_true(
    all(result$validation_report$STUDYID == "STUDY001")
  )

  expect_true(
    all(result$validation_report$DOMAIN == "DM")
  )

  unlink(input_path)
  unlink(output_dir, recursive = TRUE)
})
