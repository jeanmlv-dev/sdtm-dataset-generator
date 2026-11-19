
make_ae_pipeline_inputs <- function(dir) {
  dm_path <- file.path(dir, "raw_dm.csv")
  ae_path <- file.path(dir, "raw_ae.csv")

  dm_source <- testthat::test_path(
    "fixtures", "raw_dm.csv"
  )

  ae_source <- testthat::test_path(
    "fixtures", "raw_ae.csv"
  )

  if (!file.copy(dm_source, dm_path)) {
    stop("Failed to copy DM test fixture.")
  }

  if (!file.copy(ae_source, ae_path)) {
    stop("Failed to copy AE test fixture.")
  }

  list(
    dm = dm_path,
    ae = ae_path
  )
}

test_that("AE pipeline generates validated artifacts", {
  temp_dir <- withr::local_tempdir()
  inputs <- make_ae_pipeline_inputs(temp_dir)
  output_dir <- file.path(temp_dir, "outputs")

  result <- run_ae_pipeline(
    ae_input_path = inputs$ae,
    dm_input_path = inputs$dm,
    study_id = "STUDY001",
    output_dir = output_dir
  )

  expect_equal(nrow(result$dm), 20L)
  expect_equal(nrow(result$ae), 10L)
  expect_equal(ncol(result$ae), 14L)

  expect_true(all(result$validation_report$STATUS == "PASS"))
  expect_equal(nrow(result$validation_report), 10L)

  expect_true(file.exists(result$dm_path))
  expect_true(file.exists(result$ae_path))
  expect_true(file.exists(result$report_path))
})

test_that("AE pipeline preserves referential integrity", {
  temp_dir <- withr::local_tempdir()
  inputs <- make_ae_pipeline_inputs(temp_dir)

  result <- run_ae_pipeline(
    ae_input_path = inputs$ae,
    dm_input_path = inputs$dm,
    study_id = "STUDY001",
    output_dir = file.path(temp_dir, "outputs")
  )

  expect_true(
    all(result$ae$USUBJID %in% result$dm$USUBJID)
  )

  expect_equal(
    anyDuplicated(result$ae[c("USUBJID", "AESEQ")]),
    0L
  )
})

test_that("AE pipeline rejects missing input files", {
  temp_dir <- withr::local_tempdir()

  expect_error(
    run_ae_pipeline(
      ae_input_path = file.path(temp_dir, "missing.csv"),
      dm_input_path = file.path(temp_dir, "missing_dm.csv"),
      study_id = "STUDY001",
      output_dir = file.path(temp_dir, "outputs")
    ),
    "Input CSV file does not exist"
  )
})

test_that("AE pipeline does not export invalid AE data", {
  temp_dir <- withr::local_tempdir()
  inputs <- make_ae_pipeline_inputs(temp_dir)
  output_dir <- file.path(temp_dir, "outputs")

  raw_ae <- readr::read_csv(
    inputs$ae,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )

  raw_ae$subject_id[1] <- "999"

  readr::write_csv(raw_ae, inputs$ae)

  expect_error(
    run_ae_pipeline(
      ae_input_path = inputs$ae,
      dm_input_path = inputs$dm,
      study_id = "STUDY001",
      output_dir = output_dir
    )
  )

  expect_false(file.exists(file.path(output_dir, "ae.csv")))
  expect_false(file.exists(file.path(output_dir, "dm.csv")))
})
