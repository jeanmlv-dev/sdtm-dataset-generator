
# ============================================================
# End-to-end tests for the SDTM-like LB pipeline
# ============================================================

make_lb_pipeline_inputs <- function(dir) {
  dm_path <- file.path(dir, "raw_dm.csv")
  lb_path <- file.path(dir, "raw_lb.csv")

  dm_source <- testthat::test_path(
    "fixtures", "raw_dm.csv"
  )

  lb_source <- testthat::test_path(
    "fixtures", "raw_lb.csv"
  )

  stopifnot(
    file.exists(dm_source),
    file.exists(lb_source)
  )

  stopifnot(
    file.copy(dm_source, dm_path),
    file.copy(lb_source, lb_path)
  )

  list(dm = dm_path, lb = lb_path)
}

test_that("LB pipeline generates validated artifacts", {
  temp_dir <- withr::local_tempdir()
  inputs <- make_lb_pipeline_inputs(temp_dir)

  result <- run_lb_pipeline(
    lb_input_path = inputs$lb,
    dm_input_path = inputs$dm,
    study_id = "STUDY001",
    output_dir = file.path(temp_dir, "outputs")
  )

  expect_equal(nrow(result$dm), 20L)
  expect_equal(nrow(result$lb), 60L)
  expect_equal(ncol(result$lb), 22L)

  expect_equal(nrow(result$validation_report), 10L)
  expect_true(all(result$validation_report$STATUS == "PASS"))

  expect_true(file.exists(result$dm_path))
  expect_true(file.exists(result$lb_path))
  expect_true(file.exists(result$report_path))
})

test_that("LB pipeline preserves referential integrity", {
  temp_dir <- withr::local_tempdir()
  inputs <- make_lb_pipeline_inputs(temp_dir)

  result <- run_lb_pipeline(
    lb_input_path = inputs$lb,
    dm_input_path = inputs$dm,
    study_id = "STUDY001",
    output_dir = file.path(temp_dir, "outputs")
  )

  expect_true(
    all(result$lb$USUBJID %in% result$dm$USUBJID)
  )

  expect_equal(
    anyDuplicated(result$lb[c("USUBJID", "LBSEQ")]),
    0L
  )
})

test_that("LB pipeline rejects missing input files", {
  temp_dir <- withr::local_tempdir()

  expect_error(
    run_lb_pipeline(
      lb_input_path = file.path(temp_dir, "missing_lb.csv"),
      dm_input_path = file.path(temp_dir, "missing_dm.csv"),
      study_id = "STUDY001",
      output_dir = file.path(temp_dir, "outputs")
    ),
    "Input CSV file does not exist"
  )
})

test_that("LB pipeline does not export invalid data", {
  temp_dir <- withr::local_tempdir()
  inputs <- make_lb_pipeline_inputs(temp_dir)

  output_dir <- file.path(temp_dir, "outputs")

  raw_lb <- readr::read_csv(
    inputs$lb,
    col_types = readr::cols(
      subject_id = readr::col_character(),
      site_id = readr::col_character(),
      .default = readr::col_guess()
    ),
    show_col_types = FALSE
  )

  raw_lb$subject_id[1] <- "999"

  readr::write_csv(raw_lb, inputs$lb)

  expect_error(
    run_lb_pipeline(
      lb_input_path = inputs$lb,
      dm_input_path = inputs$dm,
      study_id = "STUDY001",
      output_dir = output_dir
    )
  )

  expect_false(
    file.exists(file.path(output_dir, "lb.csv"))
  )

  expect_false(
    file.exists(file.path(output_dir, "dm.csv"))
  )

  expect_false(
    file.exists(
      file.path(output_dir, "lb_validation_report.csv")
    )
  )
})

test_that("LB pipeline exports consistent CSV files", {
  temp_dir <- withr::local_tempdir()
  inputs <- make_lb_pipeline_inputs(temp_dir)

  result <- run_lb_pipeline(
    lb_input_path = inputs$lb,
    dm_input_path = inputs$dm,
    study_id = "STUDY001",
    output_dir = file.path(temp_dir, "outputs")
  )

  exported_dm <- readr::read_csv(
    result$dm_path,
    show_col_types = FALSE
  )

  exported_lb <- readr::read_csv(
    result$lb_path,
    show_col_types = FALSE
  )

  exported_report <- readr::read_csv(
    result$report_path,
    show_col_types = FALSE
  )

  expect_equal(nrow(exported_dm), nrow(result$dm))
  expect_equal(nrow(exported_lb), nrow(result$lb))

  expect_equal(
    nrow(exported_report),
    nrow(result$validation_report)
  )

  expect_true(all(exported_report$STATUS == "PASS"))
})
