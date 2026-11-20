
# ============================================================
# VS End-to-End Pipeline Integration Tests
# ============================================================

make_vs_pipeline_inputs <- function(dir) {
  dm_path <- file.path(dir, "raw_dm.csv")
  vs_path <- file.path(dir, "raw_vs.csv")

  dm_source <- system.file(
    "extdata", "raw_dm.csv",
    package = "sdtmgenerator"
  )

  vs_source <- system.file(
    "extdata", "raw_vs.csv",
    package = "sdtmgenerator"
  )

  # Support both installed-package tests and local development.
  if (!nzchar(dm_source)) {
    dm_source <- testthat::test_path(
      "..", "..", "inst", "extdata", "raw_dm.csv"
    )
  }

  if (!nzchar(vs_source)) {
    vs_source <- testthat::test_path(
      "..", "..", "inst", "extdata", "raw_vs.csv"
    )
  }

  stopifnot(
    file.exists(dm_source),
    file.exists(vs_source),
    file.copy(dm_source, dm_path),
    file.copy(vs_source, vs_path)
  )

  list(dm = dm_path, vs = vs_path)
}


test_that("VS pipeline generates validated artifacts", {
  temp_dir <- withr::local_tempdir()
  inputs <- make_vs_pipeline_inputs(temp_dir)
  output_dir <- file.path(temp_dir, "outputs")

  result <- run_vs_pipeline(
    vs_input_path = inputs$vs,
    dm_input_path = inputs$dm,
    study_id = "STUDY001",
    output_dir = output_dir
  )

  expect_true(is.data.frame(result$dm))
  expect_true(is.data.frame(result$vs))
  expect_true(is.data.frame(result$validation_report))

  expect_equal(nrow(result$dm), 20L)
  expect_gt(nrow(result$vs), 0L)

  expect_true(validate_dm(result$dm))
  expect_true(validate_vs(result$vs, result$dm))

  expect_true(all(result$validation_report$STATUS == "PASS"))

  expect_true(file.exists(result$dm_path))
  expect_true(file.exists(result$vs_path))
  expect_true(file.exists(result$report_path))
})


test_that("VS pipeline preserves referential integrity", {
  temp_dir <- withr::local_tempdir()
  inputs <- make_vs_pipeline_inputs(temp_dir)

  result <- run_vs_pipeline(
    vs_input_path = inputs$vs,
    dm_input_path = inputs$dm,
    study_id = "STUDY001",
    output_dir = file.path(temp_dir, "outputs")
  )

  expect_true(
    all(result$vs$USUBJID %in% result$dm$USUBJID)
  )

  expect_equal(
    anyDuplicated(result$vs[c("USUBJID", "VSSEQ")]),
    0L
  )

  expect_true(all(result$vs$DOMAIN == "VS"))
  expect_true(all(result$vs$STUDYID == "STUDY001"))
})


test_that("VS pipeline exports consistent CSV files", {
  temp_dir <- withr::local_tempdir()
  inputs <- make_vs_pipeline_inputs(temp_dir)

  result <- run_vs_pipeline(
    vs_input_path = inputs$vs,
    dm_input_path = inputs$dm,
    study_id = "STUDY001",
    output_dir = file.path(temp_dir, "outputs")
  )

  exported_vs <- readr::read_csv(
    result$vs_path,
    show_col_types = FALSE
  )

  exported_report <- readr::read_csv(
    result$report_path,
    show_col_types = FALSE
  )

  expect_equal(nrow(exported_vs), nrow(result$vs))
  expect_equal(names(exported_vs), names(result$vs))

  expect_equal(
    nrow(exported_report),
    nrow(result$validation_report)
  )

  expect_true(all(exported_report$STATUS == "PASS"))
})


test_that("VS pipeline rejects missing input files", {
  temp_dir <- withr::local_tempdir()

  expect_error(
    run_vs_pipeline(
      vs_input_path = file.path(temp_dir, "missing_vs.csv"),
      dm_input_path = file.path(temp_dir, "missing_dm.csv"),
      study_id = "STUDY001",
      output_dir = file.path(temp_dir, "outputs")
    )
  )
})


test_that("VS pipeline rejects invalid subject references", {
  temp_dir <- withr::local_tempdir()
  inputs <- make_vs_pipeline_inputs(temp_dir)
  output_dir <- file.path(temp_dir, "outputs")

  raw_vs <- readr::read_csv(
    inputs$vs,
    col_types = readr::cols(
      .default = readr::col_character()
    ),
    show_col_types = FALSE
  )

  # Introduce a subject that does not exist in DM.
  raw_vs$subject_id[1] <- "999"

  readr::write_csv(raw_vs, inputs$vs)

  expect_error(
    run_vs_pipeline(
      vs_input_path = inputs$vs,
      dm_input_path = inputs$dm,
      study_id = "STUDY001",
      output_dir = output_dir
    ),
    "VS subjects not found in DM"
  )

  expect_false(
    file.exists(file.path(output_dir, "vs.csv"))
  )
})


test_that("VS pipeline rejects invalid numeric results", {
  temp_dir <- withr::local_tempdir()
  inputs <- make_vs_pipeline_inputs(temp_dir)
  output_dir <- file.path(temp_dir, "outputs")

  raw_vs <- readr::read_csv(
    inputs$vs,
    col_types = readr::cols(
      .default = readr::col_character()
    ),
    show_col_types = FALSE
  )

  raw_vs$result[1] <- "invalid"

  readr::write_csv(raw_vs, inputs$vs)

  expect_error(
    run_vs_pipeline(
      vs_input_path = inputs$vs,
      dm_input_path = inputs$dm,
      study_id = "STUDY001",
      output_dir = output_dir
    )
  )

  expect_false(
    file.exists(file.path(output_dir, "vs.csv"))
  )
})


test_that("VS pipeline preserves leading zeros", {
  temp_dir <- withr::local_tempdir()
  inputs <- make_vs_pipeline_inputs(temp_dir)

  result <- run_vs_pipeline(
    vs_input_path = inputs$vs,
    dm_input_path = inputs$dm,
    study_id = "STUDY001",
    output_dir = file.path(temp_dir, "outputs")
  )

  raw_vs <- readr::read_csv(
    inputs$vs,
    col_types = readr::cols(
      .default = readr::col_character()
    ),
    show_col_types = FALSE
  )

  expect_true(
    all(raw_vs$subject_id %in%
          sub("^.*-", "", result$dm$USUBJID))
  )

  expect_true(
    all(result$vs$USUBJID %in% result$dm$USUBJID)
  )
})
