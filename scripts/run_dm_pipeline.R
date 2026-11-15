
# ===========================================================
# SDTM Dataset Generator
# DM End-to-End Pipeline Runner
# ===========================================================

# Load the local R package
devtools::load_all(".")

# Pipeline configuration
input_path <- "inst/extdata/raw_dm.csv"
study_id <- "STUDY001"
output_dir <- "outputs"

cat("\n")
cat("========================================\n")
cat("SDTM DATASET GENERATOR - DM PIPELINE\n")
cat("========================================\n")
cat("Study ID:", study_id, "\n")
cat("Input file:", input_path, "\n")
cat("Output directory:", output_dir, "\n")
cat("----------------------------------------\n")

# Execute the pipeline
result <- run_dm_pipeline(
  input_path = input_path,
  study_id = study_id,
  output_dir = output_dir
)

# Display execution summary
cat("\nPIPELINE EXECUTION SUMMARY\n")
cat("----------------------------------------\n")

cat("Domain: DM\n")
cat("Subjects:", nrow(result$dm), "\n")
cat("Variables:", ncol(result$dm), "\n")
cat("Validation checks:", nrow(result$validation_report), "\n")

cat(
  "Passed checks:",
  sum(result$validation_report$STATUS == "PASS"),
  "\n"
)

cat("Status: SUCCESS\n")

cat("\nGenerated files:\n")
cat(" -", result$dm_path, "\n")
cat(" -", result$report_path, "\n")

cat("\nDM dataset preview:\n")
print(utils::head(result$dm))

cat("\nValidation report:\n")
print(result$validation_report, width = Inf)

cat("\nPipeline completed successfully.\n")
