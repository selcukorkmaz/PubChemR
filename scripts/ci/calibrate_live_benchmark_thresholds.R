#!/usr/bin/env Rscript

read_thresholds_csv <- function(path) {
  if (!file.exists(path)) {
    stop("Threshold file not found: ", path)
  }
  tbl <- utils::read.csv(path, stringsAsFactors = FALSE)
  req <- c("scenario_size", "elapsed_sec", "failed_chunk_ratio")
  if (!all(req %in% names(tbl))) {
    stop("Threshold file must contain columns: ", paste(req, collapse = ", "))
  }
  tbl$scenario_size <- as.integer(tbl$scenario_size)
  tbl
}

threshold_list_to_table <- function(x) {
  scenarios <- sort(unique(c(
    as.integer(names(x$elapsed_sec)),
    as.integer(names(x$failed_chunk_ratio))
  )))
  scenarios <- scenarios[!is.na(scenarios)]

  data.frame(
    scenario_size = scenarios,
    elapsed_sec = as.numeric(x$elapsed_sec[as.character(scenarios)]),
    failed_chunk_ratio = as.numeric(x$failed_chunk_ratio[as.character(scenarios)]),
    stringsAsFactors = FALSE
  )
}

load_pubchemr <- function() {
  if (requireNamespace("PubChemR", quietly = TRUE)) {
    suppressPackageStartupMessages(library(PubChemR))
    return(invisible(TRUE))
  }
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("Neither installed package 'PubChemR' nor 'pkgload' is available.")
  }
  pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)
  invisible(TRUE)
}

load_pubchemr()

history_path <- Sys.getenv("PUBCHEMR_BENCHMARK_HISTORY", ".cache/pubchemr-benchmark-history.csv")
baseline_path <- Sys.getenv("PUBCHEMR_BENCHMARK_BASELINE_FILE", "inst/benchmarks/live-thresholds.csv")
out_threshold_path <- Sys.getenv("PUBCHEMR_BENCHMARK_CALIBRATED_FILE", ".cache/pubchemr-calibrated-thresholds.csv")
out_report_path <- Sys.getenv("PUBCHEMR_BENCHMARK_CALIBRATION_REPORT", "benchmark-artifacts/threshold-calibration.md")
min_runs <- as.integer(Sys.getenv("PUBCHEMR_BENCHMARK_CALIBRATION_MIN_RUNS", "3"))
quantile_prob <- as.numeric(Sys.getenv("PUBCHEMR_BENCHMARK_CALIBRATION_QUANTILE", "0.95"))
elapsed_buffer <- as.numeric(Sys.getenv("PUBCHEMR_BENCHMARK_CALIBRATION_ELAPSED_BUFFER", "1.25"))
failed_ratio_buffer <- as.numeric(Sys.getenv("PUBCHEMR_BENCHMARK_CALIBRATION_FAILED_BUFFER", "1.5"))

baseline_tbl <- read_thresholds_csv(baseline_path)
baseline <- list(
  elapsed_sec = stats::setNames(as.numeric(baseline_tbl$elapsed_sec), as.character(baseline_tbl$scenario_size)),
  failed_chunk_ratio = stats::setNames(as.numeric(baseline_tbl$failed_chunk_ratio), as.character(baseline_tbl$scenario_size))
)

if (!file.exists(history_path)) {
  calibrated <- baseline
  history_rows <- 0L
} else {
  history <- utils::read.csv(history_path, stringsAsFactors = FALSE)
  history_rows <- nrow(history)
  calibrated <- PubChemR:::pc_calibrate_benchmark_thresholds(
    history = history,
    baseline = baseline,
    quantile_prob = quantile_prob,
    elapsed_buffer = elapsed_buffer,
    failed_ratio_buffer = failed_ratio_buffer,
    min_runs = min_runs
  )
}

calibrated_tbl <- threshold_list_to_table(calibrated)
dir.create(dirname(out_threshold_path), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(calibrated_tbl, out_threshold_path, row.names = FALSE)

dir.create(dirname(out_report_path), recursive = TRUE, showWarnings = FALSE)
lines <- c(
  "# PubChemR Live Benchmark Threshold Calibration",
  "",
  paste0("- Generated (UTC): ", format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")),
  paste0("- History rows: ", history_rows),
  paste0("- Min runs for calibration: ", min_runs),
  paste0("- Quantile: ", quantile_prob),
  paste0("- Elapsed buffer: ", elapsed_buffer),
  paste0("- Failed-ratio buffer: ", failed_ratio_buffer),
  "",
  "## Baseline thresholds",
  ""
)
lines <- c(lines, utils::capture.output(print(baseline_tbl)))
lines <- c(lines, "", "## Calibrated thresholds", "")
lines <- c(lines, utils::capture.output(print(calibrated_tbl)))
writeLines(lines, out_report_path, useBytes = TRUE)

cat("Calibrated thresholds written to:", out_threshold_path, "\n")
