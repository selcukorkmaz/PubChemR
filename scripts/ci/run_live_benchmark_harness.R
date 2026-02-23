#!/usr/bin/env Rscript

parse_int_vec <- function(x, default) {
  raw <- trimws(Sys.getenv(x, default))
  if (!nzchar(raw)) {
    return(as.integer(strsplit(default, ",", fixed = TRUE)[[1]]))
  }
  as.integer(trimws(strsplit(raw, ",", fixed = TRUE)[[1]]))
}

read_thresholds_csv <- function(path) {
  if (!file.exists(path)) {
    stop("Threshold file not found: ", path)
  }
  tbl <- utils::read.csv(path, stringsAsFactors = FALSE)
  req <- c("scenario_size", "elapsed_sec", "failed_chunk_ratio")
  if (!all(req %in% names(tbl))) {
    stop("Threshold file must contain columns: ", paste(req, collapse = ", "))
  }

  scenario <- as.character(as.integer(tbl$scenario_size))
  list(
    elapsed_sec = stats::setNames(as.numeric(tbl$elapsed_sec), scenario),
    failed_chunk_ratio = stats::setNames(as.numeric(tbl$failed_chunk_ratio), scenario)
  )
}

append_history <- function(history_path, summary_tbl) {
  dir.create(dirname(history_path), recursive = TRUE, showWarnings = FALSE)

  stamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  run_meta <- data.frame(
    timestamp_utc = stamp,
    workflow = Sys.getenv("GITHUB_WORKFLOW", "local"),
    run_id = Sys.getenv("GITHUB_RUN_ID", "local"),
    sha = Sys.getenv("GITHUB_SHA", "local"),
    stringsAsFactors = FALSE
  )

  rows <- cbind(run_meta[rep(1, nrow(summary_tbl)), , drop = FALSE], summary_tbl)
  if (file.exists(history_path)) {
    old <- utils::read.csv(history_path, stringsAsFactors = FALSE)
    rows <- rbind(old, rows)
  }
  utils::write.csv(rows, history_path, row.names = FALSE)
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

out_dir <- Sys.getenv("PUBCHEMR_BENCHMARK_OUT_DIR", "benchmark-artifacts")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

history_path <- Sys.getenv("PUBCHEMR_BENCHMARK_HISTORY", ".cache/pubchemr-benchmark-history.csv")
threshold_path <- Sys.getenv("PUBCHEMR_BENCHMARK_THRESHOLD_FILE", "inst/benchmarks/live-thresholds.csv")
thresholds <- read_thresholds_csv(threshold_path)

scenario_sizes <- parse_int_vec("PUBCHEMR_BENCHMARK_SCENARIOS", "10,1000,100000")
chunk_sizes <- parse_int_vec("PUBCHEMR_BENCHMARK_CHUNK_SIZES", "1000")

benchmark_probe <- function(chunk_ids) {
  pc_request(
    domain = "compound",
    namespace = "cid",
    identifier = 2244,
    operation = "property/MolecularWeight",
    output = "JSON",
    cache = FALSE,
    force_refresh = TRUE,
    timeout = 45,
    retries = 2
  )
}

report_md <- file.path(out_dir, "live-benchmark-report.md")
report_rds <- file.path(out_dir, "live-benchmark-report.rds")
summary_csv <- file.path(out_dir, "live-benchmark-summary.csv")
details_csv <- file.path(out_dir, "live-benchmark-details.csv")
status_txt <- file.path(out_dir, "live-benchmark-status.txt")

report <- pc_benchmark_harness(
  fn = benchmark_probe,
  ids = rep(2244, max(scenario_sizes)),
  scenario_sizes = scenario_sizes,
  chunk_sizes = chunk_sizes,
  parallel_options = FALSE,
  thresholds = thresholds,
  report_path = report_md,
  report_format = "markdown"
)

saveRDS(report, report_rds)
utils::write.csv(report$summary, summary_csv, row.names = FALSE)
utils::write.csv(report$details, details_csv, row.names = FALSE)
append_history(history_path, report$summary)

all_pass <- isTRUE(all(report$summary$all_runs_pass))
writeLines(
  c(
    paste("threshold_file:", normalizePath(threshold_path, winslash = "/", mustWork = FALSE)),
    paste("history_file:", normalizePath(history_path, winslash = "/", mustWork = FALSE)),
    paste("all_runs_pass:", all_pass)
  ),
  con = status_txt,
  useBytes = TRUE
)

print(report)

if (!all_pass) {
  quit(save = "no", status = 1)
}
