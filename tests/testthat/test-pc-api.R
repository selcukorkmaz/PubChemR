test_that("pc_config get/set works for known keys", {
  cfg0 <- pc_config()
  expect_true(is.list(cfg0))
  expect_true("rate_limit" %in% names(cfg0))

  cfg1 <- pc_config(rate_limit = 4)
  expect_equal(cfg1$rate_limit, 4)

  # restore default used in package initialization
  pc_config(rate_limit = 5)
})

test_that("pc_response creates PubChemResult from fault payload", {
  payload <- '{"Fault":{"Code":"PUGREST.BadRequest","Message":"Invalid request"}}'
  res <- pc_response(payload, request = list(url = "https://example.org"))

  expect_s3_class(res, "PubChemResult")
  expect_false(res$success)
  expect_s3_class(res$error, "PubChemError")
  expect_match(res$error$code, "PUGREST")
})

test_that("pc_batch chunks and summarizes execution", {
  dummy <- function(ids) {
    pc_response('{"IdentifierList":{"CID":[2244]}}', request = list(identifier = ids))
  }

  b <- pc_batch(ids = 1:5, fn = dummy, chunk_size = 2, parallel = FALSE)
  expect_s3_class(b, "PubChemBatchResult")
  expect_equal(length(b$chunks), 3)
  expect_equal(length(b$results), 3)

  tbl <- as_tibble(b)
  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 3)
})

test_that("pc_benchmark returns scenario metrics", {
  dummy <- function(ids) {
    pc_response('{"IdentifierList":{"CID":[2244]}}', request = list(identifier = ids))
  }

  bm <- pc_benchmark(
    ids = 1:10,
    fn = dummy,
    chunk_sizes = c(2, 5),
    parallel_options = c(FALSE)
  )

  expect_s3_class(bm, "tbl_df")
  expect_equal(nrow(bm), 2)
  expect_true(all(c("chunk_size", "elapsed_sec", "successful_chunks") %in% names(bm)))
})

test_that("pc_benchmark_harness supports 10/1000/100000 scenarios", {
  dummy <- function(ids) {
    pc_response('{"IdentifierList":{"CID":[2244]}}', request = list(identifier = ids))
  }

  out <- pc_benchmark_harness(
    fn = dummy,
    ids = 1:10,
    scenario_sizes = c(10L, 1000L, 100000L),
    chunk_sizes = 100000L,
    parallel_options = FALSE,
    thresholds = list(
      elapsed_sec = Inf,
      failed_chunk_ratio = 0
    )
  )

  expect_s3_class(out, "PubChemBenchmarkReport")
  expect_s3_class(out$details, "tbl_df")
  expect_s3_class(out$summary, "tbl_df")
  expect_equal(sort(unique(out$details$scenario_size)), c(10L, 1000L, 100000L))
  expect_equal(nrow(out$summary), 3)
  expect_true(all(out$summary$all_runs_pass))
})

test_that("pc_benchmark_harness enforces threshold gates", {
  dummy <- function(ids) {
    pc_response('{"IdentifierList":{"CID":[2244]}}', request = list(identifier = ids))
  }

  elapsed_fail <- pc_benchmark_harness(
    fn = dummy,
    ids = 1:10,
    scenario_sizes = 10L,
    chunk_sizes = 10L,
    thresholds = list(
      elapsed_sec = -1,
      failed_chunk_ratio = 1
    )
  )

  expect_false(elapsed_fail$details$run_pass[[1]])
  expect_false(elapsed_fail$summary$all_runs_pass[[1]])

  flaky <- function(ids) {
    if (as.integer(ids[[1]]) == 6L) {
      stop("synthetic chunk failure")
    }
    pc_response('{"IdentifierList":{"CID":[2244]}}', request = list(identifier = ids))
  }

  ratio_fail <- pc_benchmark_harness(
    fn = flaky,
    ids = 1:10,
    scenario_sizes = 10L,
    chunk_sizes = 5L,
    thresholds = list(
      elapsed_sec = Inf,
      failed_chunk_ratio = 0.4
    )
  )

  expect_equal(ratio_fail$details$failed_chunks[[1]], 1)
  expect_gt(ratio_fail$details$failed_chunk_ratio[[1]], 0.4)
  expect_false(ratio_fail$details$run_pass[[1]])
})

test_that("pc_benchmark_harness writes markdown, csv, and rds reports", {
  dummy <- function(ids) {
    pc_response('{"IdentifierList":{"CID":[2244]}}', request = list(identifier = ids))
  }

  formats <- c(markdown = ".md", csv = ".csv", rds = ".rds")

  for (fmt in names(formats)) {
    path <- tempfile(fileext = formats[[fmt]])
    out <- pc_benchmark_harness(
      fn = dummy,
      ids = 1:10,
      scenario_sizes = 10L,
      chunk_sizes = 10L,
      thresholds = list(
        elapsed_sec = Inf,
        failed_chunk_ratio = 0
      ),
      report_path = path,
      report_format = fmt
    )

    expect_true(file.exists(path))

    if (fmt == "markdown") {
      txt <- readLines(path, warn = FALSE)
      expect_true(any(grepl("PubChem Benchmark Harness Report", txt, fixed = TRUE)))
    } else if (fmt == "csv") {
      csv <- utils::read.csv(path)
      expect_true("scenario_size" %in% names(csv))
    } else {
      obj <- readRDS(path)
      expect_s3_class(obj, "PubChemBenchmarkReport")
      expect_s3_class(out, "PubChemBenchmarkReport")
    }
  }
})

test_that("pc_benchmark_harness validates key inputs", {
  dummy <- function(ids) {
    pc_response('{"IdentifierList":{"CID":[2244]}}', request = list(identifier = ids))
  }

  expect_error(
    pc_benchmark_harness(
      fn = dummy,
      ids = 1:10,
      scenario_sizes = 10L,
      chunk_sizes = 0
    ),
    "chunk_sizes"
  )

  expect_error(
    pc_benchmark_harness(
      fn = dummy,
      ids = 1:10,
      scenario_sizes = 10L,
      id_generator = 123
    ),
    "id_generator"
  )

  expect_error(
    pc_benchmark_harness(
      fn = dummy,
      ids = 1:10,
      scenario_sizes = 10L,
      thresholds = 1
    ),
    "thresholds"
  )
})

test_that("pc_calibrate_benchmark_thresholds applies quantile-based calibration floors", {
  history <- data.frame(
    scenario_size = c(10, 10, 10, 1000, 1000, 1000),
    max_elapsed_sec = c(10, 12, 8, 200, 240, 260),
    max_failed_chunk_ratio = c(0, 0.02, 0, 0.01, 0.03, 0.02),
    stringsAsFactors = FALSE
  )

  calibrated <- pc_calibrate_benchmark_thresholds(
    history = history,
    baseline = list(
      elapsed_sec = c(`10` = 30, `1000` = 300),
      failed_chunk_ratio = c(`10` = 0, `1000` = 0.01)
    ),
    quantile_prob = 0.95,
    elapsed_buffer = 1.25,
    failed_ratio_buffer = 1.5,
    min_runs = 3
  )

  expect_true(is.list(calibrated))
  expect_true(all(c("elapsed_sec", "failed_chunk_ratio") %in% names(calibrated)))
  expect_true(calibrated$elapsed_sec[["10"]] >= 30)
  expect_true(calibrated$elapsed_sec[["1000"]] >= 300)
  expect_true(calibrated$failed_chunk_ratio[["10"]] >= 0)
  expect_true(calibrated$failed_chunk_ratio[["1000"]] >= 0.01)
})

test_that("pc_calibrate_benchmark_thresholds validates history schema", {
  bad <- data.frame(
    scenario_size = 10,
    max_elapsed_sec = 1,
    stringsAsFactors = FALSE
  )

  expect_error(
    pc_calibrate_benchmark_thresholds(history = bad),
    "history"
  )
})

test_that("pc_batch checkpoint manifest is created and resume can rerun failed chunks", {
  td <- tempfile("pc-batch-checkpoint-")
  dir.create(td, recursive = TRUE)

  e <- new.env(parent = emptyenv())
  e$calls <- 0L
  e$fail_once <- TRUE

  flaky <- function(ids) {
    e$calls <- e$calls + 1L
    if (as.integer(ids[[1]]) == 3L && isTRUE(e$fail_once)) {
      e$fail_once <- FALSE
      stop("transient chunk error")
    }
    pc_response('{"IdentifierList":{"CID":[2244]}}', request = list(identifier = ids))
  }

  b1 <- pc_batch(
    ids = 1:5,
    fn = flaky,
    chunk_size = 2,
    checkpoint_dir = td,
    checkpoint_id = "resume_case"
  )

  expect_false(all(b1$success))
  expect_true(file.exists(file.path(td, "pc_batch_resume_case_manifest.rds")))
  expect_equal(e$calls, 3L)

  b2 <- pc_resume_batch(
    fn = flaky,
    checkpoint_dir = td,
    checkpoint_id = "resume_case"
  )

  expect_true(all(b2$success))
  expect_true(isTRUE(b2$checkpoint$resumed))
  expect_equal(e$calls, 4L)
})

test_that("pc_resume_batch does not rerun completed chunks", {
  td <- tempfile("pc-batch-checkpoint-")
  dir.create(td, recursive = TRUE)

  e <- new.env(parent = emptyenv())
  e$calls <- 0L

  dummy <- function(ids) {
    e$calls <- e$calls + 1L
    pc_response('{"IdentifierList":{"CID":[2244]}}', request = list(identifier = ids))
  }

  b1 <- pc_batch(
    ids = 1:4,
    fn = dummy,
    chunk_size = 2,
    checkpoint_dir = td,
    checkpoint_id = "no_rerun_case"
  )

  expect_true(all(b1$success))
  calls_after_first <- e$calls

  b2 <- pc_resume_batch(
    fn = dummy,
    checkpoint_dir = td,
    checkpoint_id = "no_rerun_case"
  )

  expect_true(all(b2$success))
  expect_equal(e$calls, calls_after_first)
})

test_that("pc_resume_batch errors when checkpoint manifest is missing", {
  td <- tempfile("pc-batch-checkpoint-")
  dir.create(td, recursive = TRUE)

  expect_error(
    pc_resume_batch(
      fn = function(ids) ids,
      checkpoint_dir = td,
      checkpoint_id = "missing_case"
    ),
    "No checkpoint manifest found"
  )
})

test_that("pc_request cache flag can mark cache hit", {
  skip_on_cran()
  skip_if_not_live_smoke()
  skip_if_offline()

  pc_cache_clear()

  a <- pc_request(
    domain = "compound",
    namespace = "cid",
    identifier = 2244,
    output = "JSON",
    cache = TRUE
  )
  expect_true(a$success)

  b <- pc_request(
    domain = "compound",
    namespace = "cid",
    identifier = 2244,
    output = "JSON",
    cache = TRUE
  )

  expect_true(b$success)
  expect_true(isTRUE(b$from_cache))
})

test_that("pc_request offline mode returns cache-miss error when absent", {
  pc_cache_clear()

  out <- pc_request(
    domain = "compound",
    namespace = "cid",
    identifier = 2244,
    output = "JSON",
    cache = TRUE,
    offline = TRUE
  )

  expect_s3_class(out, "PubChemResult")
  expect_false(out$success)
  expect_equal(out$error$code, "OfflineCacheMiss")
})

test_that("pc_cache_info returns a diagnostics tibble", {
  info <- pc_cache_info()
  expect_s3_class(info, "tbl_df")
  expect_equal(nrow(info), 1)
  expect_true(all(c("memory_entries", "disk_entries", "disk_size_bytes", "cache_dir") %in% names(info)))
})

test_that("pc_feature_table produces a tabular feature set", {
  skip_on_cran()
  skip_if_not_live_smoke()
  skip_if_offline()

  tbl <- pc_feature_table(
    identifier = c(2244, 3672),
    properties = c("MolecularWeight", "XLogP"),
    namespace = "cid",
    cache = TRUE
  )

  expect_s3_class(tbl, "tbl_df")
  expect_true(nrow(tbl) >= 1)
  expect_true(any(c("MolecularWeight", "XLogP") %in% names(tbl)))
})
