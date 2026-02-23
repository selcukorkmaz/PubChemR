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

test_that("pc_request cache flag can mark cache hit", {
  skip_on_cran()
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
