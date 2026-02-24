mock_assaysummary_payload <- function() {
  list(
    Table = list(
      Columns = list(
        Column = c(
          "AID",
          "SID",
          "CID",
          "Activity Outcome",
          "Activity Value [uM]"
        )
      ),
      Row = list(
        list(Cell = c("1001", "9001", "2244", "Active", "1.20")),
        list(Cell = c("1002", "9002", "2244", "Inactive", "")),
        list(Cell = c("1003", "9003", "3672", "Inconclusive", "12.50"))
      )
    )
  )
}

test_that("pc_assay_activity_long parses assay summary payload", {
  tbl <- pc_assay_activity_long(x = mock_assaysummary_payload())

  expect_s3_class(tbl, "PubChemTable")
  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 3)
  expect_true(all(c("CID", "AID", "ActivityOutcome", "ActivityValue_uM", "ActivityOutcomeValue") %in% names(tbl)))
  expect_equal(tbl$CID[[1]], "2244")
  expect_equal(tbl$AID[[1]], "1001")
  expect_equal(tbl$ActivityOutcome[[1]], "Active")
  expect_equal(tbl$ActivityOutcomeValue[[1]], 1)
})

test_that("pc_assay_activity_long supports PubChemResult input", {
  res <- pc_make_result(
    success = TRUE,
    request = list(),
    data = mock_assaysummary_payload(),
    status = 200
  )

  tbl <- pc_assay_activity_long(x = res)
  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 3)
})

test_that("pc_export_model_data exports csv and rds", {
  mm <- pc_model_matrix(
    x = tibble::tibble(
      CID = c("1", "2", "3"),
      f1 = c(1.1, 2.2, 3.3),
      f2 = c(0.1, 0.2, 0.3)
    ),
    id_cols = c("CID")
  )

  csv_path <- tempfile(fileext = ".csv")
  rds_path <- tempfile(fileext = ".rds")

  info_csv <- pc_export_model_data(mm, csv_path, format = "csv")
  info_rds <- pc_export_model_data(mm, rds_path, format = "rds")

  expect_true(file.exists(csv_path))
  expect_true(file.exists(rds_path))
  expect_equal(info_csv$format, "csv")
  expect_equal(info_rds$format, "rds")

  csv_tbl <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  rds_tbl <- readRDS(rds_path)

  expect_true(all(c("CID", "f1", "f2") %in% names(csv_tbl)))
  expect_true(all(c("CID", "f1", "f2") %in% names(rds_tbl)))
  expect_equal(nrow(csv_tbl), 3)
  expect_equal(nrow(rds_tbl), 3)
})

test_that("pc_assay_activity_long fails explicitly when chunked requests fail", {
  ok_res <- pc_make_result(
    success = TRUE,
    request = list(identifier = "2244"),
    data = mock_assaysummary_payload(),
    status = 200
  )
  fail_res <- pc_make_result(
    success = FALSE,
    request = list(identifier = "3672"),
    data = NULL,
    error = pc_make_error("TransportError", "synthetic chunk failure"),
    status = 500
  )

  local_mocked_bindings(
    pc_batch = function(ids, fn, chunk_size, ...) {
      structure(
        list(
          ids = ids,
          chunks = list(ids[[1]], ids[[2]]),
          results = list(ok_res, fail_res),
          success = c(TRUE, FALSE),
          error = c("", "synthetic chunk failure")
        ),
        class = "PubChemBatchResult"
      )
    },
    .package = "PubChemR"
  )

  expect_error(
    pc_assay_activity_long(
      identifier = c("2244", "3672"),
      namespace = "cid",
      chunk_size = 1L
    ),
    "chunked request failed"
  )
})

test_that("pc_assay_activity_long live smoke on assaysummary", {
  skip_on_cran()
  skip_if_not_live_smoke()
  skip_if_offline()

  tbl <- pc_assay_activity_long(identifier = 2244, namespace = "cid")
  expect_s3_class(tbl, "tbl_df")
  expect_true(nrow(tbl) > 0)
  expect_true(all(c("CID", "AID", "ActivityOutcome") %in% names(tbl)))
})
