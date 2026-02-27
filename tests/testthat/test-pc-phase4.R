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

test_that("pc_assay_activity_long keeps ActivityValue_uM type stable across chunks", {
  payload_num <- list(
    Table = list(
      Columns = list(Column = c("CID", "AID", "Activity Outcome", "Activity Value [uM]")),
      Row = list(list(Cell = c("2244", "1001", "Active", "1.2")))
    )
  )
  payload_chr <- list(
    Table = list(
      Columns = list(Column = c("CID", "AID", "Activity Outcome", "Activity Value [uM]")),
      Row = list(list(Cell = c("3672", "1002", "Inactive", "not_reported")))
    )
  )

  res_num <- pc_make_result(success = TRUE, request = list(identifier = "2244"), data = payload_num, status = 200)
  res_chr <- pc_make_result(success = TRUE, request = list(identifier = "3672"), data = payload_chr, status = 200)

  local_mocked_bindings(
    pc_batch = function(ids, fn, chunk_size, ...) {
      structure(
        list(
          ids = ids,
          chunks = list(ids[[1]], ids[[2]]),
          results = list(res_num, res_chr),
          success = c(TRUE, TRUE),
          error = c("", "")
        ),
        class = "PubChemBatchResult"
      )
    },
    .package = "PubChemR"
  )

  out <- pc_assay_activity_long(
    identifier = c("2244", "3672"),
    namespace = "cid",
    chunk_size = 1L
  )

  expect_true("ActivityValue_uM" %in% names(out))
  expect_type(out$ActivityValue_uM, "double")
  expect_equal(out$ActivityValue_uM[out$CID == "2244"][[1]], 1.2)
  expect_true(is.na(out$ActivityValue_uM[out$CID == "3672"][[1]]))
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

# --- pc_sdq_bioactivity tests ---

mock_sdq_download_response <- function(cid = "2244", n = 3) {
  # download mode returns a raw JSON array (not wrapped in SDQOutputSet)
  lapply(seq_len(n), function(i) {
    list(
      baid = as.character(10000 + i),
      activityid = "Active",
      aid = as.character(1000 + i),
      sid = as.character(9000 + i),
      cid = cid,
      refsid = as.character(50000 + i),
      aidtypeid = "Confirmatory",
      aidmdate = "2025-01-01",
      hasdrc = "0",
      rnai = "0",
      aidsrcname = "TestSource",
      aidname = paste0("Assay_", i),
      cmpdname = "Aspirin",
      targetname = paste0("Target_", i),
      acvalue = as.character(1.5 * i),
      acname = "IC50",
      geneid = as.character(7000 + i)
    )
  })
}

test_that("pc_sdq_bioactivity parses mock SDQ download response correctly", {
  mock_resp <- mock_sdq_download_response(cid = "2244", n = 3)

  local_mocked_bindings(
    GET = function(url, ...) {
      structure(
        list(
          status_code = 200L,
          headers = list(`content-type` = "application/json"),
          content = charToRaw(RJSONIO::toJSON(mock_resp))
        ),
        class = "response"
      )
    },
    status_code = function(x) x$status_code,
    content = function(x, as = "text", encoding = "UTF-8", ...) {
      rawToChar(x$content)
    },
    .package = "httr"
  )

  tbl <- pc_sdq_bioactivity(2244, rate_limit = FALSE)

  expect_s3_class(tbl, "PubChemTable")
  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 3)
  expect_true(all(c("baid", "aid", "sid", "cid", "activityid",
                     "aidtypeid", "aidname", "targetname",
                     "cmpdname", "acvalue", "acname",
                     "geneid") %in% names(tbl)))
  # ID columns should be character
  expect_type(tbl$cid, "character")
  expect_type(tbl$aid, "character")
  expect_type(tbl$sid, "character")
  expect_type(tbl$baid, "character")
  expect_equal(tbl$cid[[1]], "2244")
  # Column count matches mock data (no hardcoded padding)
  expect_equal(ncol(tbl), 17)
})

test_that("pc_sdq_bioactivity errors on missing identifier", {
  expect_error(pc_sdq_bioactivity(), "'identifier' is required")
})

test_that("pc_sdq_bioactivity errors on SDQ failure status", {
  mock_resp <- list(
    SDQOutputSet = list(
      list(
        status = list(code = -4, error = "test error message"),
        totalCount = 0
      )
    )
  )

  local_mocked_bindings(
    GET = function(url, ...) {
      structure(
        list(
          status_code = 200L,
          headers = list(`content-type` = "application/json"),
          content = charToRaw(RJSONIO::toJSON(mock_resp))
        ),
        class = "response"
      )
    },
    status_code = function(x) x$status_code,
    content = function(x, as = "text", encoding = "UTF-8", ...) {
      rawToChar(x$content)
    },
    .package = "httr"
  )

  expect_error(pc_sdq_bioactivity(2244, rate_limit = FALSE),
               "SDQ query failed")
})

test_that("pc_sdq_bioactivity returns empty tibble when no rows", {
  # download mode returns an empty JSON array [] for no results
  mock_resp <- list()

  local_mocked_bindings(
    GET = function(url, ...) {
      structure(
        list(
          status_code = 200L,
          headers = list(`content-type` = "application/json"),
          content = charToRaw("[]")
        ),
        class = "response"
      )
    },
    status_code = function(x) x$status_code,
    content = function(x, as = "text", encoding = "UTF-8", ...) {
      rawToChar(x$content)
    },
    .package = "httr"
  )

  tbl <- pc_sdq_bioactivity(2244, rate_limit = FALSE)
  expect_s3_class(tbl, "PubChemTable")
  expect_equal(nrow(tbl), 0)
})

test_that("pc_sdq_bioactivity live smoke on CID 2244", {
  skip_on_cran()
  skip_if_not_live_smoke()
  skip_if_offline()

  tbl <- pc_sdq_bioactivity(2244)
  expect_s3_class(tbl, "PubChemTable")
  expect_s3_class(tbl, "tbl_df")
  expect_true(nrow(tbl) > 0)
  expect_true(all(c("cid", "aid", "sid", "baid") %in% names(tbl)))
  expect_type(tbl$cid, "character")
})
