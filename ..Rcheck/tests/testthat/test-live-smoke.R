test_that("live-smoke: PubChem name to CID mapping works", {
  skip_on_cran()
  skip_if_not_live_smoke()
  skip_if_offline()

  out <- get_cids(identifier = "aspirin", namespace = "name")
  expect_true(length(out$result) >= 1)
  expect_true(any(out$success))
})

test_that("live-smoke: pc_request basic compound retrieval works", {
  skip_on_cran()
  skip_if_not_live_smoke()
  skip_if_offline()

  res <- pc_request(
    domain = "compound",
    namespace = "cid",
    identifier = 2244,
    output = "JSON"
  )

  expect_s3_class(res, "PubChemResult")
  expect_true(isTRUE(res$success))
  expect_false(is.null(res$data))
})

test_that("live-smoke: assay summary endpoint responds", {
  skip_on_cran()
  skip_if_not_live_smoke()
  skip_if_offline()

  obj <- get_assays(identifier = 2551, namespace = "aid", operation = "description")
  expect_true(length(obj$result) >= 1)
  expect_true(any(obj$success))
})

test_that("live-smoke: biological test results section is retrievable", {
  skip_on_cran()
  skip_if_not_live_smoke()
  skip_if_offline()

  bio <- get_biological_test_results(identifier = "2244", domain = "compound")
  expect_s3_class(bio, "PugViewSection")
  expect_true(isTRUE(bio$success))
  expect_equal(bio$result$TOCHeading, "Biological Test Results")
})

test_that("live-smoke: pc_assay_activity_long returns activity table", {
  skip_on_cran()
  skip_if_not_live_smoke()
  skip_if_offline()

  tbl <- pc_assay_activity_long(identifier = 2244, namespace = "cid")
  expect_s3_class(tbl, "tbl_df")
  expect_true(nrow(tbl) > 0)
  expect_true(all(c("CID", "AID", "ActivityOutcome") %in% names(tbl)))
})
