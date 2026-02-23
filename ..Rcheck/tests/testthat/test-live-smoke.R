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
