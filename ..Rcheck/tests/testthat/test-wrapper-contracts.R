test_that("get_assays honors operation argument", {
  skip_on_cran()
  skip_if_not_live_smoke()
  skip_if_offline()

  obj <- get_assays(identifier = 2244, namespace = "cid", operation = "summary")
  expect_equal(request_args(obj, "operation"), "summary")
})

test_that("identifier wrappers return aligned success/error vectors", {
  skip_on_cran()
  skip_if_not_live_smoke()
  skip_if_offline()

  obj <- get_cids(
    identifier = c("aspirin", "definitely_not_a_real_compound_name_zz"),
    namespace = "name"
  )

  expect_length(obj$result, 2)
  expect_length(obj$success, 2)
  expect_length(obj$error, 2)
  expect_true(any(obj$success))
  expect_true(any(!obj$success))
})
