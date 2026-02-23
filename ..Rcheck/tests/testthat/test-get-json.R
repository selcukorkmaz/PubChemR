test_that("get_json returns structured faults without code execution", {
  skip_on_cran()
  skip_if_not_live_smoke()
  skip_if_offline()

  out <- PubChemR:::get_json(
    identifier = "not_a_valid_cid_value",
    namespace = "cid",
    domain = "compound"
  )

  expect_s3_class(out, "PubChemInstance")
  expect_false(out$success)
  expect_type(out$error, "list")
  expect_true("Code" %in% names(out$error))
  expect_true("Message" %in% names(out$error))
  expect_false(is.null(out$error$Message))
})

test_that("get_json includes request args in failure objects", {
  skip_on_cran()
  skip_if_not_live_smoke()
  skip_if_offline()

  out <- PubChemR:::get_json(
    identifier = "%%%invalid%%%identifier%%%",
    namespace = "cid",
    domain = "compound"
  )

  expect_false(out$success)
  expect_equal(out$request_args$namespace, "cid")
  expect_equal(out$request_args$domain, "compound")
})
