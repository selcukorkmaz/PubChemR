# request_args() tests ----
test_that("request_args() works properly", {
  expect_equal(request_args(compound, .which = "namespace"), "name")
  expect_true({
    is.list(request_args(compound))
  })

  expect_null(request_args(compound, "some_args"))
})


