a <- get_assays(
  identifier = 1234,
  namespace = "aid",
)

# test for 'namespace' arg. ----
# test_that("pulling assays via 'name' is successful", {
#   tmp <- try(get_assays(
#     identifier = "aspirin",
#     namespace = "name",
#   ))
# })


test_that("pulling assays via 'aid' is successful", {
  tmp <- try(get_assays(
    identifier = 2244,
    namespace = "aid",
  ))

  expect_false(inherits(tmp, "try-error"))

  # Returned object is a tibble or data.frame
  expect_true(inherits(tmp, "list"))
  expect_true(length(tmp) == 1)
  expect_true(names(tmp) == "'2244'")
})


test_that("return error for unknown/undefined assays", {
  expect_error({
    get_assays(
      identifier = -1,
      namespace = "aid",
    )
  })
})
