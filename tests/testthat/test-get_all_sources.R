
test_that("get all sources for 'substance'", {
  tmp <- try({
    get_all_sources(
      domain = 'substance'
    )
  })

  expect_false(inherits(tmp, "try-error"))
  expect_true(all(inherits(tmp, "character"), is.vector(tmp), length(tmp) > 0))
})

test_that("get all sources for 'assay'", {
  tmp <- try({
    get_all_sources(
      domain = 'assay'
    )
  })

  expect_false(inherits(tmp, "try-error"))
  expect_true(all(inherits(tmp, "character"), is.vector(tmp), length(tmp) > 0))
})

test_that("return error for undefined domain.", {
  expect_error({
    get_all_sources(
      domain = 'dncr'
    )
  })
})
