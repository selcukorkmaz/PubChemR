test_that("pulling substances via 'name' is successful.", {
  tmp <- try({
    get_substances(
      identifier = "aspirin",
      namespace = "name"
    )
  })

  expect_false(inherits(tmp, "try-error"))
  expect_true(all(length(tmp) > 0, names(tmp) == "Substance_aspirin"))
})

test_that("pulling multiple substances is successful.", {
  tmp <- try({
    get_substances(
      identifier = c("aspirin", "ibuprofen"),
      namespace = "name"
    )
  })

  expect_false(inherits(tmp, "try-error"))
  expect_true(all(names(tmp) %in% c("Substance_aspirin", "Substance_ibuprofen")))
})

test_that("incorrect/undefined substance identifier", {
  expect_error({
    get_substances(
      identifier = "dncr",
      namespace = "name"
    )
  })
})


