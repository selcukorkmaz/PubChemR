# test for 'namespace' arg. ----
test_that("pulling synonyms via 'name' is succesful", {
  tmp <- try(get_synonyms(
    identifier = "aspirin",
    namespace = "name"
  ))

  expect_false(inherits(tmp, "try-error"))
  expect_true(length(tmp) > 0)
})

test_that("pulling synonyms via 'cid' is succesful", {
  tmp <- try(get_synonyms(
    identifier = 2244,
    namespace = "cid"
  ))

  expect_false(inherits(tmp, "try-error"))
  expect_true(length(tmp) > 0)
})

test_that("return error for incomplete/unknown identifiers", {
  expect_message({
    tmp <- try(get_synonyms(
      identifier = "dncr",
      namespace = "name"
    ))
  })

  expect_true(length(tmp) == 0)
})

test_that("return error for incomplete/unknown identifiers", {
  expect_message({
    tmp <- try(get_synonyms(
      identifier = c("aspirin", "ibuprofen"),
      namespace = "name"
    ))
  })

  expect_true(length(tmp) == 0)
})

