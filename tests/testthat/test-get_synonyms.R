
syns <- get_synonyms(
  identifier = c("aspirin", "ibuprofen"),
  namespace = "name"
)
testRequest(syns)

# synonyms() test ----
test_that("synonyms returns list or data.frame correctly", {
  expect_true({
    tmp <- synonyms(syns, .to.data.frame = TRUE)
    all(
      is.data.frame(tmp),
      nrow(tmp) > 0,
      ncol(tmp) == 2
    )
  })

  expect_true({
    tmp <- synonyms(syns, .to.data.frame = FALSE)
    all(
      is.list(tmp),
      length(tmp) > 1
    )
  })
})

# instance(), retrieve() tests ----
test_that("instance()/retrieve() returns error for 'get_synonyms()'", {
  expect_error(instance(syns))
  expect_error(retrieve(syns))
})

# test for 'namespace' arg. ----
test_that("pulling synonyms via 'name' is succesful", {
  tmp <- get_synonyms(
    identifier = "aspirin",
    namespace = "name"
  )

  expect_true(allSuccess(tmp))
})

test_that("pulling synonyms via 'cid' is succesful", {
  tmp <- get_synonyms(
    identifier = 2244,
    namespace = "cid"
  )

  expect_true(allSuccess(tmp))
})

test_that("return error for incomplete/unknown identifiers", {
  tmp <- get_synonyms(
    identifier = "dncr",
    namespace = "name"
  )

  expect_false(allSuccess(tmp))
  expect_false(is.null(tmp$result[[1]]$error))
})
