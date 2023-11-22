# test for 'namespace' arg. ----
test_that("pulling JSON file via 'name' is succesful", {
  tmp <- try(get_json(
    identifier = "aspirin",
    namespace = "name"
  ))

  expect_false(inherits(tmp, "try-error"))
  expect_true(inherits(tmp, "list"))
  expect_true(length(tmp) > 0)
})


test_that("pulling JSON file via 'smiles' is succesful", {
  tmp <- try(get_json(
    identifier = "CC(=O)OC1=CC=CC=C1C(=O)O",
    namespace = "smiles"
  ))

  expect_false(inherits(tmp, "try-error"))
  expect_true(inherits(tmp, "list"))
  expect_true(length(tmp) > 0)
})


test_that("pulling JSON file via 'cid' is succesful", {
  tmp <- try(get_json(
    identifier = 2244,
    namespace = "cid"
  ))

  expect_false(inherits(tmp, "try-error"))
  expect_true(inherits(tmp, "list"))
  expect_true(length(tmp) > 0)
})

test_that("pulling JSON file via multiple 'cid' returns error", {
  tmp <- try({
    get_json(
      identifier = c(2244, 3627),
      namespace = "cid"
    )
  })

  # It returns NULL when encountered an error.
  expect_false(inherits(tmp, "try-error"))
  expect_false(is.null(tmp))
})
