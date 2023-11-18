get_compounds(
  identifier = "aspirin",
  namespace = "name"
)

# test for 'namespace' arg. ----
test_that("pulling compounds via 'name' is succesfull", {
  tmp <- try(get_compounds(
    identifier = "aspirin",
    namespace = "name"
  ))

  expect_false(inherits(tmp, "try-error"))
  expect_true(inherits(tmp, "list"))
  expect_true(all(length(tmp) == 1, names(tmp) == "'aspirin'"))
})


test_that("pulling compounds via 'smiles' is succesful", {
  tmp <- try(get_compounds(
    identifier = "CC(=O)OC1=CC=CC=C1C(=O)O",
    namespace = "smiles"
  ))

  expect_false(inherits(tmp, "try-error"))
  expect_true(inherits(tmp, "list"))
  expect_true(all(length(tmp) == 1, names(tmp) == "'CC(=O)OC1=CC=CC=C1C(=O)O'"))
})

test_that("pulling compounds via 'cid' is succesful", {
  tmp <- try(get_compounds(
    identifier = 2244,
    namespace = "cid"
  ))

  expect_false(inherits(tmp, "try-error"))
  expect_true(inherits(tmp, "list"))
  expect_true(all(length(tmp) == 1, names(tmp) == "'2244'"))
})


test_that("handling undefined/unknown/incorrect compounds.", {
  expect_error({
    get_compounds(
      identifier = -1,
      namespace = "cid"
    )
  })
})
