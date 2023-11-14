# test for 'namespace' arg. ----
test_that("pulling aids via 'name' is succesfull", {
  tmp <- try(get_aids(
    identifier = "aspirin",
    namespace = "name",
  ))

  # Returned object is a tibble or data.frame
  state1 <- any(inherits(tmp, "tbl"), inherits(tmp, "data.frame"), inherits(tmp, "tbl_df"))
  state2 <- all(nrow(tmp) != 0, ncol(tmp) != 0)

  expect_true(all(state1, state2))
})

test_that("pulling aids via 'cid' is succesfull", {
  tmp <- try(get_aids(
    identifier = 2244,
    namespace = "cid",
  ))

  # Returned object is a tibble or data.frame
  state1 <- any(inherits(tmp, "tbl"), inherits(tmp, "data.frame"), inherits(tmp, "tbl_df"))
  state2 <- all(nrow(tmp) != 0, ncol(tmp) != 0)

  expect_true(all(state1, state2))
})

test_that("pulling aids via 'smiles' is succesfull", {
  tmp <- try(get_aids(
    identifier = "CC(=O)OC1=CC=CC=C1C(=O)O",
    namespace = "smiles",
  ))

  # Returned object is a tibble or data.frame
  state1 <- any(inherits(tmp, "tbl"), inherits(tmp, "data.frame"), inherits(tmp, "tbl_df"))
  state2 <- all(nrow(tmp) != 0, ncol(tmp) != 0)

  expect_true(all(state1, state2))
})
