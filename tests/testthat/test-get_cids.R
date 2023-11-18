# test for 'namespace' arg. ----
test_that("pulling CIDs via 'name' is succesful", {
  tmp <- try(get_cids(
    identifier = "aspirin",
    namespace = "name",
  ))

  # Returned object is a tibble or data.frame
  state1 <- any(inherits(tmp, "tbl"), inherits(tmp, "data.frame"), inherits(tmp, "tbl_df"))
  state2 <- all(nrow(tmp) != 0, ncol(tmp) != 0)

  expect_true(all(state1, state2))
})


test_that("pulling CIDs via 'smiles' is succesful", {
  tmp <- try(get_cids(
    identifier = "CC(=O)OC1=CC=CC=C1C(=O)O",
    namespace = "smiles",
  ))

  # Returned object is a tibble or data.frame
  state1 <- any(inherits(tmp, "tbl"), inherits(tmp, "data.frame"), inherits(tmp, "tbl_df"))
  state2 <- all(nrow(tmp) != 0, ncol(tmp) != 0)

  expect_true(all(state1, state2))
})

test_that("pulling CIDs via 'cid' is succesful", {
  tmp <- try(get_cids(
    identifier = 2244,
    namespace = "cid",
  ))

  # Returned object is a tibble or data.frame
  state1 <- any(inherits(tmp, "tbl"), inherits(tmp, "data.frame"), inherits(tmp, "tbl_df"))
  state2 <- all(nrow(tmp) != 0, ncol(tmp) != 0)

  expect_true(all(state1, state2))
})

test_that("pulling CIDs via multiple 'name' is succesful", {
  tmp <- try(get_cids(
    identifier = c("aspirin", "ibuprofen"),
    namespace = "name",
  ))

  # Returned object is a tibble or data.frame
  state1 <- any(inherits(tmp, "tbl"), inherits(tmp, "data.frame"), inherits(tmp, "tbl_df"))
  state2 <- all(nrow(tmp) == 2, ncol(tmp) != 0)
  state3 <- all(c("aspirin", "ibuprofen") %in% tmp[["Compound"]])

  expect_true(all(state1, state2))
})

test_that("handling unknown/incorrect/undefined 'identifiers'", {
  tmp <- try(get_cids(
    identifier = c("aspirin", "dncr"),
    namespace = "name",
  ))

  # Returned object is a tibble or data.frame
  state1 <- any(inherits(tmp, "tbl"), inherits(tmp, "data.frame"), inherits(tmp, "tbl_df"))
  state2 <- all(nrow(tmp) == 2, ncol(tmp) != 0)
  state3 <- all(c("aspirin", "dncr") %in% tmp[["Compound"]])
  state4 <- tmp %>%
    dplyr::filter(Compound == "dncr") %>%
    .[["CID"]] == "No CID"

  expect_true(all(state1, state2, state3, state4))
})
