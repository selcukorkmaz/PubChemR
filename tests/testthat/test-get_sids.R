# test for 'namespace' arg. ----
test_that("pulling SIDs via 'name' is succesful", {
  tmp <- try(get_sids(
    identifier = "aspirin",
    namespace = "name"
  ))

  expect_false(inherits(tmp, "try-error"))
  expect_true(any(inherits(tmp, "tbl"), inherits(tmp, "data.frame"), inherits(tmp, "tbl_df")))
  expect_true(ncol(tmp) > 0 & nrow(tmp) > 0)
})


test_that("pulling SIDs via 'smiles' is succesful", {
  tmp <- try(get_sids(
    identifier = "CC(=O)OC1=CC=CC=C1C(=O)O",
    namespace = "smiles"
  ))

  expect_false(inherits(tmp, "try-error"))
  expect_true(any(inherits(tmp, "tbl"), inherits(tmp, "data.frame"), inherits(tmp, "tbl_df")))
  expect_true(ncol(tmp) > 0 & nrow(tmp) > 0)
})

test_that("pulling SIDs via 'cid' is succesful", {
  tmp <- try(get_sids(
    identifier = 2244,
    namespace = "cid",
  ))

  expect_false(inherits(tmp, "try-error"))
  expect_true(any(inherits(tmp, "tbl"), inherits(tmp, "data.frame"), inherits(tmp, "tbl_df")))
  expect_true(ncol(tmp) > 0 & nrow(tmp) > 0)
})

test_that("pulling CIDs via multiple 'name' is succesful", {
  tmp <- try(get_sids(
    identifier = c("aspirin", "ibuprofen"),
    namespace = "name"
  ))

  expect_false(inherits(tmp, "try-error"))
  expect_true(any(inherits(tmp, "tbl"), inherits(tmp, "data.frame"), inherits(tmp, "tbl_df")))
  expect_true(ncol(tmp) > 0 & nrow(tmp) > 0)
  expect_true(all(unique(tmp$Compound) %in% c("aspirin", "ibuprofen")))
})

test_that("pulling CIDs via multiple 'cid' is succesful", {
  tmp <- try(get_sids(
    identifier = c(2244, 3627),
    namespace = "cid"
  ))

  expect_false(inherits(tmp, "try-error"))
  expect_true(any(inherits(tmp, "tbl"), inherits(tmp, "data.frame"), inherits(tmp, "tbl_df")))
  expect_true(ncol(tmp) > 0 & nrow(tmp) > 0)
  expect_true(all(unique(tmp$CID) %in% c(2244, 3627)))
})

test_that("handling unknown/incorrect/undefined 'identifiers'", {
  tmp <- try(get_sids(
    identifier = "dncr",
    namespace = "name",
  ))

  expect_true(inherits(tmp, "try-error"))
})
