# test for 'namespace' arg. ----
test_that("pulling aids via 'name' is succesfull", {
  tmp <- try(get_aids(
    identifier = "aspirin",
    namespace = "name",
    as_data_frame = TRUE
  ))

  # Returned object is a tibble or data.frame
  expect_false(inherits(tmp, "try-error"))
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

test_that("pulling aids for multiple identifiers.", {
  tmp <- try(get_aids(
    identifier = c("aspirin", "ibuprofen"),
    namespace = "name",
    as_data_frame = TRUE
  ))

  expect_false(inherits(tmp, "try-error"))
  expect_true(any(inherits(tmp, "tbl"), inherits(tmp, "tbl_df"), inherits(tmp, "data.frame")))
  expect_true(all(unique(tmp$Compound) %in% c("aspirin", "ibuprofen")))

  tmp <- try(get_aids(
    identifier = c("aspirin", "ibuprofen"),
    namespace = "name",
    as_data_frame = FALSE
  ))

  expect_false(inherits(tmp, "try-error"))
  expect_true(inherits(tmp, "list"))
  expect_true(all(gsub("'", "", names(tmp)) %in% c("aspirin", "ibuprofen")))
})

test_that("pulling aids for multiple identifiers with undefined input.", {
  tmp <- get_aids(
    identifier = c("aspirin", "dncr"),
    namespace = "name",
    as_data_frame = TRUE
  )

  expect_false(inherits(tmp, "try-error"))
  expect_equal(unique(tmp$Compound), "aspirin")
})


test_that("return error for undefined identifiers.", {
  expect_message({
    tmp <- get_aids(
      identifier = "dncr",
      namespace = "name",
      as_data_frame = TRUE
    )
  })

  expect_false(inherits(tmp, "try-error"))
  expect_true(any(inherits(tmp, "tbl"), inherits(tmp, "tbl_df"), inherits(tmp, "data.frame")))
  expect_equal(nrow(tmp), 0)
  expect_equal(ncol(tmp), 0)
})
