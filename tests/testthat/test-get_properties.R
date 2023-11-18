test_that("Property: MolecularFormula --- SUCCESS", {
  tmp <- try({
    get_properties(
      properties = "MolecularFormula",
      identifier = "aspirin",
      namespace = "name"
    )
  })

  expect_false(is.null(tmp[[1]]$MolecularFormula))
  expect_true(tmp[[1]]$CID == 2244)

  # As data frame.
  tmp <- try({
    get_properties(
      properties = "MolecularFormula",
      identifier = "aspirin",
      namespace = "name",
      as_dataframe = TRUE
    )
  })

  expect_false(is.null(tmp))
  expect_true(tmp$CID == 2244)
  expect_true(ncol(tmp) > 0)
})

test_that("Property: MolecularWeight --- SUCCESS", {
  tmp <- try({
    get_properties(
      properties = "MolecularWeight",
      identifier = "aspirin",
      namespace = "name"
    )
  })

  expect_false(is.null(tmp[[1]][["MolecularWeight"]]))
  expect_true(tmp[[1]]$CID == 2244)

  tmp <- try({
    get_properties(
      properties = "MolecularWeight",
      identifier = "aspirin",
      namespace = "name",
      as_dataframe = TRUE
    )
  })

  expect_false(is.null(tmp))
  expect_true(tmp$CID == 2244)
  expect_true(ncol(tmp) > 0)
})

test_that("Property: CanonicalSMILES --- SUCCESS", {
  tmp <- try({
    get_properties(
      properties = "CanonicalSMILES",
      identifier = "aspirin",
      namespace = "name"
    )
  })

  expect_false(is.null(tmp[[1]][["CanonicalSMILES"]]))
  expect_true(tmp[[1]]$CID == 2244)

  tmp <- try({
    get_properties(
      properties = "CanonicalSMILES",
      identifier = "aspirin",
      namespace = "name",
      as_dataframe = TRUE
    )
  })

  expect_false(is.null(tmp))
  expect_true(tmp$CID == 2244)
  expect_true(ncol(tmp) > 0)
})

test_that("Property: IsomericSMILES --- SUCCESS", {
  tmp <- try({
    get_properties(
      properties = "IsomericSMILES",
      identifier = "aspirin",
      namespace = "name"
    )
  })

  expect_false(is.null(tmp[[1]][["IsomericSMILES"]]))
  expect_true(tmp[[1]]$CID == 2244)

  tmp <- try({
    get_properties(
      properties = "IsomericSMILES",
      identifier = "aspirin",
      namespace = "name",
      as_dataframe = TRUE
    )
  })

  expect_false(is.null(tmp))
  expect_true(tmp$CID == 2244)
  expect_true(ncol(tmp) > 0)
})

test_that("Property: InChI --- SUCCESS", {
  tmp <- try({
    get_properties(
      properties = "InChI",
      identifier = "aspirin",
      namespace = "name"
    )
  })

  expect_false(is.null(tmp[[1]][["InChI"]]))
  expect_true(tmp[[1]]$CID == 2244)

  tmp <- try({
    get_properties(
      properties = "InChI",
      identifier = "aspirin",
      namespace = "name",
      as_dataframe = TRUE
    )
  })

  expect_false(is.null(tmp))
  expect_true(tmp$CID == 2244)
  expect_true(ncol(tmp) > 0)
})

test_that("Unknown/Undefined property --- SUCCESS", {
  expect_error({
    get_properties(
      properties = "dncr",
      identifier = "aspirin",
      namespace = "name"
    )
  })
})
