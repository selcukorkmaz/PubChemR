test_that("get PubChem data as JSON output (name)", {
  tmp <- try({
    get_pubchem(
      identifier = "aspirin",
      namespace = "name",
      output = "JSON"
    )
  })

  expect_false(inherits(tmp, "try-error"))
  expect_false(is.null(tmp))
})

test_that("get PubChem data as XML output (name)", {
  tmp <- try({
    get_pubchem(
      identifier = "aspirin",
      namespace = "name",
      output = "XML"
    )
  })

  expect_false(inherits(tmp, "try-error"))
  expect_false(is.null(tmp))
})

test_that("get PubChem data as SDF output (name)", {
  tmp <- try({
    get_pubchem(
      identifier = "aspirin",
      namespace = "name",
      output = "SDF"
    )
  })

  expect_false(inherits(tmp, "try-error"))
  expect_false(is.null(tmp))
})

test_that("return errors when undefined/unknown identifier is given (name)", {
  expect_error({
    get_pubchem(
      identifier = "dncr",
      namespace = "name",
      output = "JSON"
    )
  })
})

test_that("get PubChem data as JSON output (cid)", {
  tmp <- try({
    get_pubchem(
      identifier = "2244",
      namespace = "cid",
      output = "JSON"
    )
  })

  expect_false(inherits(tmp, "try-error"))
  expect_false(is.null(tmp))
})

test_that("multiple inputs (cid)", {
  tmp <- try({
    get_pubchem(
      identifier = c(2244, 3627),
      namespace = "cid",
      output = "JSON"
    )
  })

  expect_false(inherits(tmp, "try-error"))
  expect_false(is.null(tmp))
})
