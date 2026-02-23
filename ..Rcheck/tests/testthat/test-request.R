test_that("request builds canonical base URL", {
  url <- PubChemR:::request(identifier = "aspirin", namespace = "name")
  expect_equal(
    url,
    "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/aspirin/JSON"
  )
})

test_that("request encodes named options into query string", {
  url <- PubChemR:::request(
    identifier = "aspirin",
    namespace = "name",
    options = list(Threshold = 95, MaxRecords = 10)
  )

  expect_true(grepl("\\?", url))
  expect_true(grepl("Threshold=95", url))
  expect_true(grepl("MaxRecords=10", url))
})

test_that("request errors on unnamed options", {
  expect_error(
    PubChemR:::request(identifier = "aspirin", namespace = "name", options = list(95, 10)),
    "named list"
  )
})
