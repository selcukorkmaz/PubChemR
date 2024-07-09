props <- get_properties(
  properties = c("MolecularWeight", "MolecularFormula", "HBondDonorCount",
                 "HBondAcceptorCount", "InChIKey", "InChI"),
  identifier = c("2244", "1234"),
  namespace = "cid",
  propertyMatch = list(
    .ignore.case = TRUE,
    type = "contain"
  )
)

testRequest(props)

test_that("instance() returns first item from complete list when .which=FALSE", {
  tmp <- instance(props, .which = NULL)
  expect_true(request_args(tmp, "identifier") == "2244")
})

test_that('all requested properties returned successfully.', {
  tmp <- instance(props)
  propNamesRequested <- request_args(props, "properties")
  propNamesReturned <- names(tmp$result$PropertyTable$Properties[[1]])

  expect_true(all(propNamesRequested %in% propNamesReturned))
})

test_that(".slot vector of length >1 returns warning and accepts the first element in .slot", {
  prop1234 <- instance(props, .which = "1234")

  expect_warning({
    tmp <- retrieve(
      prop1234,
      .slot = c("MolecularWeight", "MolecularFormula", "HBondDonorCount"),
      .to.data.frame = TRUE
    )
  })

  expect_true((
    all(is.data.frame(tmp), nrow(tmp) == 1, ncol(tmp) == 2)
  ))
})

test_that(".which is ignored when '.combine.all = TRUE'", {
  expect_identical(
    retrieve(props, .combine.all = TRUE, .which = "1234"),
    retrieve(props, .combine.all = TRUE, .which = NULL)
  )

  expect_identical(
    retrieve(props, .combine.all = TRUE, .which = "1234", .slot = "MolecularFormula"),
    retrieve(props, .combine.all = TRUE, .which = NULL, .slot = "MolecularFormula")
  )
})


test_that("pulling compounds via an unknown 'namespace'", {
  tmp <- get_compounds(
    identifier = c("2244", "1234"),
    namespace = "taxonomy"
  )
  expect_false(allSuccess(tmp))
})

test_that("get_properties() returns an object of class 'PubChemInstanceList'", {
  expect_true("PubChemInstanceList" %in% class(props))
})

# instance() tests.
test_that("instance() returns an object of class 'PubChemInstance'", {
  expect_true("PubChemInstance" %in% class(instance(props)))
  expect_output(print(instance(props)), "An object of class 'PubChemInstance'")
})

test_that("retrieve() returns selected slots as expected for a compound", {
  expect_identical(
    retrieve(props, .slot = "MolecularWeight", .which = "1234", .to.data.frame = TRUE),
    retrieve(instance(props, "1234"), .slot = "MolecularWeight", .to.data.frame = TRUE)
  )

  # Return results invisibly when .verbose = TRUE
  expect_visible({
    prop1234 <- instance(props, "1234")
    retrieve(prop1234, .slot = "MolecularWeight", .verbose = TRUE)
  })
})

test_that("retrieve() returns error when unknown/undefined slots or identifiers are provided.", {
  expect_error(retrieve(props, .which = "dncr"))
  expect_null(retrieve(props, .which = "1234", .slot = "unkown_slot"))
})

