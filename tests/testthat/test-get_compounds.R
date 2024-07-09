compounds <- get_compounds(
  identifier = c("2244", "1234"),
  namespace = "cid"
)
testRequest(compounds)

test_that("pulling compounds via an unknown 'namespace'", {
  tmp <- get_compounds(
    identifier = c("2244", "1234"),
    namespace = "taxonomy"
  )
  expect_false(allSuccess(tmp))
})

test_that("get_compounds() returns an object of class 'PubChemInstanceList'", {
  expect_true("PubChemInstanceList" %in% class(compounds))
})

# test for 'namespace' arg. ----
tmp <- get_compounds(
  identifier = "aspirin",
  namespace = "name"
)
testRequest(tmp)

tmp <- get_compounds(
  identifier = "CC(=O)OC1=CC=CC=C1C(=O)O",
  namespace = "smiles"
)
testRequest(tmp)

test_that("handling undefined/unknown/incorrect compounds. returns error.", {
  tmp <- get_compounds(
    identifier = -1,
    namespace = "cid"
  )

  expect_false(tmp$result[[1]]$success)
  expect_true(!is.null(tmp$result[[1]]$error))
})

# instance() tests. ----
test_that("instance() returns an object of class 'PubChemInstance'", {
  expect_true("PubChemInstance" %in% class(instance(compounds)))
  expect_output(print(instance(compounds)), "An object of class 'PubChemInstance'")
})

# retrieve() tests ----
test_that("retrieve() returns selected slots as expected for a compound", {
  expect_identical(
    retrieve(compounds, .slot = "id", .which = "2244", .to.data.frame = TRUE),
    retrieve(instance(compounds, "2244"), .slot = "id", .to.data.frame = TRUE)
  )

  expect_identical(
    retrieve(compounds, .slot = "id", .which = "2244", .to.data.frame = FALSE),
    retrieve(instance(compounds, "2244"), .slot = "id", .to.data.frame = FALSE)
  )

  # Return results invisibly when .verbose = TRUE
  expect_invisible(
    retrieve(compounds, .slot = "id", .to.data.frame = FALSE, .combine.all = TRUE, .verbose = TRUE)
  )

  # Combine all compounds into a data.frame
  expect_true({
    tmp <- retrieve(compounds, .slot = "atoms", .to.data.frame = TRUE, .combine.all = TRUE, .verbose = FALSE)
    all(request_args(compounds, "identifier") %in% tmp[["Identifier"]])
  })
})

test_that("retrieve() returns error when unknown/undefined slots or identifiers are provided.", {
  expect_error(retrieve(compounds, .which = "dncr"))
  expect_warning(retrieve(compounds, .which = "1234"))
  expect_true({
    tmp <- suppressWarnings(retrieve(compounds, .which = "1234"))
    is.null(tmp)
  })

  expect_null(retrieve(compounds, .which = "1234", .slot = "unkown_slot"))
})

test_that("checking the effect of '.verbose' argument in retrieve() function", {
  expect_invisible(retrieve(compounds, .slot = "id", .verbose = TRUE, .combine.all = TRUE))
})


