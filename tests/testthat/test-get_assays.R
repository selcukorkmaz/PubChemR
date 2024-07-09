assay <- get_assays(
  identifier = c("1234", "7815"),
  namespace = "aid"
)
testRequest(assay)

test_that("pulling assays via an unknown 'namespace'", {
  tmp <- get_assays(
    identifier = c("2244", "1234"),
    namespace = "cid"
  )
  expect_false(allSuccess(tmp))
})

test_that("get_assay() returns an object of class 'PubChemInstanceList'", {
  expect_true("PubChemInstanceList" %in% class(assay))
})

# instance() tests.
test_that("instance() returns an object of class 'PubChemInstance'", {
  expect_true("PubChemInstance" %in% class(instance(assay)))
  expect_output(print(instance(assay)), "An object of class 'PubChemInstance'")
})

test_that("retrieve() returns selected slots as expected for an assay", {
  expect_identical(
    retrieve(assay, .slot = "aid", .which = "1234", .to.data.frame = TRUE),
    retrieve(instance(assay, "1234"), .slot = "aid", .to.data.frame = TRUE)
  )

  expect_identical(
    retrieve(assay, .slot = "aid", .which = "1234", .to.data.frame = FALSE),
    retrieve(instance(assay, "1234"), .slot = "aid", .to.data.frame = FALSE)
  )

  # Return results invisibly when .verbose = TRUE
  expect_invisible(
    retrieve(assay, .slot = "aid", .to.data.frame = FALSE, .combine.all = TRUE, .verbose = TRUE)
  )

  # Combine all assays into a data.frame
  expect_true({
    tmp <- retrieve(assay, .slot = "aid", .to.data.frame = TRUE, .combine.all = TRUE, .verbose = FALSE)
    all(request_args(assay, "identifier") %in% tmp[["Identifier"]])
  })
})

test_that("retrieve() returns error when unknown/undefined slots or identifiers are provided.", {
  expect_error(retrieve(assay, .which = "dncr"))
  expect_warning(retrieve(assay, .which = "1234"))
  expect_true({
    tmp <- suppressWarnings(retrieve(assay, .which = "1234"))
    is.null(tmp)
  })

  expect_null(retrieve(assay, .which = "1234", .slot = "unkown_slot"))
})


test_that("checking the effect of '.verbose' argument in retrieve() function", {
  expect_output(retrieve(assay, .slot = "comment", .verbose = TRUE))
  expect_invisible(retrieve(assay, .slot = "comment", .verbose = TRUE, .combine.all = TRUE))
})

