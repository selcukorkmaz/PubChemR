
allSuccess <- function(object){
  all(unlist(lapply(object$result, "[[", "success")))
}

testAssayRequest <- function(object, ...){
  test_that(paste0("pulling assays via '", request_args(object, "namespace"), "' is succesfull"), {
    expect_true(allSuccess(object))
  })
}

assay <- get_assays(
  identifier = c("1234", "7815"),
  namespace = "aid"
)
testAssayRequest(assay)

test_that("pulling assays via an unknown 'namespace'", {
  tmp <- get_assays(
    identifier = c("2244", "1234"),
    namespace = "cid"
  )
  expect_false(allSuccess(tmp))
})

# instance() tests.
test_that("instance() returns an object of class 'PubChemInstance'", {
  expect_true("PubChemInstance" %in% class(instance(assay)))
})
