# Helper Functions ----
toDataFrame <- function(object, ...){
  # Returned object is a tibble or data.frame
  resDF <- CIDs(object, .to.data.frame = TRUE)
  expect_true({
    any(inherits(resDF, "tbl"), inherits(resDF, "data.frame"), inherits(resDF, "tbl_df"))
    all(nrow(resDF) != 0, ncol(resDF) != 0)
  })

  # Returned object is a list
  resLIST <- CIDs(object, .to.data.frame = FALSE)
  expect_true({
    inherits(resLIST, "list")
    length(resLIST) > 0
  })
}

test1 <- function(object, ...){
  test_that(paste0("pulling cids via '", request_args(object, "namespace"), "' is succesfull"), {
    expect_true(allSuccess(object))
  })

  test_that("CIDs succesfully returns 'data.frame' and 'list'", {
    toDataFrame(object)
  })
}

# Checking requests and AIDs ----
cids <- try(get_cids(
  identifier = "aspirin",
  namespace = "name"
))
test1(cids)

cids <- try(get_cids(
  identifier = "CC(=O)OC1=CC=CC=C1C(=O)O",
  namespace = "smiles"
))
test1(cids)

test_that("pulling cids for multiple identifiers with undefined input.", {
  cids <- try(get_cids(
    identifier = c("aspirin", "dncr"),
    namespace = "name"
  ))

  expect_true(cids$result[[1]]$success)
  expect_false(cids$result[[2]]$success)
})

test_that("undefined/incorrect identifier returns error", {
  cids <- get_cids(
    identifier = "dncr",
    namespace = "name"
  )

  expect_false(cids$result[[1]]$success)
  expect_true(!is.null(cids$result[[1]]$error))
})
