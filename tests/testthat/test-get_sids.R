# Helper Functions ----
toDataFrame <- function(object, ...){
  # Returned object is a tibble or data.frame
  resDF <- SIDs(object, .to.data.frame = TRUE)
  expect_true({
    any(inherits(resDF, "tbl"), inherits(resDF, "data.frame"), inherits(resDF, "tbl_df"))
    all(nrow(resDF) != 0, ncol(resDF) != 0)
  })

  # Returned object is a list
  resLIST <- SIDs(object, .to.data.frame = FALSE)
  expect_true({
    inherits(resLIST, "list")
    length(resLIST) > 0
  })
}

test1 <- function(object, ...){
  test_that(paste0("pulling sids via '", request_args(object, "namespace"), "' is succesfull"), {
    expect_true(allSuccess(object))
  })

  test_that("SIDs succesfully returns 'data.frame' and 'list'", {
    toDataFrame(object)
  })
}

# Checking requests and AIDs ----
sids <- try(get_sids(
  identifier = "aspirin",
  namespace = "name"
))
test1(sids)

sids <- try(get_sids(
  identifier = "2244",
  namespace = "cid"
))
test1(sids)

sids <- try(get_sids(
  identifier = "CC(=O)OC1=CC=CC=C1C(=O)O",
  namespace = "smiles"
))
test1(sids)

test_that("pulling sids for multiple identifiers with undefined input.", {
  sids <- try(get_sids(
    identifier = c("aspirin", "dncr"),
    namespace = "name"
  ))

  expect_true(sids$result[[1]]$success)
  expect_false(sids$result[[2]]$success)
})

test_that("undefined/incorrect identifier returns error", {
  sids <- get_sids(
    identifier = "dncr",
    namespace = "name"
  )

  expect_false(sids$result[[1]]$success)
  expect_true(!is.null(sids$result[[1]]$error))
})
