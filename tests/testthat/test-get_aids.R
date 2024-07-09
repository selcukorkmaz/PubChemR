# Helper Functions ----
toDataFrame <- function(object, ...){
  # Returned object is a tibble or data.frame
  resDF <- AIDs(object, .to.data.frame = TRUE)
  expect_true({
    any(inherits(resDF, "tbl"), inherits(resDF, "data.frame"), inherits(resDF, "tbl_df"))
    all(nrow(resDF) != 0, ncol(resDF) != 0)
  })

  # Returned object is a list
  resLIST <- AIDs(object, .to.data.frame = FALSE)
  expect_true({
    inherits(resLIST, "list")
    length(resLIST) > 0
  })
}

test1 <- function(object, ...){
  test_that(paste0("pulling aids via '", request_args(aids, "namespace"), "' is succesfull"), {
    expect_true(allSuccess(object))
  })

  test_that("AIDs succesfully returns 'data.frame' and 'list'", {
    toDataFrame(object)
  })
}

# Checking requests and AIDs ----
aids <- try(get_aids(
  identifier = "aspirin",
  namespace = "name"
))
test1(aids)

aids <- try(get_aids(
  identifier = "2244",
  namespace = "cid"
))
test1(aids)

aids <- try(get_aids(
  identifier = "CC(=O)OC1=CC=CC=C1C(=O)O",
  namespace = "smiles"
))
test1(aids)

test_that("pulling aids for multiple identifiers with undefined input.", {
  aids <- try(get_aids(
    identifier = c("aspirin", "dncr"),
    namespace = "name"
  ))

  expect_true(aids$result[[1]]$success)
  expect_false(aids$result[[2]]$success)
})

test_that("undefined/incorrect identifier returns error", {
  aids <- get_aids(
    identifier = "dncr",
    namespace = "name"
  )

  expect_false(aids$result[[1]]$success)
  expect_true(!is.null(aids$result[[1]]$error))
})
