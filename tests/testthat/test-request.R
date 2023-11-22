test_that("return api path is successful.", {
  tmp <- try({
    request(
      identifier = "aspirin",
      namespace = "name"
    )
  })

  expect_false(inherits(tmp, "try-error"))
  expect_equal(tmp, "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/aspirin/JSON")
})

test_that("return api path for 'cid' is successful", {
  tmp <- try({
    request(
      identifier = 2244,
      namespace = "cid"
    )
  })

  expect_false(inherits(tmp, "try-error"))
  expect_equal(tmp, "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/2244/JSON")
})

# test_that("If identifier is multiple elements, join with commas into string", {
#   tmp <- try({
#     request(
#       identifier = c(2244, 3627),
#       namespace = "cid"
#     )
#   })
#
#   expect_false(inherits(tmp, "try-error"))
#   expect_equal(tmp, "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/c(\"2244\", \"3627\")/JSON")
# })


test_that("return error for NULL identifier.", {
  expect_error({
    request(
      identifier = NULL,
      namespace = "name"
    )
  })
})
