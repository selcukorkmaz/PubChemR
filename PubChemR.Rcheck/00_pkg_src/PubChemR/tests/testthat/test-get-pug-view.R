test_that("get_pug_view allows NULL identifier for annotations + heading", {
  skip_on_cran()
  skip_if_offline()

  out <- get_pug_view(
    annotation = "annotations",
    identifier = NULL,
    domain = "compound",
    heading = "Pharmacology and Biochemistry"
  )

  expect_s3_class(out, "PugViewInstance")
  expect_false(isTRUE(identical(out$error$Message, "'identifier' cannot be NULL.")))
})
