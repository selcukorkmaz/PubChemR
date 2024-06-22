# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(PubChemR)

# Functions used globally in package tests (testthat) ----
allSuccess <- function(object){
  all(unlist(lapply(object$result, "[[", "success")))
}

testRequest <- function(object, ...){
  test_that(paste0("pulling via '", request_args(object, "namespace"), "' is succesfull"), {
    expect_true(allSuccess(object))
  })

  test_that("prints output to the R Console", {
    expect_output(print(object))
  })
}

test_check("PubChemR")
