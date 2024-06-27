pview <- get_pug_view(identifier = "2244", annotation = "linkout", domain = "compound")
pview2 <- get_pug_view(identifier = "2244", annotation = "data", domain = "compound")

testRequest(pview)
testRequest(pview2)

test_that("multiple identifiers are not allowed.", {
  expect_warning({
    tmp <- get_pug_view(identifier = c("2244", "1234"), annotation = "linkout", domain = "compound")
  })
  expect_true(tmp$success)
})

test_that("returns error if 'identifier' is nULL", {
  tmp <- get_pug_view(identifier = NULL, annotation = "data", domain = "compound")
  expect_false(tmp$success)
  expect_true(all(!is.null(tmp$error), is.list(tmp$error)))
})

test_that("unknown domain returns error", {
  tmp <- get_pug_view(identifier = "2244", annotation = "data", domain = "compoundasd")
  expect_false(tmp$success)
  expect_true(all(!is.null(tmp$error), is.list(tmp$error)))
})

test_that("'annotation' cannot be NULL: Default is 'data'", {
  tmp <- suppressWarnings(get_pug_view(identifier = 2244, annotation = NULL, domain = "compound"))
  expect_identical(tmp, pview2)
})

# test_that("'save' works properly", {
#
# })

test_that("sectionList() returns a tibble with number of rows equal to the number of setions", {
  sectList <- sectionList(pview2)

  expect_true(any(c("tbl", "tbl_df") %in% class(sectList)))
  expect_equal(nrow(sectList), length(pview2$result$Record$Section))
})

test_that("extracting specific section lists form complete list.", {
  expect_true({
    tmp <- sectionList(pview2, .pattern = c("chemical", "safety"), .match_type = "contain")
    nrow(tmp) > 0
  })
  expect_null({
    suppressWarnings(sectionList(pview2, .pattern = "dncr", .match_type = "start"))
  })
  expect_error(sectionList(pview2, .pattern = 2, .match_type = "match"))
})

test_that("a series of tests on sectionList()", {
  sect <- section(pview2)

  expect_identical(section(pview2, .id = "S1"), section(pview2))
  expect_true({
    tmp <- sectionList(section(pview2, .id = "S1"))
    all(any(c("tbl", "tbl_df") %in% class(tmp)), nrow(tmp) > 0)
  })
  expect_output(section(sect, .id = "S3", .verbose = TRUE))
  expect_true({
    tmp <- retrieve(sect, .slot = "Description", .to.data.frame = TRUE)
    tmp2 <- retrieve(sect, .slot = "Description", .to.data.frame = FALSE)
    all(any(c("tbl", "tbl_df") %in% class(tmp)), nrow(tmp) > 0, !is.data.frame(tmp2))
  })

  expect_error(section(sect, .id = "S2222"))
  expect_null(retrieve(sect, .slot = "Description22", .to.data.frame = FALSE))
})

test_that("print sections in a nested way", {
  expect_output(section(section(pview2, .verbose = TRUE), .verbose = TRUE))
})

