# property_map() ----
test_that("returns error if 'x' is missing", {
  expect_error(property_map())
})

test_that('"x" is ignored if type = "all"', {
  expect_no_error(property_map(type = "all"))
})

test_that("match types works properly.", {
  expect_equal(property_map(x = "MolecularFormula", type = "match"), "MolecularFormula")
  expect_equal(property_map(x = "molecularform", type = "contain"), "MolecularFormula")
  expect_equal(property_map(x = "molecularf", type = "start"), "MolecularFormula")
  expect_equal(property_map(x = "formula", type = "end"), "MolecularFormula")
})

test_that("'.ignore.case' works properly.", {
  expect_equal(property_map(x = "molecularform", type = "contain", .ignore.case = TRUE), "MolecularFormula")
  expect_equal(property_map(x = "molecularform", type = "contain", .ignore.case = FALSE), NULL)
})

test_that("returns NULL when no match found.", {
  expect_null(property_map("some_text", type = "contain", .ignore.case = TRUE))
})

# namespace_text() ----
test_that("namespace_text() returns properly", {
  expect_no_error(namespace_text(x = "aid"))
  expect_equal(namespace_text(x = "aid"), "Assay ID")
  expect_equal(namespace_text(x = "some_text"), "Domain-Specific")
})

test_that("namespace_text() returns error if 'x' is missing", {
  expect_error(namespace_text())
})

# domain_text() ----
test_that("domain_text() returns properly", {
  expect_no_error(domain_text(x = "substance"))
  expect_equal(domain_text(x = "substance"), "Substance")
  expect_equal(domain_text(x = "some_text"), "Domain-Specific")
})

test_that("namespace_text() returns error if 'x' is missing", {
  expect_error(domain_text())
})

# primary_class() ----
test_that("succesfully returned primary class of given object", {
  tmp <- get_assays(identifier = 1234, namespace = "aid")
  expect_equal(primaryClass(instance(tmp)), "PubChemInstance")
})

# find_last_layer() ----
test_that("find_last_layer() returns the given input if it is not a list or does not have nested layers", {
  expect_equal(find_last_layer(1), 1)

  tmp <- list(one = 1, two = 2)
  expect_identical(find_last_layer(tmp), tmp)

  tmp <- list(list(one = 1, two = 2))
  expect_identical(find_last_layer(tmp), tmp[[1]])
})




