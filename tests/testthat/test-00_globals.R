# request_args() ----
test_that("request_args() works properly", {
  compound <- get_compounds(
    identifier = "aspirin",
    namespace = "name"
  )

  expect_equal(request_args(compound, .which = "namespace"), "name")
  expect_true({
    is.list(request_args(compound))
  })

  expect_null(request_args(compound, "some_args"))
})

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
  expect_equal(namespace_text(x = "aid"), "AID")
  expect_equal(namespace_text(x = "some_text"), "DomainSpecific")
})

test_that("namespace_text() returns error if 'x' is missing", {
  expect_error(namespace_text())
})

# domain_text() ----
test_that("domain_text() returns properly", {
  expect_no_error(domain_text(x = "substance"))
  expect_equal(domain_text(x = "substance"), "Substance")
  expect_equal(domain_text(x = "some_text"), "DomainSpecific (some_text)")
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

# printSlotDetails() ----
test_that("slot details successfully printed", {
  pview <- get_pug_view(identifier = "1234", annotation = "data", domain = "compound")
  expect_output(printSlotDetails(find_last_layer(pview$result)))
  expect_output(printSlotDetails(find_last_layer(pview$result), pugViewSection = TRUE))
})

# printSectionDetails() ----
test_that("section details successfully printed", {
  pview <- get_pug_view(identifier = "1234", annotation = "data", domain = "compound")
  sect <- section(pview, "S1")

  expect_output(print(section(pview, "S1")))
  expect_output(printSectionDetails(sect$result))
  expect_output(printSectionDetails(section(sect)$result))
})


# calculateObjectSize() ----
# test_that("calculateObjectSize() works properly", {
#   path <- tempdir(check = TRUE)
#
#   write.csv(iris, file = file.path(path, "iris.csv"))
#   expect_true({
#     tmp <- calculateObjectSize(file.path(path, "iris.csv"))
#     all(is.numeric(tmp$size), abs(tmp$size - 4.71) < .02, tmp$unit == "KB")
#   })
#
#   expect_true(calculateObjectSize("a")$size == 0)
#   file.remove(file.path(path, "iris.csv"))
# })
