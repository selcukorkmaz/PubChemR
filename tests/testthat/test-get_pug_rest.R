test_that("pubChemData() works properly", {
  tmp <- get_pug_rest(identifier = "lipitor", namespace = "name", domain = "compound", output = "PNG")
  expect_true(is.array(pubChemData(tmp)))
})


test_that("'output' cannot be NULL", {
  tmp <- get_pug_rest(identifier = "10000", namespace = "sid", domain = "substance",
                      operation = "synonyms", output = NULL)
  expect_false(tmp$success)
})

test_that("retry the request and return error", {
  tmp <- suppressMessages({
    get_pug_rest(identifier = "10000aaa", namespace = "sid", domain = "substance",
                 operation = "synonyms", output = 'JSON')
  })

  expect_false(tmp$success)
})

test_that("tests for SDF output successful", {
  tmp <- get_pug_rest(identifier = "2244", namespace = "cid", domain = "compound", save = TRUE, output = "SDF")
  tmp2 <- get_pug_rest(identifier = "2244", namespace = "cid", domain = "compound", output = "SDF", save = TRUE,
                       file_name = "my_file")

  expect_true(tmp$success)
  expect_true({
    file.exists(file.path(tmp$fileDetails$Path, tmp$fileDetails$Name))
  })
  expect_true(tmp$fileDetails$Name == "files_downloaded.SDF")
  expect_true(all(is.list(pubChemData(tmp)), length(pubChemData(tmp)) == 0))

  # user defined file name
  expect_true(file.exists(file.path(tmp2$fileDetails$Path, tmp2$fileDetails$Name)))
  expect_true({
    tmp3 <- get_pug_rest(identifier = "dncr", namespace = "cid", domain = "compound", output = "SDF")
    !tmp3$success
  })

  # Remove temporary files.
  file.remove(file.path(tmp2$fileDetails$Path, tmp2$fileDetails$Name))
  file.remove(file.path(tmp$fileDetails$Path, tmp$fileDetails$Name))
})

test_that("test for PNG output successful", {
  tmp <- get_pug_rest(identifier = "lipitor", namespace = "name", domain = "compound", output = "PNG")
  tmp2 <- get_pug_rest(identifier = "lipitor", namespace = "name", domain = "compound", output = "PNG", save = TRUE)

  expect_true(all(tmp$success, is.array(tmp$result)))
  expect_true(all(tmp2$success, is.array(tmp$result)))

  expect_true({
    file.exists(file.path(tmp2$fileDetails$Path, tmp2$fileDetails$Name))
  })
  file.remove(file.path(tmp2$fileDetails$Path, tmp2$fileDetails$Name))
})

test_that("test for CSV output successful", {
  tmp <- get_pug_rest(identifier = 1:5, namespace = "cid", domain = "compound",
                      property = c("MolecularWeight", "MolecularFormula", "HBondDonorCount",
                                   "HBondAcceptorCount", "InChIKey", "InChI"),
                      output = "CSV", save = TRUE, file_name = "my_file")

  expect_true(tmp$success)
  expect_true({
    all(is.data.frame(tmp$result),
        nrow(tmp$result) == 5 & ncol(tmp$result) == 7,
        c("MolecularWeight", "MolecularFormula", "HBondDonorCount",
          "HBondAcceptorCount", "InChIKey", "InChI") %in% colnames(tmp$result))
  })

  expect_true({
    file.exists(file.path(tmp$fileDetails$Path, "my_file.CSV"))
  })
  file.remove(file.path(tmp$fileDetails$Path, "my_file.CSV"))
})

test_that("test for TXT output successful", {
  tmp <- get_pug_rest(identifier = "137349406", namespace = "sid", domain = "substance",
                      operation = c("xrefs","PatentID"), output = "TXT",
                      file_name = "my_file", save = TRUE)

  expect_true(tmp$success)
  expect_true({
    all(is.data.frame(tmp$result), nrow(tmp$result) > 0 & ncol(tmp$result) == 1)
  })

  expect_true({
    file.exists(file.path(tmp$fileDetails$Path, "my_file.TXT"))
  })
  file.remove(file.path(tmp$fileDetails$Path, "my_file.TXT"))
})

test_that("test for JSON output successful", {
  tmp <- get_pug_rest(identifier = "504526", namespace = "aid", domain = "assay",
                      operation = "description", output = "JSON", save = TRUE,
                      file_name = "my_file")

  expect_true(tmp$success)
  expect_true({
    all(is.list(tmp$result), length(tmp$result) >= 1)
  })

  expect_true({
    file.exists(file.path(tmp$fileDetails$Path, "my_file.JSON"))
  })
  file.remove(file.path(tmp$fileDetails$Path, "my_file.JSON"))
})

