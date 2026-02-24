test_that("get_properties handles comma-separated property strings", {
  local_mocked_bindings(
    pc_collect_instances = function(...) {
      list(...)
    },
    .package = "PubChemR"
  )

  out <- get_properties(
    properties = "MolecularWeight,XLogP",
    identifier = 2244,
    namespace = "cid"
  )

  expect_equal(out$operation, "property/MolecularWeight,XLogP")
})

test_that("get_all_sources rejects unsupported domain values", {
  expect_error(
    get_all_sources(domain = "compound"),
    "substance' or 'assay"
  )
})

test_that("download writes binary payloads safely", {
  td <- tempfile("pubchemr-download-")
  dir.create(td, recursive = TRUE)

  raw_payload <- as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x00, 0x01, 0x02))

  local_mocked_bindings(
    get_pubchem = function(...) raw_payload,
    .package = "PubChemR"
  )

  out <- PubChemR::download(
    filename = "binary",
    outformat = "PNG",
    path = td,
    identifier = 2244,
    namespace = "cid",
    domain = "compound",
    overwrite = TRUE
  )

  expect_true(file.exists(out))
  expect_equal(readBin(out, "raw", n = length(raw_payload)), raw_payload)
})

test_that("get_sdf supports vector identifiers and returns multiple paths", {
  td <- tempfile("pubchemr-sdf-")
  dir.create(td, recursive = TRUE)

  fake_response <- structure(list(status_code = 200L), class = "response")
  local_mocked_bindings(
    request = function(...) "https://example.org/fake.sdf",
    download.file = function(url, destfile, quiet = TRUE, mode = "wb", ...) {
      writeBin(charToRaw("SDF"), destfile)
      0L
    },
    .package = "PubChemR"
  )
  local_mocked_bindings(
    HEAD = function(...) fake_response,
    status_code = function(...) 200L,
    .package = "httr"
  )

  out <- get_sdf(
    identifier = c("2244", "3672"),
    namespace = "cid",
    path = td,
    file_name = "compound"
  )

  expect_type(out, "character")
  expect_length(out, 2)
  expect_true(all(file.exists(out)))
  expect_equal(basename(out), c("compound_2244.sdf", "compound_3672.sdf"))
})
