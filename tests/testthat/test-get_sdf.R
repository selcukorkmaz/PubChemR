# test for 'namespace' arg. ----
test_that("pulling SDFs via 'name' is succesful", {
  temp_dir <- tempdir(check = TRUE)

  expect_invisible({
    tmp <- try(get_sdf(
      identifier = "aspirin",
      namespace = "name",
      path = temp_dir
    ))
  })

  expect_true(file.exists(temp_dir))
  expect_true(file.exists(tmp))

  file.remove(tmp)
})

test_that("pulling SDFs via 'cid' is succesful", {
  temp_dir <- tempdir(check = TRUE)

  tmp <- try(get_sdf(
    identifier = 2244,
    namespace = "cid",
    path = temp_dir,
    file_name = "file"
  ))

  expect_true(file.exists(temp_dir))
  expect_true(file.exists(file.path(temp_dir, "file.sdf")))

  file.remove(file.path(temp_dir, "file.sdf"))
})

test_that("no SDF file saved for unknown/incorrect identifiers", {
  temp_dir <- tempdir(check = TRUE)

  expect_message({
    tmp <- try(get_sdf(
      identifier = "dncr",
      namespace = "name",
      path = temp_dir,
      file_name = "file"
    ))
  })

  expect_null(tmp)
})

test_that("save SDF files to temporary working directory when path is not specified.", {
  tmp <- try(get_sdf(
    identifier = "aspirin",
    namespace = "name",
    file_name = "file",
    path = NULL
  ))

  default_path <- tempdir(check = TRUE)
  expect_true(file.exists(default_path))
  expect_true(file.exists(file.path(default_path, "file.sdf")))
  file.remove(file.path(default_path, "file.sdf"))
})

