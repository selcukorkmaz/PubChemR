# Create directory ----
# test_that("create 'path' if it does not exist.", {
#   temp_dir <- tempdir(check = TRUE)
#
#   try(download(
#     outformat = "json",
#     path = temp_dir,
#     identifier = "aspirin",
#     namespace = "name",
#     domain = "compound",
#     operation = NULL,
#     searchtype = NULL,
#     overwrite = FALSE
#   ))
#
#   expect_true(file.exists(temp_dir))
#   file.remove(file.path(temp_dir, "aspirin.json"))
#   file.remove(temp_dir)
# })

# 'overwrite' file/folder
test_that("overwrite file if it already exists.", {
  temp_dir <- tempdir(check = TRUE)

  try(download(
    outformat = "json",
    path = temp_dir,
    identifier = "aspirin",
    namespace = "name",
    domain = "compound",
    operation = NULL,
    searchtype = NULL,
    overwrite = FALSE
  ))

  if (file.exists(temp_dir)){
    ctime <- file.info(file.path(temp_dir, "aspirin.json"))$ctime

    try(download(
      outformat = "json",
      path = temp_dir,
      identifier = "aspirin",
      namespace = "name",
      domain = "compound",
      operation = NULL,
      searchtype = NULL,
      overwrite = TRUE
    ))

    mtime <- file.info(file.path(temp_dir, "aspirin.json"))$mtime
    expect_false(identical(ctime, mtime))

    file.remove(file.path(temp_dir, "aspirin.json"))
    # file.remove(temp_dir)
  }
})

# 'overwrite' file/folder error
test_that("return error if overwrite is FALSE and file exists.", {
  temp_dir <- tempdir()

  try(download(
    outformat = "json",
    path = temp_dir,
    identifier = "aspirin",
    namespace = "name",
    domain = "compound",
    operation = NULL,
    searchtype = NULL,
    overwrite = FALSE
  ))

  expect_error({
    download(
      outformat = "json",
      path = temp_dir,
      identifier = "aspirin",
      namespace = "name",
      domain = "compound",
      operation = NULL,
      searchtype = NULL,
      overwrite = FALSE
    )
  })

  file.remove(file.path(temp_dir, "aspirin.json"))
  # file.remove(temp_dir)
})

# Download file in SDF/JSON formats ----
test_that("create 'path' if it does not exist.", {
  temp_dir <- tempdir()

  try(download(
    outformat = "json",
    path = temp_dir,
    identifier = "aspirin",
    namespace = "name",
    domain = "compound",
    operation = NULL,
    searchtype = NULL,
    overwrite = FALSE
  ))

  try(download(
    outformat = "sdf",
    path = temp_dir,
    identifier = "aspirin",
    namespace = "name",
    domain = "compound",
    operation = NULL,
    searchtype = NULL,
    overwrite = FALSE
  ))

  json_file <- file.path(temp_dir, "aspirin.json")
  sdf_file <- file.path(temp_dir, "aspirin.sdf")

  expect_true(all(file.exists(json_file), file.exists(sdf_file)))
  file.remove(json_file)
  file.remove(sdf_file)
  # file.remove(temp_dir)
})

