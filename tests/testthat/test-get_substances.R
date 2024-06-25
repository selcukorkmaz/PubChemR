subs <- get_substances(
  identifier = c("aspirin", "ibuprofen"),
  namespace = "name"
)

testRequest(subs)

# instance() tests ----
test_that("instance() is succesfull.", {
  expect_output(print(instance(subs, "aspirin")), "Substance Data from PubChem Database")
  expect_error(instance(subs, .which = "unknown_instance"))
})

test_that("instance() returns identical results when .which is NULL", {
  expect_identical(
    instance(subs),
    instance(subs, .which = "aspirin")
  )
})

test_that("incorrect/undefined substance identifier returns error", {
  tmp <- get_substances(
    identifier = "dncr",
    namespace = "name"
  )

  expect_false({
    allSuccess(tmp)
  })
  expect_true(all(!is.null(tmp$result[[1]]$error), is.list(tmp$result[[1]]$error)))
})

# retrieve() tests ----
test_that("'.verbose' works as expected", {
  expect_invisible(retrieve(instance(subs, "aspirin"), .slot = "comment", .to.data.frame = TRUE, .verbose = TRUE))
  expect_visible(retrieve(instance(subs, "aspirin"), .slot = "comment", .to.data.frame = TRUE, .verbose = FALSE))

  expect_invisible(retrieve(instance(subs, "aspirin"), .slot = "comment", .to.data.frame = FALSE, .verbose = TRUE))
  expect_visible(retrieve(instance(subs, "aspirin"), .slot = "comment", .to.data.frame = FALSE, .verbose = FALSE))
})

test_that("NULL returns for unknown/empty slots.", {
  tmp <- instance(subs, "aspirin")

  expect_null(retrieve(tmp, .slot = "comment", .to.data.frame = TRUE, .idx = 132))
  expect_null(retrieve(tmp, .slot = "unknown_slot", .to.data.frame = TRUE, .idx = 132))
})

test_that("return error for unavailable substance idx.", {
  tmp <- instance(subs, "aspirin")
  expect_error(retrieve(tmp, .slot = "comment", .to.data.frame = TRUE, .idx = 66666))
})

test_that("'.idx' of length >1 is not permitted.", {
  tmp <- instance(subs, "aspirin")
  expect_warning(retrieve(tmp, .slot = "sid", .to.data.frame = TRUE, .idx = 1:3))

  tmp1 <- suppressWarnings(retrieve(tmp, .slot = "sid", .to.data.frame = TRUE, .idx = 1:3))
  tmp2 <- retrieve(tmp, .slot = "sid", .to.data.frame = TRUE, .idx = 1)
  expect_true(identical(tmp1, tmp2))
})



