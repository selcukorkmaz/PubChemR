test_that("internal heading collector finds nested biological sections", {
  sections <- list(
    list(
      TOCHeading = "Names and Identifiers",
      Section = list(
        list(
          TOCHeading = "Synonyms",
          Section = list()
        )
      )
    ),
    list(
      TOCHeading = "Chemical and Physical Properties",
      Section = list(
        list(
          TOCHeading = "Biological Test Results",
          Section = list(
            list(TOCHeading = "BioAssay Results", Section = list())
          )
        )
      )
    )
  )

  exact <- PubChemR:::.pv_collect_heading_sections(
    sections = sections,
    heading = "Biological Test Results",
    .match_type = "match"
  )
  expect_equal(length(exact), 1)
  expect_equal(exact[[1]]$TOCHeading, "Biological Test Results")

  partial <- PubChemR:::.pv_collect_heading_sections(
    sections = sections,
    heading = "Biological",
    .match_type = "contain"
  )
  expect_equal(length(partial), 1)
  expect_equal(partial[[1]]$TOCHeading, "Biological Test Results")
})

test_that("internal failed-section builder returns PugViewSection error object", {
  out <- PubChemR:::.pv_make_failed_section(
    message = "No section found.",
    request_args = list(identifier = "2244", domain = "compound")
  )

  expect_s3_class(out, "PugViewSection")
  expect_false(out$success)
  expect_match(out$error$Message, "No section found")
  expect_equal(out$error$Code, "SectionNotFound")
})

test_that("get_biological_test_results retrieves heading from live PubChem", {
  skip_on_cran()
  skip_if_not_live_smoke()
  skip_if_offline()

  out <- get_biological_test_results(
    identifier = "2244",
    domain = "compound"
  )

  expect_s3_class(out, "PugViewSection")
  expect_true(isTRUE(out$success))
  expect_equal(out$result$TOCHeading, "Biological Test Results")
})
