test_that("pc_profile applies predefined settings", {
  original <- pc_config()
  on.exit(do.call(pc_config, original), add = TRUE)

  cfg <- pc_profile("cloud")
  expect_true(is.list(cfg))
  expect_equal(cfg$rate_limit, 3)
  expect_equal(cfg$retries, 5)

  cfg2 <- pc_profile("default", rate_limit = 7)
  expect_equal(cfg2$rate_limit, 7)
})

test_that("pc_similarity_search builds similarity request metadata", {
  out <- pc_similarity_search(
    identifier = "CCO",
    namespace = "smiles",
    threshold = 88,
    max_records = 25,
    cache = TRUE,
    offline = TRUE
  )

  expect_s3_class(out, "PubChemResult")
  expect_s3_class(out, "PubChemIdMap")
  expect_false(out$success)
  expect_equal(out$error$code, "OfflineCacheMiss")
  expect_equal(out$request$searchtype, "similarity")
  expect_equal(out$request$options$Threshold, 88)
  expect_equal(out$request$options$MaxRecords, 25L)
})

test_that("pc_activity_matrix creates wide CID x AID matrix", {
  dat <- tibble::tibble(
    CID = c(1, 1, 2, 3),
    AID = c(10, 10, 10, 11),
    ActivityOutcome = c("Inactive", "Active", "Inactive", "Active")
  )

  mat <- pc_activity_matrix(dat, aggregate = "max")
  expect_s3_class(mat, "PubChemTable")
  expect_true(all(c("CID", "AID_10", "AID_11") %in% names(mat)))
  expect_equal(mat$AID_10[mat$CID == "1"], 1)
  expect_equal(mat$AID_10[mat$CID == "2"], 0)
})

test_that("pc_cross_domain_join composes domain tables", {
  compounds <- tibble::tibble(CID = c("1", "2"), Name = c("A", "B"))
  substances <- tibble::tibble(CID = c("1", "2"), SID = c("S1", "S2"))
  assays <- tibble::tibble(CID = c("1", "2"), AID = c("100", "101"), Score = c(0.2, 0.7))
  targets <- tibble::tibble(AID = c("100", "101"), Target = c("T1", "T2"))

  out <- pc_cross_domain_join(
    compounds = compounds,
    substances = substances,
    assays = assays,
    targets = targets
  )

  expect_s3_class(out, "PubChemTable")
  expect_equal(nrow(out), 2)
  expect_true(all(c("SID", "AID", "Target") %in% names(out)))
})

test_that("pc_model_matrix creates numeric predictors and outcome", {
  dat <- tibble::tibble(
    CID = c(1, 2, 3),
    MolecularWeight = c("180.16", "206.28", "194.19"),
    XLogP = c("1.2", "2.5", "2.1"),
    Active = c(1, 0, 1)
  )

  mm <- pc_model_matrix(dat, outcome = "Active", na_fill = 0)
  expect_s3_class(mm, "PubChemModelMatrix")
  expect_true(is.matrix(mm$x))
  expect_equal(nrow(mm$x), 3)
  expect_equal(ncol(mm$x), 2)
  expect_equal(length(mm$y), 3)
})

test_that("pc_lifecycle_policy returns lifecycle contract table", {
  pol <- pc_lifecycle_policy()
  expect_s3_class(pol, "tbl_df")
  expect_equal(nrow(pol), 2)
  expect_true(all(c("stream", "stability", "deprecation_notice") %in% names(pol)))
})

test_that("pc_to_rcdk fails clearly when optional dependency is missing", {
  tbl <- tibble::tibble(CID = c(1), CanonicalSMILES = c("CCO"))

  installed_pkgs <- rownames(installed.packages())
  if ("rcdk" %in% installed_pkgs) {
    skip("Optional dependency 'rcdk' is installed; missing-dependency branch not applicable.")
  }
  expect_error(pc_to_rcdk(tbl), "rcdk")
})

test_that("pc_to_chemminer fails clearly when optional dependency is missing", {
  tbl <- tibble::tibble(CID = c(1), CanonicalSMILES = c("CCO"))

  installed_pkgs <- rownames(installed.packages())
  if ("ChemmineR" %in% installed_pkgs) {
    skip("Optional dependency 'ChemmineR' is installed; missing-dependency branch not applicable.")
  }
  expect_error(pc_to_chemminer(tbl), "ChemmineR")
})
