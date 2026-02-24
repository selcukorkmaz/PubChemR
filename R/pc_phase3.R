#' Apply a Predefined Execution Profile
#'
#' @param profile One of `"default"`, `"cloud"`, or `"high_throughput"`.
#' @param ... Named overrides applied on top of the selected profile.
#'
#' @return A named list with active transport configuration.
#' @export
pc_profile <- function(profile = c("default", "cloud", "high_throughput"), ...) {
  profile <- match.arg(profile)

  base <- switch(
    profile,
    default = list(
      rate_limit = 5,
      timeout = 60,
      retries = 3,
      pause_base = 1,
      pause_cap = 8,
      cache_ttl = 24 * 60 * 60
    ),
    cloud = list(
      rate_limit = 3,
      timeout = 120,
      retries = 5,
      pause_base = 1,
      pause_cap = 20,
      cache_ttl = 7 * 24 * 60 * 60
    ),
    high_throughput = list(
      rate_limit = 10,
      timeout = 60,
      retries = 4,
      pause_base = 0.5,
      pause_cap = 10,
      cache_ttl = 24 * 60 * 60
    )
  )

  overrides <- list(...)
  if (length(overrides) > 0) {
    base[names(overrides)] <- overrides
  }
  cfg <- base
  do.call(pc_config, cfg)
}

#' Similarity-Driven Identifier Search
#'
#' @param identifier Query identifier(s) for similarity search.
#' @param namespace Namespace for query identifiers.
#' @param domain PubChem domain.
#' @param to Output mapping target (`"cids"`, `"sids"`, or `"aids"`).
#' @param searchtype Similarity mode (`"similarity"`, `"fastsimilarity_2d"`, `"fastsimilarity_3d"`).
#' @param threshold Similarity threshold.
#' @param max_records Optional maximum number of records returned by PubChem.
#' @param options Optional named list of additional query options.
#' @param ... Additional arguments passed to `pc_request()`.
#'
#' @return A typed object inheriting from `PubChemIdMap`.
#' @export
pc_similarity_search <- function(identifier,
                                 namespace = c("smiles", "cid", "inchi", "sdf"),
                                 domain = "compound",
                                 to = c("cids", "sids", "aids"),
                                 searchtype = c("similarity", "fastsimilarity_2d", "fastsimilarity_3d"),
                                 threshold = 95,
                                 max_records = NULL,
                                 options = NULL,
                                 ...) {
  if (missing(identifier) || length(identifier) == 0) {
    stop("'identifier' must contain at least one query value.")
  }

  namespace <- match.arg(namespace)
  to <- match.arg(to)
  searchtype <- match.arg(searchtype)

  if (!is.numeric(threshold) || length(threshold) != 1 || is.na(threshold)) {
    stop("'threshold' must be a single numeric value.")
  }
  if (threshold < 0 || threshold > 100) {
    stop("'threshold' must be between 0 and 100.")
  }
  if (!is.null(max_records) && (!is.numeric(max_records) || max_records <= 0)) {
    stop("'max_records' must be a positive integer or NULL.")
  }
  if (!is.null(options) && !is.list(options)) {
    stop("'options' must be NULL or a named list.")
  }

  opt <- options %||% list()
  opt$Threshold <- as.numeric(threshold)
  if (!is.null(max_records)) {
    opt$MaxRecords <- as.integer(max_records)
  }

  out <- pc_request(
    domain = domain,
    namespace = namespace,
    identifier = identifier,
    operation = to,
    searchtype = searchtype,
    output = "JSON",
    options = opt,
    ...
  )

  class(out) <- unique(c("PubChemSimilarityResult", "PubChemIdMap", class(out)))
  out
}

pc_as_tibble_input <- function(x) {
  if (inherits(x, "PubChemResult")) {
    return(tibble::as_tibble(x))
  }
  if (is.data.frame(x)) {
    return(tibble::as_tibble(x))
  }
  stop("'x' must be a data.frame/tibble or a PubChemResult object.")
}

#' Harmonize Activity Outcome Labels
#'
#' Maps textual activity outcomes (for example, `Active`/`Inactive`) to
#' numeric values for modeling workflows.
#'
#' @param values Outcome values (character/factor/numeric).
#' @param map Optional named numeric map. Names are matched
#'   case-insensitively after trimming.
#' @param strict Logical. If `TRUE`, unknown non-empty labels raise an error.
#' @param unknown Numeric value assigned to unknown labels when
#'   `strict = FALSE`.
#'
#' @return Numeric vector aligned with `values`.
#' @export
pc_activity_outcome_map <- function(values,
                                    map = NULL,
                                    strict = FALSE,
                                    unknown = NA_real_) {
  if (is.numeric(values)) {
    return(as.numeric(values))
  }

  default_map <- c(
    Active = 1,
    Inactive = 0,
    Inconclusive = NA_real_,
    Unknown = NA_real_,
    Unspecified = NA_real_,
    "Not Tested" = NA_real_,
    Hit = 1,
    "Non-Hit" = 0
  )

  if (is.null(map)) {
    map_use <- default_map
  } else {
    if (is.null(names(map)) || any(names(map) == "")) {
      stop("'map' must be a named numeric vector.")
    }
    map_use <- default_map
    map_use[names(map)] <- as.numeric(map)
  }

  vals <- as.character(values)
  keys <- trimws(tolower(vals))
  key_map <- trimws(tolower(names(map_use)))

  idx <- match(keys, key_map)
  out <- as.numeric(unname(map_use[idx]))

  # Preserve explicit NA inputs.
  out[is.na(vals)] <- NA_real_

  unknown_mask <- is.na(idx) & !is.na(vals) & nzchar(trimws(vals))
  if (isTRUE(strict) && any(unknown_mask)) {
    bad <- unique(vals[unknown_mask])
    stop(
      "Unknown activity outcome label(s): ",
      paste(bad, collapse = ", "),
      ". Provide 'map' entries or set strict = FALSE."
    )
  }

  out[unknown_mask] <- as.numeric(unknown)
  out
}

pc_scale_matrix <- function(x) {
  means <- colMeans(x, na.rm = TRUE)
  sds <- apply(x, 2, function(v) {
    vv <- v[!is.na(v)]
    if (length(vv) <= 1) return(1)
    sdv <- sqrt(sum((vv - mean(vv))^2) / (length(vv) - 1))
    if (!is.finite(sdv) || sdv == 0) 1 else sdv
  })
  out <- sweep(x, 2, means, FUN = "-")
  sweep(out, 2, sds, FUN = "/")
}

#' Build an Assay Activity Matrix
#'
#' @param x A long-form table or `PubChemResult` with at least CID/AID/outcome columns.
#' @param cid_col Column name containing compound identifiers.
#' @param aid_col Column name containing assay identifiers.
#' @param outcome_col Column name containing activity outcome values.
#' @param value_map Named numeric mapping for character outcomes.
#' @param strict_outcome Logical. If `TRUE`, unknown outcome labels raise an error.
#' @param unknown_outcome Numeric fallback for unknown labels when `strict_outcome = FALSE`.
#' @param fill Fill value for missing matrix cells.
#' @param prefix Prefix used for assay columns in wide format output.
#' @param aggregate Aggregation method for repeated CID/AID pairs.
#' @param output Output type: `"tibble"` (default dense table) or `"sparse"` (Matrix backend).
#'
#' @return A wide tibble (`output = "tibble"`) or a sparse matrix wrapper
#'   object of class `PubChemSparseActivityMatrix` (`output = "sparse"`).
#' @export
pc_activity_matrix <- function(x,
                               cid_col = "CID",
                               aid_col = "AID",
                               outcome_col = "ActivityOutcome",
                               value_map = c(Active = 1, Inactive = 0, Inconclusive = NA_real_),
                               strict_outcome = FALSE,
                               unknown_outcome = NA_real_,
                               fill = NA_real_,
                               prefix = "AID_",
                               aggregate = c("max", "mean", "first"),
                               output = c("tibble", "sparse")) {
  aggregate <- match.arg(aggregate)
  output <- match.arg(output)
  tbl <- pc_as_tibble_input(x)

  required_cols <- c(cid_col, aid_col, outcome_col)
  missing_cols <- setdiff(required_cols, names(tbl))
  if (length(missing_cols) > 0) {
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  cid <- as.character(tbl[[cid_col]])
  aid <- as.character(tbl[[aid_col]])
  values_raw <- tbl[[outcome_col]]

  if (is.numeric(values_raw)) {
    values <- as.numeric(values_raw)
  } else {
    values <- pc_activity_outcome_map(
      values_raw,
      map = value_map,
      strict = strict_outcome,
      unknown = unknown_outcome
    )
  }

  pair_key <- paste(cid, aid, sep = "\r")
  idx <- split(seq_along(pair_key), pair_key)

  aggregate_fun <- switch(
    aggregate,
    max = function(v) if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE),
    mean = function(v) if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE),
    first = function(v) {
      w <- v[!is.na(v)]
      if (length(w) == 0) NA_real_ else as.numeric(w[[1]])
    }
  )

  agg_values <- vapply(idx, function(i) aggregate_fun(values[i]), numeric(1))
  agg_keys <- strsplit(names(agg_values), "\r", fixed = TRUE)
  agg_cid <- vapply(agg_keys, `[`, character(1), 1)
  agg_aid <- vapply(agg_keys, `[`, character(1), 2)

  cid_levels <- unique(cid)
  aid_levels <- unique(aid)
  row_idx <- match(agg_cid, cid_levels)
  col_idx <- match(agg_aid, aid_levels)
  good <- !is.na(row_idx) & !is.na(col_idx)

  if (identical(output, "sparse")) {
    if (!requireNamespace("Matrix", quietly = TRUE)) {
      stop("Package 'Matrix' is required for output = 'sparse'.")
    }

    if (!isTRUE(is.na(fill)) && !isTRUE(all.equal(fill, 0))) {
      warning(
        "Sparse output has implicit zero fill in Matrix format. ",
        "Original 'fill' is retained in metadata as 'implicit_fill'."
      )
    }

    sparse <- Matrix::sparseMatrix(
      i = as.integer(row_idx[good]),
      j = as.integer(col_idx[good]),
      x = as.numeric(agg_values[good]),
      dims = c(length(cid_levels), length(aid_levels)),
      dimnames = list(cid_levels, paste0(prefix, aid_levels)),
      giveCsparse = TRUE
    )
    out_sparse <- structure(
      list(
        x = sparse,
        row_ids = cid_levels,
        col_ids = aid_levels,
        colnames_prefixed = paste0(prefix, aid_levels),
        implicit_fill = fill,
        outcome_col = outcome_col
      ),
      class = "PubChemSparseActivityMatrix"
    )
    return(out_sparse)
  }

  mat <- matrix(
    fill,
    nrow = length(cid_levels),
    ncol = length(aid_levels),
    dimnames = list(cid_levels, paste0(prefix, aid_levels))
  )
  mat[cbind(row_idx[good], col_idx[good])] <- agg_values[good]

  out <- tibble::as_tibble(
    data.frame(CID = cid_levels, mat, check.names = FALSE, stringsAsFactors = FALSE),
    .name_repair = "minimal"
  )
  class(out) <- unique(c("PubChemTable", class(out)))
  out
}

#' Join Compound, Substance, Assay, and Target Tables
#'
#' @param compounds Base compound table.
#' @param substances Optional substance table.
#' @param assays Optional assay table.
#' @param targets Optional target table.
#' @param by Named list of join keys for each join edge.
#' @param join Join type (`"left"`, `"inner"`, `"full"`).
#'
#' @return A joined tibble suitable for downstream analysis workflows.
#' @export
pc_cross_domain_join <- function(compounds,
                                 substances = NULL,
                                 assays = NULL,
                                 targets = NULL,
                                 by = list(
                                   compound_substance = "CID",
                                   compound_assay = "CID",
                                   assay_target = "AID"
                                 ),
                                 join = c("left", "inner", "full")) {
  if (missing(compounds) || !is.data.frame(compounds)) {
    stop("'compounds' must be a data.frame or tibble.")
  }

  join <- match.arg(join)
  join_fun <- switch(
    join,
    left = dplyr::left_join,
    inner = dplyr::inner_join,
    full = dplyr::full_join
  )

  out <- tibble::as_tibble(compounds)

  if (!is.null(substances)) {
    out <- join_fun(
      out,
      tibble::as_tibble(substances),
      by = by$compound_substance %||% "CID"
    )
  }

  if (!is.null(assays)) {
    out <- join_fun(
      out,
      tibble::as_tibble(assays),
      by = by$compound_assay %||% "CID"
    )
  }

  if (!is.null(targets)) {
    out <- join_fun(
      out,
      tibble::as_tibble(targets),
      by = by$assay_target %||% "AID"
    )
  }

  class(out) <- unique(c("PubChemTable", class(out)))
  out
}

#' Convert Feature Tables to Model Matrix Form
#'
#' @param x Input table or `PubChemResult`.
#' @param outcome Optional outcome column name.
#' @param id_cols Identifier columns excluded from predictors.
#' @param na_fill Optional numeric value used to fill missing predictor values.
#' @param scale Logical; center and scale predictor matrix.
#'
#' @return An object of class `PubChemModelMatrix` with `x`, `y`, and metadata fields.
#' @export
pc_model_matrix <- function(x,
                            outcome = NULL,
                            id_cols = c("CID", "SID", "AID", "Identifier"),
                            na_fill = NULL,
                            scale = FALSE) {
  tbl <- pc_as_tibble_input(x)
  if (!is.null(outcome) && !outcome %in% names(tbl)) {
    stop("Outcome column not found: ", outcome)
  }

  predictor_cols <- setdiff(names(tbl), c(id_cols, outcome))
  if (length(predictor_cols) == 0) {
    stop("No predictor columns available after excluding identifiers and outcome.")
  }

  pred <- tbl[, predictor_cols, drop = FALSE]
  for (nm in names(pred)) {
    if (!is.numeric(pred[[nm]])) {
      suppressWarnings(num <- as.numeric(pred[[nm]]))
      if (sum(!is.na(num)) > 0 || all(is.na(pred[[nm]]))) {
        pred[[nm]] <- num
      }
    }
  }

  is_num <- vapply(pred, is.numeric, logical(1))
  pred <- pred[, is_num, drop = FALSE]
  if (ncol(pred) == 0) {
    stop("No numeric predictor columns could be derived.")
  }

  x_mat <- as.matrix(pred)
  storage.mode(x_mat) <- "double"

  if (!is.null(na_fill)) {
    x_mat[is.na(x_mat)] <- as.numeric(na_fill)
  }

  if (isTRUE(scale)) {
    x_mat <- pc_scale_matrix(x_mat)
  }

  id_present <- intersect(id_cols, names(tbl))
  out <- list(
    x = x_mat,
    y = if (is.null(outcome)) NULL else tbl[[outcome]],
    feature_names = colnames(x_mat),
    ids = if (length(id_present) > 0) tbl[, id_present, drop = FALSE] else NULL
  )

  class(out) <- "PubChemModelMatrix"
  out
}

#' Convert PubChem Tables to rcdk Molecules
#'
#' @param x Input table or `PubChemResult`.
#' @param smiles_col Column with SMILES strings.
#' @param id_col Optional identifier column used to name returned molecules.
#'
#' @return A list of `rcdk` molecule objects.
#' @export
pc_to_rcdk <- function(x, smiles_col = "CanonicalSMILES", id_col = "CID") {
  pkg <- "rcdk"
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package 'rcdk' is required for pc_to_rcdk().")
  }

  tbl <- pc_as_tibble_input(x)
  if (!smiles_col %in% names(tbl)) {
    stop("SMILES column not found: ", smiles_col)
  }

  smiles <- as.character(tbl[[smiles_col]])
  keep <- !is.na(smiles) & nzchar(smiles)
  smiles <- smiles[keep]
  if (length(smiles) == 0) {
    return(list())
  }

  parse_smiles <- getExportedValue(pkg, "parse.smiles")
  mols <- parse_smiles(smiles)

  if (!is.null(id_col) && id_col %in% names(tbl)) {
    ids <- as.character(tbl[[id_col]][keep])
    names(mols) <- ids
  }

  mols
}

#' Convert PubChem Tables to ChemmineR SDF Objects
#'
#' @param x Input table or `PubChemResult`.
#' @param smiles_col Column with SMILES strings.
#'
#' @return A `ChemmineR` SDF object.
#' @export
pc_to_chemminer <- function(x, smiles_col = "CanonicalSMILES") {
  pkg <- "ChemmineR"
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package 'ChemmineR' is required for pc_to_chemminer().")
  }
  pkg_ns <- asNamespace(pkg)
  if (!exists("smiles2sdf", where = pkg_ns, mode = "function")) {
    stop("ChemmineR::smiles2sdf() is not available in this ChemmineR installation.")
  }

  tbl <- pc_as_tibble_input(x)
  if (!smiles_col %in% names(tbl)) {
    stop("SMILES column not found: ", smiles_col)
  }

  smiles <- as.character(tbl[[smiles_col]])
  smiles <- smiles[!is.na(smiles) & nzchar(smiles)]
  if (length(smiles) == 0) {
    stop("No non-empty SMILES values found in '", smiles_col, "'.")
  }

  sdf_fun <- get("smiles2sdf", envir = pkg_ns)
  sdf_fun(smiles)
}

#' Versioning and Deprecation Policy
#'
#' @return A tibble describing PubChemR compatibility and deprecation guarantees.
#' @export
pc_lifecycle_policy <- function() {
  tibble::tibble(
    stream = c("legacy", "nextgen"),
    stability = c("maintenance", "stable"),
    support_window = c("bugfix-only", "minor+patch"),
    deprecation_notice = c(">= 1 minor release", ">= 2 minor releases"),
    breaking_change_window = c("major release only", "major release only")
  )
}

#' @export
print.PubChemModelMatrix <- function(x, ...) {
  cat("\n")
  cat(" PubChemModelMatrix", "\n\n")
  cat("  - Rows: ", nrow(x$x), "\n", sep = "")
  cat("  - Features: ", ncol(x$x), "\n", sep = "")
  cat("  - Outcome: ", ifelse(is.null(x$y), "None", "Provided"), "\n", sep = "")
  invisible(x)
}

#' @export
print.PubChemSparseActivityMatrix <- function(x, ...) {
  cat("\n")
  cat(" PubChemSparseActivityMatrix", "\n\n")
  cat("  - Rows (compounds): ", nrow(x$x), "\n", sep = "")
  cat("  - Columns (assays): ", ncol(x$x), "\n", sep = "")
  cat("  - Non-zero entries: ", length(x$x@x), "\n", sep = "")
  cat("  - Implicit fill: ", ifelse(is.na(x$implicit_fill), "NA", as.character(x$implicit_fill)), "\n", sep = "")
  invisible(x)
}
