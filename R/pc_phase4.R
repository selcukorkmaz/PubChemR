#' Convert PubChem Assay Summary to Long Activity Table
#'
#' @description
#' Builds a normalized long-form table from the PubChem
#' `compound/*/assaysummary/JSON` payload, suitable for
#' `pc_activity_matrix()`.
#'
#' @param identifier Identifier vector used when `x` is `NULL`.
#' @param namespace Namespace for `identifier` when requesting from PubChem.
#' @param x Optional source object. One of:
#'   - `PubChemResult` from `pc_request(...)`
#'   - raw parsed payload list containing `Table/Columns/Row`
#' @param chunk_size Optional chunk size for large identifier vectors.
#' @param unique_rows Logical. Remove duplicate rows after normalization.
#' @param add_outcome_value Logical. If `TRUE`, adds a numeric
#'   `ActivityOutcomeValue` column when `ActivityOutcome` exists.
#' @param outcome_map Optional named mapping passed to
#'   `pc_activity_outcome_map()`.
#' @param strict_outcome Logical. If `TRUE`, unknown outcome labels error.
#' @param unknown_outcome Numeric fallback for unknown labels when
#'   `strict_outcome = FALSE`.
#' @param ... Additional arguments forwarded to `pc_request()` when `x` is `NULL`.
#'
#' @details
#' Input can be fetched directly from PubChem or provided as an already parsed
#' payload. Column names are normalized to stable identifiers and optional
#' numeric activity outcomes can be added for modeling workflows.
#'
#' @return A tibble with normalized fields including `CID`, `AID`,
#'   and `ActivityOutcome` when available.
#'
#' @examples
#' payload <- list(
#'   Table = list(
#'     Columns = list(Column = c("CID", "AID", "Activity Outcome")),
#'     Row = list(
#'       list(Cell = c("2244", "367", "Active")),
#'       list(Cell = c("2244", "368", "Inactive"))
#'     )
#'   )
#' )
#' pc_assay_activity_long(x = payload)
#' @export
pc_assay_activity_long <- function(identifier = NULL,
                                   namespace = "cid",
                                   x = NULL,
                                   chunk_size = NULL,
                                   unique_rows = TRUE,
                                   add_outcome_value = TRUE,
                                   outcome_map = NULL,
                                   strict_outcome = FALSE,
                                   unknown_outcome = NA_real_,
                                   ...) {
  if (is.null(x)) {
    if (is.null(identifier) || length(identifier) == 0) {
      stop("Provide either 'x' or non-empty 'identifier'.")
    }

    ids <- as.character(identifier)
    if (!is.null(chunk_size)) {
      chunk_size <- pc_validate_positive_integer_scalar(chunk_size, "'chunk_size'")
    }

    if (!is.null(chunk_size) && length(ids) > chunk_size) {
      b <- pc_batch(
        ids = ids,
        fn = function(chunk_ids, ...) {
          pc_request(
            domain = "compound",
            namespace = namespace,
            identifier = chunk_ids,
            operation = "assaysummary",
            output = "JSON",
            ...
          )
        },
        chunk_size = chunk_size,
        ...
      )

      success_flags <- as.logical(b$success)
      success_flags[is.na(success_flags)] <- FALSE
      failed_idx <- which(!success_flags)
      if (length(failed_idx) > 0) {
        failed_msgs <- vapply(failed_idx, function(i) {
          msg <- b$error[[i]] %||% ""
          if (!nzchar(msg) && inherits(b$results[[i]], "PubChemResult")) {
            msg <- b$results[[i]]$error$message %||% ""
          }
          if (!nzchar(msg)) {
            msg <- "Unknown error"
          }
          paste0("chunk ", i, ": ", msg)
        }, character(1))

        extra <- ""
        if (length(failed_msgs) > 3) {
          extra <- paste0(" (showing first 3 of ", length(failed_msgs), " failures)")
        }

        stop(
          "Assay summary chunked request failed for ",
          length(failed_idx),
          " of ",
          length(b$chunks),
          " chunk(s): ",
          paste(utils::head(failed_msgs, 3), collapse = " | "),
          extra
        )
      }

      parts <- lapply(b$results, function(res) {
        if (inherits(res, "PubChemResult") && isTRUE(res$success)) {
          return(.pc_parse_assaysummary_payload(res$data))
        }
        tibble::tibble()
      })

      out <- dplyr::bind_rows(parts)
    } else {
      res <- pc_request(
        domain = "compound",
        namespace = namespace,
        identifier = ids,
        operation = "assaysummary",
        output = "JSON",
        ...
      )

      if (!isTRUE(res$success)) {
        stop("Assay summary request failed: ", res$error$message %||% "Unknown error")
      }
      out <- .pc_parse_assaysummary_payload(res$data)
    }
  } else {
    payload <- if (inherits(x, "PubChemResult")) {
      if (!isTRUE(x$success)) {
        stop("Assay summary payload is unsuccessful: ", x$error$message %||% "Unknown error")
      }
      x$data
    } else if (is.list(x)) {
      x
    } else {
      stop("'x' must be a PubChemResult or parsed payload list.")
    }

    out <- .pc_parse_assaysummary_payload(payload)
  }

  if (isTRUE(unique_rows) && nrow(out) > 0) {
    out <- dplyr::distinct(out)
  }

  if (isTRUE(add_outcome_value) && "ActivityOutcome" %in% names(out)) {
    out$ActivityOutcomeValue <- pc_activity_outcome_map(
      out$ActivityOutcome,
      map = outcome_map,
      strict = strict_outcome,
      unknown = unknown_outcome
    )
  }

  class(out) <- unique(c("PubChemTable", class(out)))
  out
}

.pc_clean_colname <- function(x) {
  y <- gsub("[^A-Za-z0-9]+", "_", x)
  y <- gsub("(^_+|_+$)", "", y)
  y <- gsub("_+", "_", y)
  y
}

.pc_parse_assaysummary_payload <- function(payload) {
  if (is.null(payload) || is.null(payload$Table)) {
    return(tibble::tibble())
  }

  cols <- payload$Table$Columns$Column
  rows <- payload$Table$Row

  if (is.null(cols) || is.null(rows) || length(cols) == 0 || length(rows) == 0) {
    return(tibble::tibble())
  }

  cols <- as.character(cols)
  n_cols <- length(cols)

  cell_rows <- lapply(rows, function(r) {
    cells <- r$Cell
    if (is.null(cells)) {
      cells <- character(0)
    }
    cells <- as.character(cells)
    if (length(cells) < n_cols) {
      cells <- c(cells, rep("", n_cols - length(cells)))
    } else if (length(cells) > n_cols) {
      cells <- cells[seq_len(n_cols)]
    }
    cells
  })

  mat <- do.call(rbind, cell_rows)
  df <- as.data.frame(mat, stringsAsFactors = FALSE)
  names(df) <- cols

  # Preserve canonical activity-matrix columns, sanitize the rest.
  nms <- names(df)
  std <- character(length(nms))
  std[] <- nms
  std[tolower(nms) == "activity outcome"] <- "ActivityOutcome"
  std[tolower(nms) == "activity value [um]"] <- "ActivityValue_uM"
  std[tolower(nms) == "aid"] <- "AID"
  std[tolower(nms) == "cid"] <- "CID"
  std[tolower(nms) == "sid"] <- "SID"

  keep_exact <- c("ActivityOutcome", "ActivityValue_uM", "AID", "CID", "SID")
  cleaned <- vapply(std, .pc_clean_colname, character(1))
  cleaned[std %in% keep_exact] <- std[std %in% keep_exact]
  names(df) <- make.unique(cleaned)

  for (id_col in c("CID", "AID", "SID")) {
    if (id_col %in% names(df)) {
      suppressWarnings({
        num <- as.integer(df[[id_col]])
      })
      if (sum(!is.na(num)) > 0) {
        df[[id_col]] <- as.character(num)
      } else {
        df[[id_col]] <- as.character(df[[id_col]])
      }
    }
  }

  if ("ActivityOutcome" %in% names(df)) {
    df$ActivityOutcome <- as.character(df$ActivityOutcome)
  }
  if ("ActivityValue_uM" %in% names(df)) {
    suppressWarnings({
      val <- as.numeric(df$ActivityValue_uM)
    })
    # Keep a stable schema across chunked requests.
    df$ActivityValue_uM <- val
  }

  tibble::as_tibble(df)
}

#' Export Model-Ready Data
#'
#' @description
#' Writes model-ready data to disk from either a `PubChemModelMatrix`
#' object or a tabular object.
#'
#' @param x Input object. Supported:
#'   - `PubChemModelMatrix`
#'   - `data.frame`/`tibble`
#' @param path Output path.
#' @param format Export format, one of `"csv"` or `"rds"`.
#' @param include_ids Logical. Include ID columns from `PubChemModelMatrix`.
#' @param include_outcome Logical. Include outcome vector from `PubChemModelMatrix`.
#'
#' @details
#' Output format is selected by `format`; `csv` is written with
#' `utils::write.csv()` and `rds` with `saveRDS()`.
#'
#' @return Invisibly returns a list with `path`, `format`, and output dimensions.
#'
#' @examples
#' out_file <- tempfile(fileext = ".csv")
#' x <- tibble::tibble(CID = c("1", "2"), x1 = c(0.1, 0.2))
#' meta <- pc_export_model_data(x, path = out_file, format = "csv")
#' file.exists(meta$path)
#' @export
pc_export_model_data <- function(x,
                                 path,
                                 format = c("csv", "rds"),
                                 include_ids = TRUE,
                                 include_outcome = TRUE) {
  format <- match.arg(format)
  if (missing(path) || !is.character(path) || length(path) != 1 || !nzchar(path)) {
    stop("'path' must be a non-empty file path.")
  }

  out_tbl <- NULL
  if (inherits(x, "PubChemModelMatrix")) {
    out_tbl <- as.data.frame(x$x, stringsAsFactors = FALSE)
    if (isTRUE(include_ids) && !is.null(x$ids)) {
      out_tbl <- dplyr::bind_cols(x$ids, out_tbl)
    }
    if (isTRUE(include_outcome) && !is.null(x$y)) {
      out_tbl$.outcome <- x$y
    }
  } else if (is.data.frame(x)) {
    out_tbl <- as.data.frame(x, stringsAsFactors = FALSE)
  } else {
    stop("'x' must be a PubChemModelMatrix or data.frame/tibble.")
  }

  if (format == "csv") {
    utils::write.csv(out_tbl, file = path, row.names = FALSE)
  } else {
    saveRDS(out_tbl, file = path)
  }

  invisible(list(
    path = normalizePath(path, winslash = "/", mustWork = FALSE),
    format = format,
    rows = nrow(out_tbl),
    cols = ncol(out_tbl)
  ))
}

#' Retrieve Full Biological Test Results from PubChem SDQ
#'
#' @description
#' Queries the PubChem SDQ (Structured Data Query) agent to retrieve the full
#' biological test results table for a compound. Uses the \code{download} query
#' mode to return all available columns for each record. The number and names
#' of columns vary by compound depending on available data (e.g. baid, aid,
#' sid, cid, activityid, aidtypeid, aidname, targetname, cmpdname, acvalue,
#' geneid, etc.).
#'
#' @param identifier A single compound identifier (CID, name, or InChIKey
#'   depending on \code{namespace}).
#' @param namespace Character. The namespace for \code{identifier}. Default
#'   \code{"cid"}. If not \code{"cid"}, the identifier is first resolved to a
#'   CID via \code{\link{pc_request}}.
#' @param collection Character. SDQ collection to query. Default
#'   \code{"bioactivity"}.
#' @param limit Integer. Maximum number of rows to return. Default
#'   \code{10000000L}.
#' @param order Character. Column and direction for sorting results. Default
#'   \code{"activity,asc"}.
#' @param rate_limit Logical or numeric. If \code{TRUE} (default), applies
#'   the global rate limit from \code{\link{pc_config}}. If numeric, uses that
#'   value as requests per second.
#' @param cache Logical. If \code{TRUE}, cache results to disk. Default
#'   \code{FALSE}.
#' @param cache_dir Character. Directory for disk cache. Defaults to the value
#'   from \code{\link{pc_config}}.
#' @param cache_ttl Numeric. Cache time-to-live in seconds. Defaults to the
#'   value from \code{\link{pc_config}}.
#' @param force_refresh Logical. If \code{TRUE}, bypass any cached result.
#'   Default \code{FALSE}.
#'
#' @details
#' When \code{namespace != "cid"}, the identifier is first resolved to CID via
#' \code{\link{pc_request}} before querying SDQ. Returned columns depend on
#' source availability for the requested compound.
#'
#' @return A tibble of class \code{PubChemTable} containing the full
#'   bioactivity results.
#'
#' @examples
#' names(formals(pc_sdq_bioactivity))
#'
#' \dontrun{
#'   # Retrieve bioactivity data for aspirin (CID 2244)
#'   bio <- pc_sdq_bioactivity(2244)
#'   head(bio)
#' }
#'
#' @export
pc_sdq_bioactivity <- function(identifier,
                               namespace = "cid",
                               collection = "bioactivity",
                               limit = 10000000L,
                               order = "activity,asc",
                               rate_limit = TRUE,
                               cache = FALSE,
                               cache_dir = NULL,
                               cache_ttl = NULL,
                               force_refresh = FALSE) {
  if (missing(identifier) || is.null(identifier) || length(identifier) == 0) {
    stop("'identifier' is required.")
  }

  cache_dir <- cache_dir %||% .pc_state$config$cache_dir
  cache_ttl <- cache_ttl %||% .pc_state$config$cache_ttl

  # Resolve to CIDs if namespace is not "cid"
  if (tolower(namespace) != "cid") {
    res <- pc_request(
      domain = "compound",
      namespace = namespace,
      identifier = as.character(identifier),
      operation = "cids",
      output = "JSON",
      rate_limit = rate_limit
    )
    if (!isTRUE(res$success)) {
      stop("Failed to resolve identifier to CID: ",
           res$error$message %||% "Unknown error")
    }
    cids <- as.character(unlist(res$data$IdentifierList$CID))
    if (length(cids) == 0) {
      stop("No CIDs found for identifier '", identifier, "'.")
    }
  } else {
    cids <- as.character(identifier)
  }

  sdq_base <- "https://pubchem.ncbi.nlm.nih.gov/sdq/sdqagent.cgi"

  all_results <- lapply(cids, function(cid) {
    query_list <- list(
      download = "*",
      collection = collection,
      where = list(ands = list(list(cid = cid))),
      order = list(order),
      start = 1L,
      limit = as.integer(limit)
    )
    query_json <- RJSONIO::toJSON(query_list)
    url <- paste0(sdq_base, "?infmt=json&outfmt=json&query=",
                  utils::URLencode(query_json, reserved = TRUE))

    # Check cache
    if (isTRUE(cache) && !isTRUE(force_refresh)) {
      cache_key <- pc_cache_key(method = "GET", url = url)
      cached <- pc_cache_get(cache_key, cache_dir = cache_dir, ttl = cache_ttl)
      if (!is.null(cached)) {
        return(cached)
      }
    }

    # Rate limiting
    pc_throttle(rate_limit)

    resp <- httr::GET(url, httr::user_agent(
      .pc_state$config$user_agent
    ))

    if (httr::status_code(resp) != 200L) {
      stop("SDQ request failed with HTTP status ",
           httr::status_code(resp), " for CID ", cid, ".")
    }

    text <- httr::content(resp, as = "text", encoding = "UTF-8")
    parsed <- tryCatch(RJSONIO::fromJSON(text), error = function(e) NULL)

    if (is.null(parsed)) {
      stop("Failed to parse SDQ response for CID ", cid, ".")
    }

    # download mode returns a raw JSON array on success,
    # or an SDQOutputSet wrapper on error
    if (!is.null(names(parsed)) && "SDQOutputSet" %in% names(parsed)) {
      output_set <- parsed$SDQOutputSet[[1]]
      sdq_status <- output_set[["status"]]
      err_code <- sdq_status[["code"]]
      err_msg <- sdq_status[["error"]] %||% "Unknown SDQ error"
      stop("SDQ query failed for CID ", cid, " (code ", err_code, "): ", err_msg)
    }

    # parsed is a list of named vectors (one per row)
    if (length(parsed) == 0) {
      tbl <- tibble::tibble()
    } else {
      tbl <- dplyr::bind_rows(lapply(parsed, function(r) {
        tibble::as_tibble(as.list(r))
      }))
    }

    # Cache the result
    if (isTRUE(cache)) {
      cache_key <- pc_cache_key(method = "GET", url = url)
      pc_cache_set(cache_key, tbl, cache_dir = cache_dir)
    }

    tbl
  })

  out <- dplyr::bind_rows(all_results)

  # Coerce ID columns to character for consistency
  id_cols <- intersect(c("cid", "aid", "sid", "baid"), names(out))
  for (col in id_cols) {
    out[[col]] <- as.character(out[[col]])
  }

  class(out) <- unique(c("PubChemTable", class(out)))
  out
}
