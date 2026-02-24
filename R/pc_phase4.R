#' Convert PubChem Assay Summary to Long Activity Table
#'
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
#' @return A tibble with normalized fields including `CID`, `AID`,
#'   and `ActivityOutcome` when available.
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
    if (sum(!is.na(val)) > 0) {
      df$ActivityValue_uM <- val
    }
  }

  tibble::as_tibble(df)
}

#' Export Model-Ready Data
#'
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
#' @return Invisibly returns a list with `path`, `format`, and output dimensions.
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
