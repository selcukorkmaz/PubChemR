pc_drop_null <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

# Namespaces whose identifiers can contain characters (/ and \) that conflict
# with URL path separators and must be sent via POST body instead.
pc_use_post <- function(namespace) {
  !is.null(namespace) &&
    tolower(as.character(namespace)[1]) %in% c("smiles", "inchi", "sdf", "smarts")
}

pc_format_fault <- function(fault) {
  if (is.null(fault)) {
    return("Unknown PubChem fault.")
  }

  if (!is.list(fault)) {
    return(as.character(fault))
  }

  code <- fault[["Code"]]
  message <- fault[["Message"]]
  details <- fault[["Details"]]

  parts <- c(code, message)
  if (!is.null(details)) {
    if (is.character(details)) {
      parts <- c(parts, details)
    } else if (is.list(details)) {
      parts <- c(parts, unlist(details, use.names = FALSE))
    } else {
      parts <- c(parts, as.character(details))
    }
  }

  parts <- parts[!is.na(parts) & nzchar(parts)]
  if (length(parts) == 0) {
    "Unknown PubChem fault."
  } else {
    paste(parts, collapse = " - ")
  }
}

pc_normalize_error <- function(code = "RequestFailed",
                               message = "Unknown error.",
                               details = NULL,
                               error_class = "request_error",
                               status = NULL) {
  err <- list(
    Code = as.character(code),
    Message = as.character(message),
    Details = details,
    ErrorClass = as.character(error_class),
    Status = status
  )

  pc_drop_null(err)
}

pc_error_message <- function(error) {
  if (is.null(error)) {
    return("Unknown error.")
  }

  if (is.character(error)) {
    return(paste(error, collapse = " "))
  }

  if (is.list(error)) {
    msg <- error[["Message"]]
    if (!is.null(msg) && nzchar(as.character(msg))) {
      return(as.character(msg))
    }
    return(paste(unlist(error, use.names = FALSE), collapse = " "))
  }

  as.character(error)
}

pc_make_failed_instance <- function(request_args,
                                    code = "RequestFailed",
                                    message = "Unknown error.",
                                    details = NULL,
                                    error_class = "request_error",
                                    status = NULL) {
  structure(
    list(
      result = list(),
      request_args = pc_drop_null(request_args),
      success = FALSE,
      error = pc_normalize_error(
        code = code,
        message = message,
        details = details,
        error_class = error_class,
        status = status
      )
    ),
    class = "PubChemInstance"
  )
}

pc_collect_instances <- function(identifier,
                                 namespace,
                                 domain,
                                 operation = NULL,
                                 searchtype = NULL,
                                 options = NULL,
                                 result_class = NULL,
                                 drop_class = FALSE,
                                 request_args_extra = list()) {
  if (is.null(identifier) || length(identifier) == 0) {
    stop("'identifier' cannot be NULL or empty.")
  }

  # Check if batching is possible
  batchable <- tolower(namespace) %in% c("cid", "sid", "aid") &&
    is.null(searchtype) &&
    length(identifier) > 1 &&
    (is.null(operation) || grepl("^property/", operation) || identical(operation, "property"))

  if (batchable) {
    collected <- pc_collect_instances_batched(
      identifier = identifier,
      namespace = namespace,
      domain = domain,
      operation = operation,
      searchtype = searchtype,
      options = options,
      result_class = result_class,
      drop_class = drop_class
    )
  } else {
    collected <- pc_collect_instances_sequential(
      identifier = identifier,
      namespace = namespace,
      domain = domain,
      operation = operation,
      searchtype = searchtype,
      options = options,
      result_class = result_class,
      drop_class = drop_class
    )
  }

  req <- list(
    namespace = namespace,
    identifier = identifier,
    domain = domain,
    operation = operation,
    searchtype = searchtype,
    options = options
  )

  if (length(request_args_extra) > 0) {
    for (nm in names(request_args_extra)) {
      req[[nm]] <- request_args_extra[[nm]]
    }
  }

  list(
    result = collected$results,
    request_args = pc_drop_null(req),
    success = collected$success,
    error = collected$errors
  )
}

#' Sequential per-identifier collection (original behavior)
#' @noRd
pc_collect_instances_sequential <- function(identifier, namespace, domain,
                                            operation = NULL, searchtype = NULL,
                                            options = NULL, result_class = NULL,
                                            drop_class = FALSE) {
  results <- vector("list", length(identifier))
  success <- rep(FALSE, length(identifier))
  errors <- rep("", length(identifier))

  for (i in seq_along(identifier)) {
    id_i <- identifier[[i]]

    req_args <- list(
      namespace = namespace,
      identifier = id_i,
      domain = domain,
      operation = operation,
      searchtype = searchtype,
      options = options
    )

    instance <- tryCatch(
      get_json(
        identifier = id_i,
        namespace = namespace,
        domain = domain,
        operation = operation,
        searchtype = searchtype,
        options = options
      ),
      error = function(e) {
        pc_make_failed_instance(
          request_args = req_args,
          code = "ClientError",
          message = conditionMessage(e),
          error_class = "client_error"
        )
      }
    )

    if (isTRUE(instance$success) && !is.null(result_class)) {
      class(instance) <- unique(c(class(instance), result_class))
    }

    if (drop_class) {
      class(instance) <- NULL
    }

    results[[i]] <- instance
    success[i] <- isTRUE(instance$success)

    if (!success[i]) {
      errors[i] <- pc_error_message(instance$error)
    }
  }

  list(results = results, success = success, errors = errors)
}

#' Batched collection: group identifiers into chunks and decompose responses
#' @noRd
pc_collect_instances_batched <- function(identifier, namespace, domain,
                                         operation = NULL, searchtype = NULL,
                                         options = NULL, result_class = NULL,
                                         drop_class = FALSE,
                                         chunk_size = 100) {
  n <- length(identifier)
  results <- vector("list", n)
  success <- rep(FALSE, n)
  errors <- rep("", n)

  # Map original identifiers to numeric for matching
  id_nums <- as.numeric(identifier)

  # Split into chunks
  chunks <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))

  for (chunk_idx in chunks) {
    chunk_ids <- identifier[chunk_idx]

    batch_instance <- tryCatch(
      get_json(
        identifier = chunk_ids,
        namespace = namespace,
        domain = domain,
        operation = operation,
        searchtype = searchtype,
        options = options
      ),
      error = function(e) {
        pc_make_failed_instance(
          request_args = list(namespace = namespace, identifier = chunk_ids,
                              domain = domain, operation = operation),
          code = "ClientError",
          message = conditionMessage(e),
          error_class = "client_error"
        )
      }
    )

    if (!isTRUE(batch_instance$success)) {
      # Batch failed — fall back to per-identifier for this chunk
      fallback <- pc_collect_instances_sequential(
        identifier = chunk_ids,
        namespace = namespace,
        domain = domain,
        operation = operation,
        searchtype = searchtype,
        options = options,
        result_class = result_class,
        drop_class = drop_class
      )
      for (j in seq_along(chunk_idx)) {
        results[[chunk_idx[j]]] <- fallback$results[[j]]
        success[chunk_idx[j]] <- fallback$success[j]
        errors[chunk_idx[j]] <- fallback$errors[j]
      }
      next
    }

    # Decompose batch response into per-ID instances
    decomposed <- pc_decompose_batch(batch_instance$result, id_nums[chunk_idx], namespace)

    if (is.null(decomposed)) {
      # Unknown structure — fall back to per-identifier
      fallback <- pc_collect_instances_sequential(
        identifier = chunk_ids,
        namespace = namespace,
        domain = domain,
        operation = operation,
        searchtype = searchtype,
        options = options,
        result_class = result_class,
        drop_class = drop_class
      )
      for (j in seq_along(chunk_idx)) {
        results[[chunk_idx[j]]] <- fallback$results[[j]]
        success[chunk_idx[j]] <- fallback$success[j]
        errors[chunk_idx[j]] <- fallback$errors[j]
      }
      next
    }

    # Build per-ID instances from decomposed data
    for (j in seq_along(chunk_idx)) {
      pos <- chunk_idx[j]
      id_i <- identifier[pos]
      id_data <- decomposed[[as.character(id_nums[pos])]]

      if (is.null(id_data)) {
        results[[pos]] <- pc_make_failed_instance(
          request_args = list(namespace = namespace, identifier = id_i,
                              domain = domain, operation = operation),
          code = "NotFound",
          message = paste0("No data returned for identifier '", id_i, "' in batch response."),
          error_class = "pubchem_fault"
        )
        errors[pos] <- paste0("No data for identifier '", id_i, "'")
      } else {
        instance <- structure(
          list(
            result = id_data,
            request_args = pc_drop_null(list(
              namespace = namespace,
              identifier = id_i,
              domain = domain,
              operation = operation,
              searchtype = searchtype,
              options = options
            )),
            success = TRUE,
            error = NULL
          ),
          class = "PubChemInstance"
        )

        if (!is.null(result_class)) {
          class(instance) <- unique(c(class(instance), result_class))
        }
        if (drop_class) {
          class(instance) <- NULL
        }

        results[[pos]] <- instance
        success[pos] <- TRUE
      }
    }
  }

  list(results = results, success = success, errors = errors)
}

#' Decompose a batch PubChem response into per-ID data
#' @param result The parsed JSON result list from a batch request
#' @param id_nums Numeric vector of IDs requested
#' @param namespace The namespace (cid, sid, or aid)
#' @return Named list (keyed by ID) of per-ID result data, or NULL if structure is unknown
#' @noRd
pc_decompose_batch <- function(result, id_nums, namespace) {
  id_key <- switch(tolower(namespace),
    cid = "CID",
    sid = "SID",
    aid = "AID",
    return(NULL)
  )

  # Case 1: PC_Compounds (full compound records)
  compounds <- result[["PC_Compounds"]]
  if (!is.null(compounds) && is.list(compounds)) {
    out <- list()
    for (comp in compounds) {
      cid <- tryCatch(comp$id$id$cid, error = function(e) NULL)
      if (!is.null(cid)) {
        out[[as.character(cid)]] <- list(PC_Compounds = list(comp))
      }
    }
    if (length(out) > 0) return(out)
  }

  # Case 2: PropertyTable$Properties
  props <- result[["PropertyTable"]][["Properties"]]
  if (!is.null(props) && is.list(props)) {
    out <- list()
    for (prop in props) {
      id_val <- prop[[id_key]]
      if (!is.null(id_val)) {
        key <- as.character(id_val)
        out[[key]] <- list(PropertyTable = list(Properties = list(prop)))
      }
    }
    if (length(out) > 0) return(out)
  }

  # Case 3: InformationList$Information
  info_list <- result[["InformationList"]][["Information"]]
  if (!is.null(info_list) && is.list(info_list)) {
    out <- list()
    for (info in info_list) {
      id_val <- info[[id_key]]
      if (!is.null(id_val)) {
        key <- as.character(id_val)
        if (is.null(out[[key]])) {
          out[[key]] <- list(InformationList = list(Information = list(info)))
        } else {
          out[[key]]$InformationList$Information <- c(
            out[[key]]$InformationList$Information, list(info)
          )
        }
      }
    }
    if (length(out) > 0) return(out)
  }

  # Unknown structure
  NULL
}
