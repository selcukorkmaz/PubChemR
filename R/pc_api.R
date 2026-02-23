# Internal state for Phase 2 API
.pc_state <- local({
  e <- new.env(parent = emptyenv())
  e$last_request_time <- 0
  e$cache_mem <- new.env(parent = emptyenv())
  e$config <- list(
    rate_limit = 5,
    timeout = 60,
    retries = 3,
    pause_base = 1,
    pause_cap = 8,
    user_agent = paste0("PubChemR/", as.character(utils::packageVersion("PubChemR"))),
    cache_dir = file.path(tempdir(), "PubChemR_cache"),
    cache_ttl = 24 * 60 * 60,
    offline = FALSE
  )
  e
})

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

pc_hash_text <- function(x) {
  tf <- tempfile(fileext = ".txt")
  on.exit(unlink(tf), add = TRUE)
  writeBin(charToRaw(enc2utf8(x)), tf)
  unname(tools::md5sum(tf))
}

pc_stable_string <- function(x) {
  paste(utils::capture.output(dput(x)), collapse = "\n")
}

pc_build_url <- function(domain = "compound",
                         namespace = "cid",
                         identifier = NULL,
                         operation = NULL,
                         output = "JSON",
                         searchtype = NULL,
                         options = NULL) {
  if (is.null(domain)) {
    stop("'domain' cannot be NULL.")
  }

  if (!is.null(operation) && length(operation) > 1) {
    operation <- paste(operation, collapse = "/")
  }

  id_part <- NULL
  if (!is.null(identifier)) {
    ids <- as.character(identifier)
    ids <- URLencode(ids, reserved = TRUE)
    id_part <- paste(ids, collapse = ",")
  }

  comps <- Filter(
    Negate(is.null),
    list("https://pubchem.ncbi.nlm.nih.gov/rest/pug", domain, searchtype, namespace, id_part, operation, output)
  )
  url <- paste(comps, collapse = "/")

  if (!is.null(options)) {
    if (!is.list(options) || is.null(names(options)) || any(names(options) == "")) {
      stop("'options' must be a named list.")
    }
    query <- paste(
      paste0(
        URLencode(as.character(names(options)), reserved = TRUE),
        "=",
        URLencode(as.character(unlist(options, use.names = FALSE)), reserved = TRUE)
      ),
      collapse = "&"
    )
    url <- paste0(url, "?", query)
  }

  url
}

pc_throttle <- function(rate_limit = TRUE) {
  rl <- rate_limit
  if (isTRUE(rate_limit)) {
    rl <- .pc_state$config$rate_limit
  }

  if (isFALSE(rate_limit) || is.null(rl) || !is.numeric(rl) || rl <= 0) {
    return(invisible(NULL))
  }

  min_interval <- 1 / rl
  now <- as.numeric(Sys.time())
  elapsed <- now - .pc_state$last_request_time
  wait_time <- min_interval - elapsed
  if (wait_time > 0) {
    Sys.sleep(wait_time)
  }
  .pc_state$last_request_time <- as.numeric(Sys.time())
  invisible(NULL)
}

pc_cache_key <- function(method, url, body = NULL) {
  key_data <- list(method = toupper(method), url = url, body = body)
  pc_hash_text(pc_stable_string(key_data))
}

pc_cache_path <- function(cache_dir, key) {
  file.path(cache_dir, paste0(key, ".rds"))
}

pc_cache_get <- function(key, cache_dir, ttl) {
  if (exists(key, envir = .pc_state$cache_mem, inherits = FALSE)) {
    item <- get(key, envir = .pc_state$cache_mem, inherits = FALSE)
    if ((as.numeric(Sys.time()) - item$timestamp) <= ttl) {
      return(item$value)
    }
    rm(list = key, envir = .pc_state$cache_mem)
  }

  f <- pc_cache_path(cache_dir, key)
  if (!file.exists(f)) {
    return(NULL)
  }

  item <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(item)) {
    return(NULL)
  }

  if ((as.numeric(Sys.time()) - item$timestamp) > ttl) {
    unlink(f, force = TRUE)
    return(NULL)
  }

  assign(key, item, envir = .pc_state$cache_mem)
  item$value
}

pc_cache_set <- function(key, value, cache_dir) {
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  item <- list(timestamp = as.numeric(Sys.time()), value = value)
  assign(key, item, envir = .pc_state$cache_mem)
  saveRDS(item, pc_cache_path(cache_dir, key))
  invisible(value)
}

#' Configure PubChemR Next-Gen API Defaults
#'
#' @param ... Named configuration values to update.
#'
#' @return A named list of active configuration values.
#' @export
pc_config <- function(...) {
  dots <- list(...)
  if (length(dots) == 0) {
    return(.pc_state$config)
  }

  valid <- names(.pc_state$config)
  unknown <- setdiff(names(dots), valid)
  if (length(unknown) > 0) {
    stop("Unknown config key(s): ", paste(unknown, collapse = ", "))
  }

  for (nm in names(dots)) {
    .pc_state$config[[nm]] <- dots[[nm]]
  }

  .pc_state$config
}

#' Clear PubChemR Request Cache
#'
#' @param cache_dir Cache directory. If `NULL`, uses configured cache directory.
#' @param memory Logical; clear in-memory cache.
#' @param disk Logical; clear on-disk cache.
#'
#' @return Invisibly returns `TRUE`.
#' @export
pc_cache_clear <- function(cache_dir = NULL, memory = TRUE, disk = TRUE) {
  cache_dir <- cache_dir %||% .pc_state$config$cache_dir

  if (isTRUE(memory)) {
    rm(list = ls(envir = .pc_state$cache_mem, all.names = TRUE), envir = .pc_state$cache_mem)
  }

  if (isTRUE(disk) && dir.exists(cache_dir)) {
    files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
    if (length(files) > 0) {
      unlink(files, force = TRUE)
    }
  }

  invisible(TRUE)
}

#' Cache Diagnostics for PubChemR
#'
#' @param cache_dir Cache directory. If `NULL`, uses configured cache directory.
#'
#' @return A one-row tibble with memory and disk cache diagnostics.
#' @export
pc_cache_info <- function(cache_dir = NULL) {
  cache_dir <- cache_dir %||% .pc_state$config$cache_dir

  mem_keys <- ls(envir = .pc_state$cache_mem, all.names = TRUE)
  mem_n <- length(mem_keys)

  disk_files <- if (dir.exists(cache_dir)) {
    list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  } else {
    character(0)
  }
  disk_n <- length(disk_files)
  disk_size <- if (disk_n > 0) sum(file.info(disk_files)$size, na.rm = TRUE) else 0

  tibble::tibble(
    memory_entries = mem_n,
    disk_entries = disk_n,
    disk_size_bytes = disk_size,
    cache_dir = cache_dir
  )
}

pc_make_error <- function(code, message, status = NULL, details = NULL) {
  structure(
    list(
      code = as.character(code),
      message = as.character(message),
      status = status,
      details = details
    ),
    class = "PubChemError"
  )
}

pc_make_result <- function(success,
                           request,
                           data = NULL,
                           error = NULL,
                           status = NULL,
                           raw = NULL,
                           headers = NULL,
                           from_cache = FALSE,
                           pending = FALSE,
                           listkey = NULL) {
  structure(
    list(
      success = isTRUE(success),
      request = request,
      data = data,
      error = error,
      status = status,
      raw = raw,
      headers = headers,
      from_cache = isTRUE(from_cache),
      pending = isTRUE(pending),
      listkey = listkey,
      timestamp = Sys.time()
    ),
    class = "PubChemResult"
  )
}

pc_fault_get <- function(fault, field) {
  if (is.null(fault)) {
    return(NULL)
  }
  if (is.list(fault)) {
    return(fault[[field]])
  }
  if (!is.null(names(fault)) && field %in% names(fault)) {
    return(unname(fault[[field]]))
  }
  NULL
}

pc_parse_text_payload <- function(text) {
  parsed <- tryCatch(fromJSON(text), error = function(e) NULL)
  if (is.null(parsed)) {
    return(list(data = text, fault = NULL, pending = FALSE, listkey = NULL))
  }

  fault <- parsed$Fault
  pending <- !is.null(parsed$Waiting) && !is.null(parsed$Waiting$ListKey)
  listkey <- if (pending) parsed$Waiting$ListKey else NULL
  list(data = parsed, fault = fault, pending = pending, listkey = listkey)
}

#' Normalize an HTTP Response Into a Typed PubChem Result
#'
#' @param response A `httr` response object or raw text.
#' @param request Request metadata list.
#'
#' @return An object of class `PubChemResult`.
#' @export
pc_response <- function(response, request = list()) {
  if (inherits(response, "response")) {
    status <- httr::status_code(response)
    text <- httr::content(response, as = "text", encoding = "UTF-8")
    headers <- response$headers
  } else {
    status <- NA_integer_
    text <- as.character(response)
    headers <- NULL
  }

  parsed <- pc_parse_text_payload(text)

  if (!is.null(parsed$fault)) {
    fault_code <- pc_fault_get(parsed$fault, "Code") %||% "PubChemFault"
    fault_details <- pc_fault_get(parsed$fault, "Details")
    err <- pc_make_error(
      code = fault_code,
      message = pc_format_fault(parsed$fault),
      status = status,
      details = fault_details
    )
    return(pc_make_result(
      success = FALSE,
      request = request,
      data = parsed$data,
      error = err,
      status = status,
      raw = text,
      headers = headers
    ))
  }

  ok_status <- is.na(status) || (status >= 200 && status < 300)
  if (!ok_status) {
    err <- pc_make_error(
      code = "HTTPError",
      message = paste0("HTTP ", status, " request failed."),
      status = status,
      details = text
    )
    return(pc_make_result(
      success = FALSE,
      request = request,
      data = parsed$data,
      error = err,
      status = status,
      raw = text,
      headers = headers
    ))
  }

  pc_make_result(
    success = TRUE,
    request = request,
    data = parsed$data,
    error = NULL,
    status = status,
    raw = text,
    headers = headers,
    pending = parsed$pending,
    listkey = parsed$listkey
  )
}

#' Unified Transport Layer for PubChem Requests
#'
#' @param domain PubChem domain.
#' @param namespace PubChem namespace.
#' @param identifier Identifier(s).
#' @param operation Operation.
#' @param searchtype Search type.
#' @param output Output format.
#' @param options Named list of query options.
#' @param method HTTP method; `"GET"` or `"POST"`.
#' @param body Optional POST body.
#' @param rate_limit `TRUE` to use configured default, `FALSE` to disable, or numeric req/sec.
#' @param timeout Timeout in seconds.
#' @param retries Retry count.
#' @param pause_base Retry base pause.
#' @param pause_cap Retry max pause.
#' @param user_agent User-agent string.
#' @param cache Logical; enable memory+disk cache.
#' @param cache_dir Cache directory.
#' @param cache_ttl TTL in seconds.
#' @param force_refresh Skip cache and refresh.
#' @param offline `TRUE` to use cache-only replay mode (no network calls).
#' @param ... Additional arguments forwarded to `httr::RETRY`.
#'
#' @return An object of class `PubChemResult`.
#' @export
pc_request <- function(domain = "compound",
                       namespace = "cid",
                       identifier = NULL,
                       operation = NULL,
                       searchtype = NULL,
                       output = "JSON",
                       options = NULL,
                       method = c("GET", "POST"),
                       body = NULL,
                       rate_limit = TRUE,
                       timeout = NULL,
                       retries = NULL,
                       pause_base = NULL,
                       pause_cap = NULL,
                       user_agent = NULL,
                       cache = FALSE,
                       cache_dir = NULL,
                       cache_ttl = NULL,
                       force_refresh = FALSE,
                       offline = NULL,
                       ...) {
  method <- toupper(match.arg(method))

  timeout <- timeout %||% .pc_state$config$timeout
  retries <- retries %||% .pc_state$config$retries
  pause_base <- pause_base %||% .pc_state$config$pause_base
  pause_cap <- pause_cap %||% .pc_state$config$pause_cap
  user_agent <- user_agent %||% .pc_state$config$user_agent
  cache_dir <- cache_dir %||% .pc_state$config$cache_dir
  cache_ttl <- cache_ttl %||% .pc_state$config$cache_ttl
  offline <- offline %||% .pc_state$config$offline

  url <- pc_build_url(
    domain = domain,
    namespace = namespace,
    identifier = identifier,
    operation = operation,
    output = output,
    searchtype = searchtype,
    options = options
  )

  request_meta <- list(
    method = method,
    url = url,
    domain = domain,
    namespace = namespace,
    identifier = identifier,
    operation = operation,
    searchtype = searchtype,
    output = output,
    options = options
  )

  cache_key <- pc_cache_key(method = method, url = url, body = body)
  if (isTRUE(cache) && !isTRUE(force_refresh)) {
    cached <- pc_cache_get(cache_key, cache_dir = cache_dir, ttl = cache_ttl)
    if (!is.null(cached)) {
      cached$from_cache <- TRUE
      return(cached)
    }
  }

  if (isTRUE(offline)) {
    return(pc_make_result(
      success = FALSE,
      request = request_meta,
      data = NULL,
      error = pc_make_error(
        code = "OfflineCacheMiss",
        message = "Offline mode enabled and no cached response found for this request."
      )
    ))
  }

  pc_throttle(rate_limit = rate_limit)

  req_args <- list(
    verb = method,
    url = url,
    times = retries,
    pause_base = pause_base,
    pause_cap = pause_cap,
    quiet = TRUE,
    httr::timeout(timeout),
    httr::user_agent(user_agent)
  )

  dots <- list(...)
  if (length(dots) > 0) {
    req_args <- c(req_args, dots)
  }

  if (method == "POST") {
    req_args$body <- body
    req_args$encode <- "form"
  }

  resp <- tryCatch(
    do.call(httr::RETRY, req_args),
    error = function(e) {
      return(pc_make_result(
        success = FALSE,
        request = request_meta,
        data = NULL,
        error = pc_make_error(
          code = "TransportError",
          message = conditionMessage(e),
          details = list(class = class(e))
        )
      ))
    }
  )

  if (inherits(resp, "PubChemResult")) {
    return(resp)
  }

  out <- pc_response(resp, request = request_meta)

  if (isTRUE(cache) && isTRUE(out$success)) {
    pc_cache_set(cache_key, out, cache_dir = cache_dir)
  }

  out
}

pc_result_to_tibble <- function(x) {
  if (is.null(x$data)) {
    return(tibble::tibble())
  }

  dat <- x$data

  if (is.data.frame(dat)) {
    return(tibble::as_tibble(dat))
  }

  if (is.list(dat)) {
    if (!is.null(dat$PropertyTable$Properties)) {
      props <- dat$PropertyTable$Properties
      if (is.data.frame(props)) {
        return(tibble::as_tibble(props, .name_repair = "unique"))
      }
      if (is.list(props)) {
        by_row <- tryCatch(dplyr::bind_rows(props), error = function(e) NULL)
        if (!is.null(by_row) && nrow(by_row) > 0) {
          return(tibble::as_tibble(by_row, .name_repair = "unique"))
        }

        by_col <- tryCatch(as.data.frame(props, stringsAsFactors = FALSE), error = function(e) NULL)
        if (!is.null(by_col)) {
          return(tibble::as_tibble(by_col, .name_repair = "unique"))
        }
      }
    }

    if (!is.null(dat$IdentifierList$CID)) {
      return(tibble::tibble(CID = dat$IdentifierList$CID))
    }

    if (!is.null(dat$IdentifierList$SID)) {
      return(tibble::tibble(SID = dat$IdentifierList$SID))
    }

    if (!is.null(dat$IdentifierList$AID)) {
      return(tibble::tibble(AID = dat$IdentifierList$AID))
    }

    if (!is.null(dat$InformationList$Information)) {
      info <- dat$InformationList$Information
      if (is.data.frame(info)) {
        return(tibble::as_tibble(info))
      }
      if (is.list(info)) {
        df <- tryCatch(dplyr::bind_rows(info), error = function(e) NULL)
        if (!is.null(df)) {
          return(tibble::as_tibble(df))
        }
      }
    }

    return(tibble::tibble(data = I(list(dat))))
  }

  tibble::tibble(value = as.character(dat))
}

#' @export
as_tibble.PubChemResult <- function(x, ...) {
  base <- pc_result_to_tibble(x)
  meta <- tibble::tibble(
    success = x$success,
    status = x$status %||% NA_integer_,
    from_cache = x$from_cache %||% FALSE,
    pending = x$pending %||% FALSE
  )
  if (nrow(base) == 0) {
    return(meta)
  }
  dplyr::bind_cols(meta[rep(1, nrow(base)), , drop = FALSE], base)
}

#' @export
as_tibble.PubChemRecord <- function(x, ...) {
  as_tibble.PubChemResult(x, ...)
}

#' @export
as_tibble.PubChemIdMap <- function(x, ...) {
  as_tibble.PubChemResult(x, ...)
}

#' @export
as_tibble.PubChemBatchResult <- function(x, ...) {
  tibble::tibble(
    chunk = seq_along(x$results),
    n_ids = vapply(x$chunks, length, integer(1)),
    success = x$success,
    error = x$error
  )
}

#' Query Compound Records via the Next-Generation API
#'
#' @inheritParams pc_request
#'
#' @return A typed `PubChemRecord` object.
#' @export
pc_compound <- function(identifier,
                        namespace = "cid",
                        operation = NULL,
                        searchtype = NULL,
                        output = "JSON",
                        options = NULL,
                        ...) {
  out <- pc_request(
    domain = "compound",
    namespace = namespace,
    identifier = identifier,
    operation = operation,
    searchtype = searchtype,
    output = output,
    options = options,
    ...
  )
  class(out) <- unique(c("PubChemRecord", class(out)))
  out
}

#' Build a Modeling-Ready Feature Table
#'
#' @param identifier Identifier vector for compound property retrieval.
#' @param properties Compound property names.
#' @param namespace Namespace for identifier.
#' @param numeric_only If `TRUE`, coerce feature columns to numeric where possible.
#' @param ... Additional arguments passed to `pc_property()`.
#'
#' @return A tibble of compound features suitable for downstream modeling workflows.
#' @export
pc_feature_table <- function(identifier,
                             properties = c("MolecularWeight", "XLogP", "TPSA", "HBondDonorCount", "HBondAcceptorCount"),
                             namespace = "cid",
                             numeric_only = TRUE,
                             ...) {
  res <- pc_property(
    identifier = identifier,
    properties = properties,
    namespace = namespace,
    ...
  )

  if (!isTRUE(res$success)) {
    stop("Property retrieval failed: ", res$error$message %||% "Unknown error")
  }

  tbl <- as_tibble(res)
  if (nrow(tbl) == 0) {
    return(tbl)
  }

  # Drop transport metadata columns and keep pure feature table.
  keep <- setdiff(names(tbl), c("success", "status", "from_cache", "pending"))
  tbl <- tbl[, keep, drop = FALSE]

  if (isTRUE(numeric_only)) {
    for (nm in names(tbl)) {
      if (nm %in% c("CID", "SID", "AID", "Identifier")) {
        next
      }
      suppressWarnings({
        cand <- as.numeric(tbl[[nm]])
      })
      # Convert only when at least one non-NA numeric value exists.
      if (sum(!is.na(cand)) > 0) {
        tbl[[nm]] <- cand
      }
    }
  }

  tibble::as_tibble(tbl)
}

#' Query Substance Records via the Next-Generation API
#'
#' @inheritParams pc_request
#'
#' @return A typed `PubChemRecord` object.
#' @export
pc_substance <- function(identifier,
                         namespace = "sid",
                         operation = "record",
                         output = "JSON",
                         options = NULL,
                         ...) {
  out <- pc_request(
    domain = "substance",
    namespace = namespace,
    identifier = identifier,
    operation = operation,
    output = output,
    options = options,
    ...
  )
  class(out) <- unique(c("PubChemRecord", class(out)))
  out
}

#' Query Assay Records via the Next-Generation API
#'
#' @inheritParams pc_request
#'
#' @return A typed `PubChemRecord` object.
#' @export
pc_assay <- function(identifier,
                     namespace = "aid",
                     operation = "description",
                     output = "JSON",
                     options = NULL,
                     ...) {
  out <- pc_request(
    domain = "assay",
    namespace = namespace,
    identifier = identifier,
    operation = operation,
    output = output,
    options = options,
    ...
  )
  class(out) <- unique(c("PubChemRecord", class(out)))
  out
}

#' Query Compound Properties via the Next-Generation API
#'
#' @param properties Character vector of property names.
#' @inheritParams pc_request
#'
#' @return A typed `PubChemRecord` object.
#' @export
pc_property <- function(identifier,
                        properties,
                        namespace = "cid",
                        searchtype = NULL,
                        options = NULL,
                        ...) {
  if (missing(properties) || length(properties) == 0) {
    stop("'properties' must include at least one property name.")
  }
  op <- paste("property", paste(properties, collapse = ","), sep = "/")
  out <- pc_request(
    domain = "compound",
    namespace = namespace,
    identifier = identifier,
    operation = op,
    searchtype = searchtype,
    output = "JSON",
    options = options,
    ...
  )
  class(out) <- unique(c("PubChemRecord", class(out)))
  out
}

#' Map Identifiers via the Next-Generation API
#'
#' @param to Operation target; usually one of `"cids"`, `"sids"`, or `"aids"`.
#' @inheritParams pc_request
#'
#' @return A typed `PubChemIdMap` object.
#' @export
pc_identifier_map <- function(identifier,
                              namespace = "name",
                              to = c("cids", "sids", "aids"),
                              domain = "compound",
                              searchtype = NULL,
                              options = NULL,
                              ...) {
  to <- match.arg(to)
  out <- pc_request(
    domain = domain,
    namespace = namespace,
    identifier = identifier,
    operation = to,
    searchtype = searchtype,
    output = "JSON",
    options = options,
    ...
  )
  class(out) <- unique(c("PubChemIdMap", class(out)))
  out
}

pc_parallel_apply <- function(x, fn, parallel = FALSE, workers = NULL) {
  if (!isTRUE(parallel)) {
    return(lapply(x, fn))
  }

  if (.Platform$OS.type == "windows") {
    warning("Parallel batching on Windows currently falls back to sequential execution.")
    lapply(x, fn)
  } else {
    workers <- workers %||% max(1, parallel::detectCores() - 1)
    parallel::mclapply(x, fn, mc.cores = workers)
  }
}

#' Batch-Orchestrate PubChem Workflows
#'
#' @param ids Identifier vector.
#' @param fn Function to run on each chunk of `ids`.
#' @param chunk_size Chunk size.
#' @param parallel Logical; use parallel execution.
#' @param workers Number of workers.
#' @param checkpoint_dir Optional directory to persist per-chunk checkpoint files.
#' @param checkpoint_id Optional checkpoint run id. If `NULL`, a deterministic id is generated.
#' @param resume Logical; resume from an existing checkpoint manifest.
#' @param rerun_failed Logical; when resuming, rerun chunks previously marked as failed.
#' @param ... Additional arguments passed into `fn`.
#'
#' @return A typed `PubChemBatchResult` object.
#' @export
pc_batch <- function(ids,
                     fn,
                     chunk_size = 100,
                     parallel = FALSE,
                     workers = NULL,
                     checkpoint_dir = NULL,
                     checkpoint_id = NULL,
                     resume = FALSE,
                     rerun_failed = TRUE,
                     ...) {
  if (missing(ids) || length(ids) == 0) {
    stop("'ids' must contain at least one identifier.")
  }
  if (!is.function(fn)) {
    stop("'fn' must be a function.")
  }
  if (!is.numeric(chunk_size) || chunk_size <= 0) {
    stop("'chunk_size' must be a positive integer.")
  }

  idx <- ceiling(seq_along(ids) / as.integer(chunk_size))
  chunks <- split(ids, idx)

  checkpoint_enabled <- !is.null(checkpoint_dir)
  checkpoint_id <- checkpoint_id %||% paste0(
    "batch_",
    pc_hash_text(pc_stable_string(list(ids = as.character(ids), chunk_size = as.integer(chunk_size))))
  )

  checkpoint_manifest_path <- if (checkpoint_enabled) {
    if (!dir.exists(checkpoint_dir)) {
      dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
    }
    file.path(checkpoint_dir, paste0("pc_batch_", checkpoint_id, "_manifest.rds"))
  } else {
    NULL
  }

  checkpoint_chunk_path <- function(i) {
    file.path(checkpoint_dir, paste0("pc_batch_", checkpoint_id, "_chunk_", sprintf("%05d", i), ".rds"))
  }

  save_manifest <- function(manifest) {
    if (checkpoint_enabled) {
      saveRDS(manifest, checkpoint_manifest_path)
    }
    invisible(manifest)
  }

  load_manifest <- function() {
    if (!file.exists(checkpoint_manifest_path)) {
      stop(
        "No checkpoint manifest found for id '", checkpoint_id,
        "' in '", checkpoint_dir, "'."
      )
    }
    readRDS(checkpoint_manifest_path)
  }

  make_manifest <- function(chunk_status, errors) {
    list(
      version = 1L,
      checkpoint_id = checkpoint_id,
      checkpoint_dir = checkpoint_dir,
      ids = as.character(ids),
      chunks = lapply(chunks, as.character),
      chunk_size = as.integer(chunk_size),
      chunk_status = as.character(chunk_status),
      error = as.character(errors),
      chunk_files = vapply(seq_along(chunks), checkpoint_chunk_path, character(1)),
      updated_at = Sys.time()
    )
  }

  worker_fun <- function(chunk_ids) {
    tryCatch(
      fn(chunk_ids, ...),
      error = function(e) {
        pc_make_result(
          success = FALSE,
          request = list(identifier = chunk_ids),
          error = pc_make_error("BatchWorkerError", conditionMessage(e))
        )
      }
    )
  }

  n_chunks <- length(chunks)
  results <- vector("list", n_chunks)
  chunk_status <- rep("pending", n_chunks)
  errors <- rep("", n_chunks)
  resumed <- FALSE

  if (checkpoint_enabled) {
    if (isTRUE(resume)) {
      manifest <- load_manifest()
      if (!identical(as.character(manifest$ids), as.character(ids))) {
        stop("Checkpoint manifest IDs do not match the provided 'ids'.")
      }
      if (!identical(as.integer(manifest$chunk_size), as.integer(chunk_size))) {
        stop("Checkpoint manifest chunk_size does not match provided 'chunk_size'.")
      }
      if (length(manifest$chunks) != n_chunks) {
        stop("Checkpoint manifest chunk layout does not match the provided ids/chunk_size.")
      }

      resumed <- TRUE
      chunk_status <- as.character(manifest$chunk_status %||% rep("pending", n_chunks))
      errors <- as.character(manifest$error %||% rep("", n_chunks))

      for (i in seq_len(n_chunks)) {
        cp <- checkpoint_chunk_path(i)
        keep_failed <- identical(chunk_status[i], "failed") && !isTRUE(rerun_failed)
        keep_success <- identical(chunk_status[i], "success")
        if (keep_success || keep_failed) {
          if (file.exists(cp)) {
            obj <- tryCatch(readRDS(cp), error = function(e) NULL)
            if (!is.null(obj)) {
              results[[i]] <- obj
            } else {
              chunk_status[i] <- "pending"
              errors[i] <- "Checkpoint chunk file unreadable. Re-running chunk."
            }
          } else {
            chunk_status[i] <- "pending"
            errors[i] <- "Checkpoint chunk file missing. Re-running chunk."
          }
        }
        if (identical(chunk_status[i], "failed") && isTRUE(rerun_failed)) {
          chunk_status[i] <- "pending"
          errors[i] <- ""
        }
      }
    } else {
      # Start a fresh manifest and overwrite previous run metadata for same id.
      if (file.exists(checkpoint_manifest_path)) {
        warning(
          "Existing checkpoint manifest found for id '", checkpoint_id,
          "'. Starting a fresh run (resume = FALSE)."
        )
      }
      old_chunk_files <- list.files(
        checkpoint_dir,
        pattern = paste0("^pc_batch_", gsub("([.|()\\^{}+$*?]|\\[|\\]|\\\\)", "\\\\\\1", checkpoint_id), "_chunk_"),
        full.names = TRUE
      )
      if (length(old_chunk_files) > 0) {
        unlink(old_chunk_files, force = TRUE)
      }
    }

    save_manifest(make_manifest(chunk_status = chunk_status, errors = errors))
  }

  run_indices <- which(chunk_status == "pending")
  if (length(run_indices) > 0) {
    use_parallel <- isTRUE(parallel) && !checkpoint_enabled
    if (isTRUE(parallel) && checkpoint_enabled) {
      warning("Parallel mode with checkpointing falls back to sequential execution to keep manifest writes safe.")
    }

    if (use_parallel) {
      pending_chunks <- chunks[run_indices]
      run_results <- pc_parallel_apply(pending_chunks, worker_fun, parallel = TRUE, workers = workers)
      for (k in seq_along(run_indices)) {
        i <- run_indices[[k]]
        results[[i]] <- run_results[[k]]
      }
    } else {
      for (i in run_indices) {
        res_i <- worker_fun(chunks[[i]])
        results[[i]] <- res_i

        if (checkpoint_enabled) {
          saveRDS(res_i, checkpoint_chunk_path(i))
          if (inherits(res_i, "PubChemResult") && !isTRUE(res_i$success)) {
            chunk_status[i] <- "failed"
            errors[i] <- res_i$error$message %||% "Unknown error"
          } else {
            chunk_status[i] <- "success"
            errors[i] <- ""
          }
          save_manifest(make_manifest(chunk_status = chunk_status, errors = errors))
        }
      }
    }
  }

  success <- vapply(results, function(x) {
    if (is.null(x)) {
      FALSE
    } else if (inherits(x, "PubChemResult")) {
      isTRUE(x$success)
    } else {
      TRUE
    }
  }, logical(1))

  errors <- vapply(seq_along(results), function(i) {
    x <- results[[i]]
    if (inherits(x, "PubChemResult") && !isTRUE(x$success)) {
      return(x$error$message %||% "Unknown error")
    }
    if (!is.null(errors[[i]]) && nzchar(errors[[i]])) {
      return(errors[[i]])
    }
    if (is.null(x)) {
      return("Chunk has no result.")
    }
    ""
  }, character(1))

  if (checkpoint_enabled) {
    chunk_status <- ifelse(success, "success", ifelse(nzchar(errors), "failed", "pending"))
    save_manifest(make_manifest(chunk_status = chunk_status, errors = errors))
  } else {
    chunk_status <- ifelse(success, "success", "failed")
  }

  structure(
    list(
      ids = ids,
      chunks = chunks,
      results = results,
      success = success,
      error = errors,
      chunk_size = as.integer(chunk_size),
      parallel = isTRUE(parallel),
      workers = workers %||% 1,
      chunk_status = chunk_status,
      checkpoint = list(
        enabled = checkpoint_enabled,
        id = if (checkpoint_enabled) checkpoint_id else NULL,
        dir = checkpoint_dir,
        manifest = checkpoint_manifest_path,
        resumed = resumed,
        rerun_failed = isTRUE(rerun_failed)
      )
    ),
    class = "PubChemBatchResult"
  )
}

#' Resume a Checkpointed Batch Workflow
#'
#' @param fn Function to run on each pending chunk.
#' @param checkpoint_dir Directory containing checkpoint manifest/files.
#' @param checkpoint_id Checkpoint run id.
#' @param parallel Logical; use parallel execution where supported.
#' @param workers Number of workers.
#' @param rerun_failed Logical; rerun chunks previously marked as failed.
#' @param ... Additional arguments passed into `fn`.
#'
#' @return A typed `PubChemBatchResult` object.
#' @export
pc_resume_batch <- function(fn,
                            checkpoint_dir,
                            checkpoint_id,
                            parallel = FALSE,
                            workers = NULL,
                            rerun_failed = TRUE,
                            ...) {
  if (missing(fn) || !is.function(fn)) {
    stop("'fn' must be a function.")
  }
  if (missing(checkpoint_dir) || !is.character(checkpoint_dir) || !nzchar(checkpoint_dir)) {
    stop("'checkpoint_dir' must be a non-empty character path.")
  }
  if (missing(checkpoint_id) || !is.character(checkpoint_id) || !nzchar(checkpoint_id)) {
    stop("'checkpoint_id' must be a non-empty character value.")
  }

  manifest_path <- file.path(checkpoint_dir, paste0("pc_batch_", checkpoint_id, "_manifest.rds"))
  if (!file.exists(manifest_path)) {
    stop(
      "No checkpoint manifest found for id '", checkpoint_id,
      "' in '", checkpoint_dir, "'."
    )
  }

  manifest <- readRDS(manifest_path)
  ids <- manifest$ids
  chunk_size <- as.integer(manifest$chunk_size %||% 100L)

  pc_batch(
    ids = ids,
    fn = fn,
    chunk_size = chunk_size,
    parallel = parallel,
    workers = workers,
    checkpoint_dir = checkpoint_dir,
    checkpoint_id = checkpoint_id,
    resume = TRUE,
    rerun_failed = rerun_failed,
    ...
  )
}

#' Benchmark Chunked PubChem Workflows
#'
#' @param ids Identifier vector.
#' @param fn Function applied by `pc_batch()`.
#' @param chunk_sizes Integer vector of chunk sizes.
#' @param parallel_options Logical vector controlling parallel toggle.
#' @param workers Number of workers used when parallel is enabled.
#' @param ... Additional arguments passed to `fn`.
#'
#' @return A tibble with runtime and success metrics for each benchmark scenario.
#' @export
pc_benchmark <- function(ids,
                         fn,
                         chunk_sizes = c(25, 50, 100),
                         parallel_options = c(FALSE),
                         workers = NULL,
                         ...) {
  if (missing(ids) || length(ids) == 0) {
    stop("'ids' must contain at least one identifier.")
  }
  if (!is.function(fn)) {
    stop("'fn' must be a function.")
  }

  scenarios <- expand.grid(
    chunk_size = as.integer(chunk_sizes),
    parallel = as.logical(parallel_options),
    stringsAsFactors = FALSE
  )

  out <- lapply(seq_len(nrow(scenarios)), function(i) {
    chunk_size <- scenarios$chunk_size[[i]]
    par_flag <- scenarios$parallel[[i]]

    t0 <- proc.time()[["elapsed"]]
    b <- pc_batch(
      ids = ids,
      fn = fn,
      chunk_size = chunk_size,
      parallel = par_flag,
      workers = workers,
      ...
    )
    elapsed <- proc.time()[["elapsed"]] - t0

    tibble::tibble(
      chunk_size = chunk_size,
      parallel = par_flag,
      workers = ifelse(par_flag, workers %||% 1, 1),
      elapsed_sec = elapsed,
      chunks = length(b$chunks),
      successful_chunks = sum(b$success),
      failed_chunks = sum(!b$success)
    )
  })

  dplyr::bind_rows(out)
}

pc_default_benchmark_thresholds <- function() {
  list(
    elapsed_sec = c(`10` = 30, `1000` = 300, `100000` = 3600),
    failed_chunk_ratio = c(`10` = 0, `1000` = 0.01, `100000` = 0.05)
  )
}

pc_benchmark_threshold_value <- function(x, scenario_size, default = Inf) {
  if (is.null(x)) {
    return(default)
  }

  if (length(x) == 1 && is.null(names(x))) {
    return(as.numeric(x[[1]]))
  }

  nms <- names(x)
  if (is.null(nms)) {
    return(default)
  }

  key <- as.character(scenario_size)
  if (key %in% nms) {
    return(as.numeric(x[[key]]))
  }

  default
}

pc_benchmark_make_ids <- function(ids, n, id_generator = NULL) {
  if (!is.null(id_generator)) {
    if (!is.function(id_generator)) {
      stop("'id_generator' must be a function when provided.")
    }
    made <- id_generator(as.integer(n))
    if (length(made) != as.integer(n)) {
      stop("'id_generator' must return exactly n identifiers.")
    }
    return(made)
  }

  if (is.null(ids)) {
    return(as.character(seq_len(as.integer(n))))
  }

  if (length(ids) >= n) {
    return(ids[seq_len(as.integer(n))])
  }

  rep_len(ids, as.integer(n))
}

pc_write_benchmark_report <- function(report, report_path, report_format = c("markdown", "csv", "rds")) {
  report_format <- match.arg(report_format)
  dir.create(dirname(report_path), recursive = TRUE, showWarnings = FALSE)

  if (report_format == "markdown") {
    lines <- c(
      "# PubChem Benchmark Harness Report",
      "",
      paste0("- Generated: ", format(report$timestamp, "%Y-%m-%d %H:%M:%S %Z")),
      paste0("- Scenario sizes: ", paste(report$scenario_sizes, collapse = ", ")),
      "",
      "## Summary",
      ""
    )
    lines <- c(lines, utils::capture.output(print(report$summary)))
    lines <- c(lines, "", "## Details", "")
    lines <- c(lines, utils::capture.output(print(report$details)))
    writeLines(lines, con = report_path, useBytes = TRUE)
  } else if (report_format == "csv") {
    utils::write.csv(report$summary, file = report_path, row.names = FALSE)
  } else {
    saveRDS(report, file = report_path)
  }

  invisible(report_path)
}

pc_calibrate_benchmark_thresholds <- function(history,
                                              baseline = pc_default_benchmark_thresholds(),
                                              quantile_prob = 0.95,
                                              elapsed_buffer = 1.25,
                                              failed_ratio_buffer = 1.5,
                                              min_runs = 3L) {
  required_cols <- c("scenario_size", "max_elapsed_sec", "max_failed_chunk_ratio")
  if (!is.data.frame(history) || !all(required_cols %in% names(history))) {
    stop(
      "'history' must be a data.frame containing columns: ",
      paste(required_cols, collapse = ", ")
    )
  }
  if (!is.numeric(quantile_prob) || length(quantile_prob) != 1 || quantile_prob <= 0 || quantile_prob >= 1) {
    stop("'quantile_prob' must be a numeric scalar in (0, 1).")
  }

  baseline_elapsed <- baseline$elapsed_sec %||% numeric(0)
  baseline_failed <- baseline$failed_chunk_ratio %||% numeric(0)
  all_scenarios <- sort(unique(c(
    as.integer(names(baseline_elapsed)),
    as.integer(history$scenario_size)
  )))
  all_scenarios <- all_scenarios[!is.na(all_scenarios)]

  calibrated_elapsed <- baseline_elapsed
  calibrated_failed <- baseline_failed

  for (sc in all_scenarios) {
    part <- history[as.integer(history$scenario_size) == as.integer(sc), , drop = FALSE]
    if (nrow(part) < as.integer(min_runs)) {
      next
    }

    elapsed_q <- stats::quantile(part$max_elapsed_sec, probs = quantile_prob, na.rm = TRUE, names = FALSE, type = 7)
    failed_q <- stats::quantile(part$max_failed_chunk_ratio, probs = quantile_prob, na.rm = TRUE, names = FALSE, type = 7)

    elapsed_val <- as.numeric(elapsed_q) * as.numeric(elapsed_buffer)
    failed_val <- min(1, as.numeric(failed_q) * as.numeric(failed_ratio_buffer))

    key <- as.character(sc)
    base_elapsed <- pc_benchmark_threshold_value(baseline_elapsed, scenario_size = sc, default = 0)
    base_failed <- pc_benchmark_threshold_value(baseline_failed, scenario_size = sc, default = 0)

    calibrated_elapsed[[key]] <- max(base_elapsed, elapsed_val, na.rm = TRUE)
    calibrated_failed[[key]] <- max(base_failed, failed_val, na.rm = TRUE)
  }

  list(
    elapsed_sec = calibrated_elapsed,
    failed_chunk_ratio = calibrated_failed
  )
}

#' Benchmark Harness for Scale Scenarios
#'
#' Executes benchmark scenarios (defaults: 10, 1000, 100000 identifiers),
#' evaluates threshold gates, and optionally writes a report artifact.
#'
#' @param fn Function applied by `pc_batch()`/`pc_benchmark()`.
#' @param ids Optional base identifier vector. If shorter than a scenario size,
#'   identifiers are recycled.
#' @param scenario_sizes Integer vector of benchmark scenario sizes.
#' @param id_generator Optional function `function(n)` returning `n` identifiers.
#' @param chunk_sizes Integer vector of chunk sizes evaluated per scenario.
#' @param parallel_options Logical vector controlling parallel toggle.
#' @param workers Number of workers used when parallel is enabled.
#' @param thresholds Named list with optional elements `elapsed_sec` and
#'   `failed_chunk_ratio`. Each can be scalar or a named numeric vector keyed by
#'   scenario size.
#' @param report_path Optional path to write report output.
#' @param report_format One of `"markdown"`, `"csv"`, or `"rds"`.
#' @param ... Additional arguments passed to `fn`.
#'
#' @return An object of class `PubChemBenchmarkReport` containing
#'   `details` and `summary` tibbles.
#' @export
pc_benchmark_harness <- function(fn,
                                 ids = NULL,
                                 scenario_sizes = c(10L, 1000L, 100000L),
                                 id_generator = NULL,
                                 chunk_sizes = c(25L, 100L, 1000L),
                                 parallel_options = c(FALSE),
                                 workers = NULL,
                                 thresholds = pc_default_benchmark_thresholds(),
                                 report_path = NULL,
                                 report_format = c("markdown", "csv", "rds"),
                                 ...) {
  if (!is.function(fn)) {
    stop("'fn' must be a function.")
  }
  if (!is.numeric(chunk_sizes) || length(chunk_sizes) == 0 || any(is.na(chunk_sizes)) || any(chunk_sizes <= 0)) {
    stop("'chunk_sizes' must contain positive integers.")
  }
  if (!is.logical(parallel_options) || length(parallel_options) == 0) {
    stop("'parallel_options' must contain at least one logical value.")
  }
  if (!is.null(id_generator) && !is.function(id_generator)) {
    stop("'id_generator' must be a function when provided.")
  }
  if (!is.list(thresholds)) {
    stop("'thresholds' must be a list.")
  }

  scenario_sizes <- as.integer(scenario_sizes)
  if (length(scenario_sizes) == 0 || any(is.na(scenario_sizes)) || any(scenario_sizes <= 0)) {
    stop("'scenario_sizes' must contain positive integers.")
  }

  report_format <- match.arg(report_format)

  details <- lapply(scenario_sizes, function(n) {
    ids_n <- pc_benchmark_make_ids(ids = ids, n = n, id_generator = id_generator)
    bm <- pc_benchmark(
      ids = ids_n,
      fn = fn,
      chunk_sizes = as.integer(chunk_sizes),
      parallel_options = parallel_options,
      workers = workers,
      ...
    )

    elapsed_thr <- pc_benchmark_threshold_value(thresholds$elapsed_sec, scenario_size = n, default = Inf)
    fail_thr <- pc_benchmark_threshold_value(thresholds$failed_chunk_ratio, scenario_size = n, default = Inf)

    bm$scenario_size <- as.integer(n)
    bm$failed_chunk_ratio <- ifelse(bm$chunks > 0, bm$failed_chunks / bm$chunks, 0)
    bm$elapsed_threshold <- elapsed_thr
    bm$failed_ratio_threshold <- fail_thr
    bm$meets_elapsed <- bm$elapsed_sec <= elapsed_thr
    bm$meets_failed_ratio <- bm$failed_chunk_ratio <= fail_thr
    bm$run_pass <- bm$meets_elapsed & bm$meets_failed_ratio
    bm
  })

  details_tbl <- dplyr::bind_rows(details)

  summary_tbl <- dplyr::bind_rows(lapply(scenario_sizes, function(n) {
    part <- details_tbl[details_tbl$scenario_size == as.integer(n), , drop = FALSE]
    tibble::tibble(
      scenario_size = as.integer(n),
      runs = nrow(part),
      min_elapsed_sec = if (nrow(part) > 0) min(part$elapsed_sec, na.rm = TRUE) else NA_real_,
      max_elapsed_sec = if (nrow(part) > 0) max(part$elapsed_sec, na.rm = TRUE) else NA_real_,
      max_failed_chunk_ratio = if (nrow(part) > 0) max(part$failed_chunk_ratio, na.rm = TRUE) else NA_real_,
      elapsed_threshold = if (nrow(part) > 0) unique(part$elapsed_threshold)[1] else NA_real_,
      failed_ratio_threshold = if (nrow(part) > 0) unique(part$failed_ratio_threshold)[1] else NA_real_,
      all_runs_pass = if (nrow(part) > 0) all(part$run_pass) else FALSE,
      any_run_pass = if (nrow(part) > 0) any(part$run_pass) else FALSE
    )
  }))

  out <- structure(
    list(
      scenario_sizes = scenario_sizes,
      thresholds = thresholds,
      details = details_tbl,
      summary = summary_tbl,
      report_path = report_path,
      report_format = report_format,
      timestamp = Sys.time()
    ),
    class = "PubChemBenchmarkReport"
  )

  if (!is.null(report_path)) {
    pc_write_benchmark_report(out, report_path = report_path, report_format = report_format)
  }

  out
}

#' @export
print.PubChemBenchmarkReport <- function(x, ...) {
  cat("\n")
  cat(" PubChemBenchmarkReport", "\n\n")
  cat("  - Scenarios: ", paste(x$scenario_sizes, collapse = ", "), "\n", sep = "")
  cat("  - Runs: ", nrow(x$details), "\n", sep = "")
  cat("  - Passed scenarios: ", sum(x$summary$all_runs_pass), "/", nrow(x$summary), "\n", sep = "")
  if (!is.null(x$report_path)) {
    cat("  - Report: ", x$report_path, " (", x$report_format, ")", "\n", sep = "")
  }
  invisible(x)
}

pc_extract_listkey <- function(x) {
  if (inherits(x, "PubChemResult")) {
    return(x$listkey %||% NULL)
  }
  NULL
}

#' Submit an Asynchronous PubChem Query
#'
#' @inheritParams pc_request
#'
#' @return An object of class `PubChemAsyncQuery`.
#' @export
pc_submit <- function(domain = "compound",
                      namespace = "cid",
                      identifier = NULL,
                      operation = NULL,
                      searchtype = NULL,
                      output = "JSON",
                      options = NULL,
                      ...) {
  initial <- pc_request(
    domain = domain,
    namespace = namespace,
    identifier = identifier,
    operation = operation,
    searchtype = searchtype,
    output = output,
    options = options,
    ...
  )

  structure(
    list(
      initial = initial,
      listkey = pc_extract_listkey(initial),
      domain = domain,
      operation = operation,
      output = output,
      options = options
    ),
    class = "PubChemAsyncQuery"
  )
}

#' Poll an Asynchronous PubChem ListKey
#'
#' @param x A `PubChemAsyncQuery` object or listkey string.
#' @param domain Domain for polling.
#' @param operation Operation for polling.
#' @param output Output format.
#' @param options Polling options.
#' @param interval Poll interval in seconds.
#' @param max_attempts Maximum polling attempts.
#' @param ... Additional arguments passed to `pc_request`.
#'
#' @return A `PubChemResult` object.
#' @export
pc_poll <- function(x,
                    domain = "compound",
                    operation = NULL,
                    output = "JSON",
                    options = NULL,
                    interval = 1.5,
                    max_attempts = 20,
                    ...) {
  if (inherits(x, "PubChemAsyncQuery")) {
    listkey <- x$listkey
    domain <- x$domain %||% domain
    operation <- x$operation %||% operation
    output <- x$output %||% output
    options <- x$options %||% options

    if (is.null(listkey) && inherits(x$initial, "PubChemResult")) {
      return(x$initial)
    }
  } else {
    listkey <- as.character(x)
  }

  if (is.null(listkey) || !nzchar(listkey)) {
    stop("No valid listkey found for polling.")
  }

  last <- NULL
  for (i in seq_len(max_attempts)) {
    res <- pc_request(
      domain = domain,
      namespace = "listkey",
      identifier = listkey,
      operation = operation,
      output = output,
      options = options,
      ...
    )

    last <- res
    if (!isTRUE(res$pending)) {
      return(res)
    }
    Sys.sleep(interval)
  }

  pc_make_result(
    success = FALSE,
    request = list(domain = domain, namespace = "listkey", identifier = listkey, operation = operation),
    data = if (!is.null(last)) last$data else NULL,
    error = pc_make_error(
      code = "PollingTimeout",
      message = paste0("ListKey ", listkey, " is still pending after ", max_attempts, " attempts.")
    )
  )
}

#' Collect Results From an Async PubChem Query
#'
#' @param x A `PubChemAsyncQuery` object.
#' @param ... Additional arguments passed to `pc_poll`.
#'
#' @return A `PubChemResult` object.
#' @export
pc_collect <- function(x, ...) {
  if (!inherits(x, "PubChemAsyncQuery")) {
    stop("'x' must be a PubChemAsyncQuery object.")
  }

  if (!is.null(x$listkey)) {
    return(pc_poll(x, ...))
  }

  x$initial
}

#' @export
print.PubChemResult <- function(x, ...) {
  cat("\n")
  cat(" PubChemResult", "\n\n")
  cat("  - Success: ", x$success, "\n", sep = "")
  cat("  - Status: ", ifelse(is.null(x$status), "NA", x$status), "\n", sep = "")
  cat("  - Pending: ", x$pending %||% FALSE, "\n", sep = "")
  if (!is.null(x$listkey)) {
    cat("  - ListKey: ", x$listkey, "\n", sep = "")
  }
  cat("  - From cache: ", x$from_cache %||% FALSE, "\n", sep = "")
  if (!x$success && !is.null(x$error)) {
    cat("  - Error Code: ", x$error$code %||% "Unknown", "\n", sep = "")
    cat("  - Error Message: ", x$error$message %||% "Unknown error", "\n", sep = "")
  }
  invisible(x)
}

#' @export
print.PubChemBatchResult <- function(x, ...) {
  cat("\n")
  cat(" PubChemBatchResult", "\n\n")
  cat("  - Chunks: ", length(x$chunks), "\n", sep = "")
  cat("  - Chunk size: ", x$chunk_size, "\n", sep = "")
  cat("  - Parallel: ", x$parallel, "\n", sep = "")
  cat("  - Successful chunks: ", sum(x$success), "/", length(x$success), "\n", sep = "")
  if (!is.null(x$checkpoint) && isTRUE(x$checkpoint$enabled)) {
    cat("  - Checkpoint id: ", x$checkpoint$id, "\n", sep = "")
    cat("  - Resumed: ", x$checkpoint$resumed, "\n", sep = "")
  }
  invisible(x)
}
