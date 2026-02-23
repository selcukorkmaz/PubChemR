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
#' @param ... Additional arguments passed into `fn`.
#'
#' @return A typed `PubChemBatchResult` object.
#' @export
pc_batch <- function(ids,
                     fn,
                     chunk_size = 100,
                     parallel = FALSE,
                     workers = NULL,
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

  results <- pc_parallel_apply(chunks, worker_fun, parallel = parallel, workers = workers)

  success <- vapply(results, function(x) {
    if (inherits(x, "PubChemResult")) isTRUE(x$success) else TRUE
  }, logical(1))

  errors <- vapply(results, function(x) {
    if (inherits(x, "PubChemResult") && !isTRUE(x$success)) {
      x$error$message %||% "Unknown error"
    } else {
      ""
    }
  }, character(1))

  structure(
    list(
      ids = ids,
      chunks = chunks,
      results = results,
      success = success,
      error = errors,
      chunk_size = as.integer(chunk_size),
      parallel = isTRUE(parallel),
      workers = workers %||% 1
    ),
    class = "PubChemBatchResult"
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
  invisible(x)
}
