pc_drop_null <- function(x) {
  x[!vapply(x, is.null, logical(1))]
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
    result = results,
    request_args = pc_drop_null(req),
    success = success,
    error = errors
  )
}
