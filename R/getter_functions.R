# Getter Functions ----
## PubChemRequest class ----

#' Call Function Parameters
#'
#' @param object <details here>
#' @param ... <details here>
#'
#' @return a list
#'
#' @examples
#' 1L
#'
#' @export
request_args <- function(object, .which = NULL, ...){
  if (is.null(.which)){
    return(object[["request_args"]])
  } else {
    return(object[["request_args"]][[.which]])
  }
}

#' @export
instance.PubChemInstanceList <- function(object, .which = NULL, ....){
  if (is.null(.which)){
    idx <- 1
  } else {
    if (!(.which %in% request_args(object, "identifier"))){
      stop("Unknown instance identifier. Run 'request_args(object, \"identifier\")' to see all the requested instance identifiers.")
    }
    idx <- which(request_args(object, "identifier") == .which)
  }

  return(object[[1]][[idx]])
}


#' @export
instance <- function(object, ...){
  UseMethod("instance")
}


## PubChemInstance class ----
#' @export
atoms.PubChemInstance <- function(object, ...){
  if (!object$success){
    return(stop("'object' encountered an error. Nothing to return. \n See error details in 'object'."))
  }

  object$result[[1]][[1]][["atoms"]]
}

#' @export
atoms.PubChemRequest <- function(object, .which = NULL, ...){
  if (is.null(.which)){
    idx <- 1
  } else {
    if (!(.which %in% object$call_parameters$identifier)){
      stop("Compound identifier is not available within 'object'.")
    }
    idx <- which(object$call_parameters$identifier == .which)
  }

  object <- instance(object, .which = object$call_parameters$identifier[idx])
  atoms(object, ...)
}

#' @export
atoms <- function(object, ...){
  UseMethod("atoms")
}

#' @export
bonds.PubChemInstance <- function(object, ...){
  if (!object$success){
    return(stop("'object' encountered an error. Nothing to return. \n See error details in 'object'."))
  }

  object$result[[1]][[1]][["bonds"]]
}

#' @export
bonds.PubChemRequest <- function(object, .which = NULL, ...){
  if (is.null(.which)){
    idx <- 1
  } else {
    if (!(.which %in% object$call_parameters$identifier)){
      stop("Compound identifier is not available within 'object'.")
    }
    idx <- which(object$call_parameters$identifier == .which)
  }

  object <- instance(object, .which = object$call_parameters$identifier[idx])
  bonds(object, ...)
}

#' @export
bonds <- function(object, ...){
  UseMethod("bonds")
}

#' @export
coords.PubChemInstance <- function(object, ...){
  if (!object$success){
    return(stop("'object' encountered an error. Nothing to return. \n See error details in 'object'."))
  }

  object$result[[1]][[1]][["coords"]]
}

#' @export
coords.PubChemRequest <- function(object, .which = NULL, ...){
  if (is.null(.which)){
    idx <- 1
  } else {
    if (!(.which %in% object$call_parameters$identifier)){
      stop("Compound identifier is not available within 'object'.")
    }
    idx <- which(object$call_parameters$identifier == .which)
  }

  object <- instance(object, .which = object$call_parameters$identifier[idx])
  coords(object, ...)
}

#' @export
coords <- function(object, ...){
  UseMethod("coords")
}

#' @export
charge.PubChemInstance <- function(object, ...){
  if (!object$success){
    return(stop("'object' encountered an error. Nothing to return. \n See error details in 'object'."))
  }

  object$result[[1]][[1]][["charge"]]
}

#' @export
charge.PubChemRequest <- function(object, .which = NULL, ...){
  if (is.null(.which)){
    idx <- 1
  } else {
    if (!(.which %in% object$call_parameters$identifier)){
      stop("Compound identifier is not available within 'object'.")
    }
    idx <- which(object$call_parameters$identifier == .which)
  }

  object <- instance(object, .which = object$call_parameters$identifier[idx])
  charge(object, ...)
}

#' @export
charge <- function(object, ...){
  UseMethod("charge")
}

#' @importFrom tidyr as_tibble
#' @importFrom magrittr '%>%'
#' @export
props.PubChemInstance <- function(object, .to.data.frame = TRUE, ...){

  if (!object$success){
    return(stop("'object' encountered an error. Nothing to return. \n See error details in 'object'."))
  }

  tmp <- object$result[[1]][[1]][["props"]]

  if (.to.data.frame){
    tmp <- lapply(tmp, function(x){
      as.data.frame(as.matrix(bind_cols(x)))
    })

    res <- tmp[[1]]
    for (i in 2:length(tmp)){
      res <- suppressMessages(full_join(res, tmp[[i]])) %>%
        as_tibble(.)
    }

    return(res)
  } else {
    return(tmp)
  }
}

#' @export
props.PubChemRequest <- function(object, .which = NULL, .to.data.frame = FALSE, ...){
  if (is.null(.which)){
    idx <- 1
  } else {
    if (!(.which %in% object$call_parameters$identifier)){
      stop("Compound identifier is not available within 'object'.")
    }
    idx <- which(object$call_parameters$identifier == .which)
  }

  object <- instance(object, .which = object$call_parameters$identifier[idx])
  props(object, .to.data.frame, ...)
}

#' @export
props <- function(object, ...){
  UseMethod("props")
}


#' @export
count.PubChemInstance <- function(object, ...){
  if (!object$success){
    return(stop("'object' encountered an error. Nothing to return. \n See error details in 'object'."))
  }

  object$result[[1]][[1]][["count"]]
}

#' @export
count.PubChemRequest <- function(object, .which = NULL, ...){
  if (is.null(.which)){
    idx <- 1
  } else {
    if (!(.which %in% object$call_parameters$identifier)){
      stop("Compound identifier is not available within 'object'.")
    }
    idx <- which(object$call_parameters$identifier == .which)
  }

  object <- instance(object, .which = object$call_parameters$identifier[idx])
  count(object, ...)
}

#' @export
count <- function(object, ...){
  UseMethod("count")
}

# PubChemInstance_AIDs ----
#' @export
AIDs.PubChemInstance_AIDs <- function(object, .to.data.frame = TRUE, ...){
  tmp <- object$result

  if (.to.data.frame){
    res <- lapply(tmp, function(x){
      bind_cols(x$result$InformationList$Information)
    }) %>%
      bind_rows(.) %>%
      as_tibble(.)
  } else {
    res <- lapply(tmp, "[[", "result")
  }

  return(res)
}

#' @export
AIDs <- function(object, ...){
  UseMethod("AIDs")
}

# PubChemInstance_CIDs ----
#' @export
CIDs.PubChemInstance_CIDs <- function(object, .to.data.frame = TRUE, ...){
  tmp <- object$result

  if (.to.data.frame){
    res <- lapply(tmp, function(x){
      xx <- suppressMessages({
        list(x$request_args$identifier, CID = x$result$IdentifierList$CID) %>%
          bind_cols()
      })

      names(xx)[1] <- namespace_text(x$request_args$namespace)
      return(xx)
    }) %>%
      bind_rows(.) %>%
      as_tibble(.)
  } else {
    res <- lapply(tmp, "[[", "result")
  }

  return(res)
}

#' @export
CIDs <- function(object, ...){
  UseMethod("CIDs")
}


# PubChemInstance_SIDs ----
#' @importFrom dplyr bind_rows
#' @importFrom tidyr as_tibble
#' @export
SIDs.PubChemInstance_SIDs <- function(object, .to.data.frame = TRUE, ...){
  tmp <- object$result

  if (.to.data.frame){
    res <- lapply(tmp, function(x){
      xx <- suppressMessages({
        list(x$request_args$identifier, SID = x$result$InformationList$Information[[1]]$SID) %>%
          bind_cols()
      })

      names(xx)[1] <- namespace_text(x$request_args$namespace)
      return(xx)
    }) %>%
      bind_rows(.) %>%
      as_tibble(.)
  } else {
    res <- lapply(tmp, "[[", "result")
  }

  return(res)
}

#' @export
SIDs <- function(object, ...){
  UseMethod("SIDs")
}
