# Getter Functions ----
## PubChemRequest class ----
call_params.PubChemRequest <- function(object){
  return(object[["call_parameters"]])
}

#' @export
call_params <- function(object, ...){
  UseMethod("call_params")
}


instance.PubChemRequest <- function(object, .which = NULL, ....){
  if (is.null(.which)){
    idx <- 1
  } else {
    if (!(.which %in% object$call_parameters$identifier)){
      stop("Compound identifier is not available within 'object'.")
    }
    idx <- which(object$call_parameters$identifier == .which)
  }

  return(object[[1]][[idx]])
}

#' @export
instance <- function(object, ...){
  UseMethod("instance")
}


## PubChemInstance class ----
atoms.PubChemInstance <- function(object, ...){
  object[["atoms"]]
}

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


bonds.PubChemInstance <- function(object, ...){
  object[["bonds"]]
}

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


coords.PubChemInstance <- function(object, ...){
  object[["coords"]]
}

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


charge.PubChemInstance <- function(object, ...){
  object[["charge"]]
}

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


props.PubChemInstance <- function(object, .to.data.frame = FALSE, ...){
  if (.to.data.frame){
    tmp <- lapply(object[["props"]], function(x){
      as.data.frame(as.matrix(bind_cols(x)))
    })

    res <- tmp[[1]]
    for (i in 2:length(tmp)){
      res <- suppressMessages(full_join(res, tmp[[i]]))
    }

    return(res)
  } else {
    object[["props"]]
  }
}

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


count.PubChemInstance <- function(object, ...){
  object[["count"]]
}

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
