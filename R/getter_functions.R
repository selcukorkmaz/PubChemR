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


# PubChemInstance (Assays) ----
#' @export
aid <- function(object, ...){
  UseMethod("aid")
}

#' @importFrom magrittr '%>%'
#' @importFrom tibble as_tibble as_tibble_row
#'
#' @export
aid.PubChemInstance <- function(object, ..., .to.data.frame = TRUE){
  res <- NULL

  if ("PC_Assay" %in% class(object) & object$success){
    tmp <- object$result$PC_AssayContainer[[1]]$assay$descr$aid

    if (.to.data.frame){
      if (is.vector(tmp)){
        res <- tmp %>%
          as_tibble_row
      } else {
        res <- tmp %>%
          as_tibble
      }
    } else {
      res <- tmp
    }
  }

  return(res)
}

#' @export
aid.PubChemInstanceList <- function(object, ..., .to.data.frame = TRUE, .which = NULL){
  res <- NULL

  if (is.null(.which)){
    idx <- 1
  } else {
    if (!(.which %in% request_args(object, "identifier"))){
      stop("Unknown instance identifier. Run 'request_args(object, \"identifier\")' to see all the requested instance identifiers.")
    }
    idx <- which(request_args(object, "identifier") == .which)
  }

  aid(object$result[[idx]], .to.data.frame = .to.data.frame)
}


#' @export
aid_source <- function(object, ..., .verbose = TRUE){
  UseMethod("aid_source")
}

#' @export
aid_source.PubChemInstance <- function(object, ..., .verbose = TRUE){
  res <- NULL

  if (object$success){
    if ("PC_Assay" %in% class(object)){
      tmp <- object$result$PC_AssayContainer[[1]]$assay$descr$aid_source[[1]]

      if (.verbose & !is.null(tmp)){
        cat("\n")
        cat(" PubChem Assay Source Details", "\n\n")
        for (i in 1:length(tmp)){
          cat(" ", ifelse(is.null(names(tmp[1])), "", paste0(" - ", names(tmp[i]), ": ", sep = "")), tmp[[i]], sep = "", "\n")
        }
        cat("\n")
      }

      res <- tmp
    }
  } else {
    if (.verbose){
      cat("\n")
      cat(" PubChem Assay Details (aid_source)", "\n\n")
      cat(" Nothing to return. An error has been occurred. See requested assay results for details.", "\n")
      cat("\n")
    }
  }

  invisible(res)
}

#' @export
aid_source.PubChemInstanceList <- function(object, ..., .which = NULL, .verbose = TRUE){
  res <- NULL

  if (is.null(.which)){
    idx <- 1
  } else {
    if (!(.which %in% request_args(object, "identifier"))){
      stop("Unknown instance identifier. Run 'request_args(object, \"identifier\")' to see all the requested instance identifiers.")
    }
    idx <- which(request_args(object, "identifier") == .which)
  }

  aid_source(object$result[[idx]], .verbose = .verbose)
}



#' @export
name <- function(object, ..., .verbose = TRUE){
  UseMethod("name")
}

#' @export
name.PubChemInstance <- function(object, ..., .verbose = TRUE){
  res <- NULL

  if (object$success){
    if ("PC_Assay" %in% class(object)){
      tmp <- object$result$PC_AssayContainer[[1]]$assay$descr$name

      if (.verbose & !is.null(tmp)){
        cat("\n")
        cat(" PubChem Assay Details (name)", "\n\n")
        for (i in 1:length(tmp)){
          cat(" ", ifelse(is.null(names(tmp[1])), "", paste0(" - ", names(tmp[i]), ": ", sep = "")), tmp[i], sep = "", "\n")
        }
        cat("\n")
      }

      res <- tmp
    }
  } else {
    if (.verbose){
      cat("\n")
      cat(" PubChem Assay Details (name)", "\n\n")
      cat(" Nothing to return. An error has been occurred. See requested assay results for details.", "\n")
      cat("\n")
    }
  }

  invisible(res)
}

#' @export
name.PubChemInstanceList <- function(object, ..., .which = NULL, .verbose = TRUE){
  res <- NULL

  if (is.null(.which)){
    idx <- 1
  } else {
    if (!(.which %in% request_args(object, "identifier"))){
      stop("Unknown instance identifier. Run 'request_args(object, \"identifier\")' to see all the requested instance identifiers.")
    }
    idx <- which(request_args(object, "identifier") == .which)
  }

  name(object$result[[idx]], .verbose = .verbose)
}


#' @export
description <- function(object, ..., .verbose = TRUE){
  UseMethod("description")
}

#' @export
description.PubChemInstance <- function(object, ..., .verbose = TRUE){
  res <- NULL

  if (object$success){
    if ("PC_Assay" %in% class(object)){
      tmp <- object$result$PC_AssayContainer[[1]]$assay$descr$description

      if (.verbose & !is.null(tmp)){
        cat("\n")
        cat(" PubChem Assay Details (description)", "\n\n")
        for (i in 1:length(tmp)){
          cat(" ", ifelse(is.null(names(tmp[1])), "", paste0(" - ", names(tmp[i]), ": ", sep = "")), tmp[i], sep = "", "\n")
        }
        cat("\n")
      }

      res <- tmp
    }
  } else {
    if (.verbose){
      cat("\n")
      cat(" PubChem Assay Details (description)", "\n\n")
      cat(" Nothing to return. An error has been occurred. See requested assay results for details.", "\n")
      cat("\n")
    }
  }

  invisible(res)
}

#' @export
description.PubChemInstanceList <- function(object, ..., .which = NULL, .verbose = TRUE){
  res <- NULL

  if (is.null(.which)){
    idx <- 1
  } else {
    if (!(.which %in% request_args(object, "identifier"))){
      stop("Unknown instance identifier. Run 'request_args(object, \"identifier\")' to see all the requested instance identifiers.")
    }
    idx <- which(request_args(object, "identifier") == .which)
  }

  description(object$result[[idx]], .verbose = .verbose)
}


#' @export
protocol <- function(object, ..., .verbose = TRUE){
  UseMethod("protocol")
}

#' @export
protocol.PubChemInstance <- function(object, ..., .verbose = TRUE){
  res <- NULL

  if (object$success){
    if ("PC_Assay" %in% class(object)){
      tmp <- object$result$PC_AssayContainer[[1]]$assay$descr$protocol

      if (.verbose && !is.null(tmp)){
        cat("\n")
        cat(" PubChem Assay Details (protocol)", "\n\n")
        for (i in 1:length(tmp)){
          cat(" ", ifelse(is.null(names(tmp[1])), "", paste0(" - ", names(tmp[i]), ": ", sep = "")), tmp[i], sep = "", "\n")
        }
        cat("\n")
      }

      res <- tmp
    }
  } else {
    if (.verbose){
      cat("\n")
      cat(" PubChem Assay Details (protocol)", "\n\n")
      cat(" Nothing to return. An error has been occurred. See requested assay results for details.", "\n")
      cat("\n")
    }
  }

  invisible(res)
}

#' @export
protocol.PubChemInstanceList <- function(object, ..., .which = NULL, .verbose = TRUE){
  res <- NULL

  if (is.null(.which)){
    idx <- 1
  } else {
    if (!(.which %in% request_args(object, "identifier"))){
      stop("Unknown instance identifier. Run 'request_args(object, \"identifier\")' to see all the requested instance identifiers.")
    }
    idx <- which(request_args(object, "identifier") == .which)
  }

  protocol(object$result[[idx]], .verbose = .verbose)
}



#' @export
comment <- function(object, ..., .verbose = TRUE){
  UseMethod("comment")
}

#' @export
comment.PubChemInstance <- function(object, ..., .verbose = TRUE){
  res <- NULL

  if (object$success){
    if ("PC_Assay" %in% class(object)){
      tmp <- object$result$PC_AssayContainer[[1]]$assay$descr$comment

      if (.verbose && !is.null(tmp)){
        cat("\n")
        cat(" PubChem Assay Details (comment)", "\n\n")
        for (i in 1:length(tmp)){
          cat(" ", ifelse(is.null(names(tmp[1])), "", paste0(" - ", names(tmp[i]), ": ", sep = "")), tmp[i], sep = "", "\n")
        }
        cat("\n")
      }

      res <- tmp
    }
  } else {
    if (.verbose){
      cat("\n")
      cat(" PubChem Assay Details (comment)", "\n\n")
      cat(" Nothing to return. An error has been occurred. See requested assay results for details.", "\n")
      cat("\n")
    }
  }

  invisible(res)
}

#' @export
comment.PubChemInstanceList <- function(object, ..., .which = NULL, .verbose = TRUE){
  if (is.null(.which)){
    idx <- 1
  } else {
    if (!(.which %in% request_args(object, "identifier"))){
      stop("Unknown instance identifier. Run 'request_args(object, \"identifier\")' to see all the requested instance identifiers.")
    }
    idx <- which(request_args(object, "identifier") == .which)
  }

  comment(object$result[[idx]], .verbose = .verbose)
}


#' @export
xref <- function(object, ..., .verbose = TRUE){
  UseMethod("xref")
}

#' @export
xref.PubChemInstance <- function(object, ..., .verbose = TRUE){
  res <- NULL

  if (object$success){
    if ("PC_Assay" %in% class(object)){
      tmp <- object$result$PC_AssayContainer[[1]]$assay$descr$xref

      if (.verbose && !is.null(tmp)){
        cat("\n")
        cat(" PubChem Assay Details (xref)", "\n\n")
        for (i in 1:length(tmp)){
          slotNames <- names(tmp[[i]])
          for (j in 1:length(tmp[[i]])){
            ref <- tmp[[i]][[j]]
            refName <- names(ref)
            cat(ifelse(j == 1, " > ", "   "), sep = "")
            cat(ifelse(is.null(slotNames[j]), "", paste0(slotNames[j], ifelse(is.null(refName), ": ", paste0(" (", refName, "): ")))), ref, sep = "", "\n")
          }
          cat("\n")
        }
        cat("\n")
      }

      res <- tmp
    }
  } else {
    if (.verbose){
      cat("\n")
      cat(" PubChem Assay Details (xref)", "\n\n")
      cat(" Nothing to return. An error has been occurred. See requested assay results for details.", "\n")
      cat("\n")
    }
  }

  invisible(res)
}

#' @export
xref.PubChemInstanceList <- function(object, ..., .which = NULL, .verbose = TRUE){
  if (is.null(.which)){
    idx <- 1
  } else {
    if (!(.which %in% request_args(object, "identifier"))){
      stop("Unknown instance identifier. Run 'request_args(object, \"identifier\")' to see all the requested instance identifiers.")
    }
    idx <- which(request_args(object, "identifier") == .which)
  }

  xref(object$result[[idx]], .verbose = .verbose)
}

#' @export
results <- function(object, ...){
  UseMethod("results")
}

#' @importFrom dplyr bind_rows bind_cols
#'
#' @export
results.PubChemInstance <- function(object, ..., .to.data.frame = TRUE){
  res <- NULL

  if ("PC_Assay" %in% class(object) & object$success){
    tmp <- object$result$PC_AssayContainer[[1]]$assay$descr$results

    if (.to.data.frame){
      res <- bind_rows(lapply(tmp, bind_cols))
    } else {
      res <- tmp
    }
  }

  return(res)
}

#' @export
results.PubChemInstanceList <- function(object, ..., .to.data.frame = TRUE, .which = NULL){
  if (is.null(.which)){
    idx <- 1
  } else {
    if (!(.which %in% request_args(object, "identifier"))){
      stop("Unknown instance identifier. Run 'request_args(object, \"identifier\")' to see all the requested instance identifiers.")
    }
    idx <- which(request_args(object, "identifier") == .which)
  }

  results(object$result[[idx]], .to.data.frame = .to.data.frame)
}


#' @export
instanceProperties <- function(object, ...){
  UseMethod("instanceProperties")
}

#' @importFrom dplyr bind_rows bind_cols
#'
#' @export
instanceProperties.PubChemInstance <- function(object, ..., .to.data.frame = TRUE){
  res <- NULL

  if ("PC_Properties" %in% class(object) & object$success){
    tmp <- object$result[[1]][[1]][[1]]

    if (.to.data.frame){
      res <- bind_rows(bind_cols(tmp))
    } else {
      res <- tmp
    }
  }

  return(res)
}

#' @importFrom magrittr "%>%"
#' @importFrom dplyr bind_rows
#'
#' @export
instanceProperties.PubChemInstanceList <- function(object, ..., .to.data.frame = TRUE, .which = NULL, .combine.all = FALSE){
  if (!.combine.all){
    if (is.null(.which)){
      idx <- 1
    } else {
      if (!(.which %in% request_args(object, "identifier"))){
        stop("Unknown instance identifier. Run 'request_args(object, \"identifier\")' to see all the requested instance identifiers.")
      }
      idx <- which(request_args(object, "identifier") == .which)
    }

    res <- instanceProperties(object$result[[idx]], .to.data.frame = .to.data.frame)
  } else {
    .to.data.frame <- TRUE

    res <- lapply(request_args(object, "identifier"), function(x){
      tmp <- instance(object, .which = x)
      instanceProperties(tmp, .to.data.frame = .to.data.frame)
    }) %>%
      bind_rows()
  }

  return(res)
}
