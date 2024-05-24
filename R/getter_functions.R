# Getter Functions ----

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


## Global function for extracting elements. ----

#' @export
retrieve <- function(object, ...){
  UseMethod("retrieve")
}

#' @importFrom dplyr bind_cols bind_rows full_join mutate_all
#' @importFrom tibble as_tibble as_tibble_col tibble
#'
#' @export
retrieve.PubChemInstance <- function(object, .slot = NULL, .to.data.frame = TRUE, .verbose = FALSE, ...){

  dots <- list(...)
  returnInvisible <- FALSE

  if (!object$success){
    return(stop("'object' encountered an error. Nothing to return. \n See error details in 'object'."))
  }

  # Gather all the elements from selected slot. ----
  slotContents <- if ("PC_Compounds" %in% class(object)){
    object$result[[1]][[1]][[.slot]]
  } else if ("PC_Assay" %in% class(object)){
    object$result$PC_AssayContainer[[1]]$assay$descr[[.slot]]
  }

  # Walk through next layer of slotContents list, if it is, until the last layer.
  slotContents <- find_last_layer(slotContents)

  if (is.null(slotContents)){
    return(NULL)
  }

  # If the structure of "slotContents" is a "vector"
  vectorSlot <- !any(is.list(slotContents), is.matrix(slotContents),
                     is.data.frame(slotContents), is.array(slotContents))

  # Convert into data.frame if possible. ----
  successDF <- TRUE
  if (.to.data.frame){
    successDF <- try({
      if (!vectorSlot){
        if (.slot == "props" & ("PC_Compounds" %in% class(object))){
          slotContents <- lapply(slotContents, function(x){
            as.data.frame(as.matrix(bind_cols(x)))
          })

          resDF <- slotContents[[1]]
          for (i in 2:length(slotContents)){
            resDF <- suppressMessages(full_join(resDF, slotContents[[i]])) %>%
              as_tibble
          }
        } else if (.slot == "xref" & ("PC_Assay" %in% class(object))){
          resDF <- suppressMessages({
            lapply(slotContents, function(x){
              tibble(source = names(x[[.slot]])) %>%
                bind_cols(x) %>%
                mutate_all(as.character)
            }) %>%
              bind_rows
          })

        } else if (.slot == "results" & ("PC_Assay" %in% class(object))){
          resDF <- suppressMessages({
            lapply(slotContents, function(x){
              bind_cols(x) %>%
                mutate_all(as.character)
            }) %>% bind_rows
          })

        } else {
          resDF <- bind_cols(slotContents)
        }
      } else {
        slotNames <- names(slotContents)

        # If vector slot has names, it will be structured as two column tibble_df, with names in
        # the first column and values in the second column. Otherwise, a column tibbled_df will be
        # returned with values only.
        resDF <- if (is.null(slotNames)){
          as_tibble_col(slotContents)
        } else {
          tibble(Name = slotNames, Value = slotContents)
        }
      }

      TRUE
    })
  }

  if (!.to.data.frame | inherits(successDF, "try-error")){
    resDF <- slotContents
  }

  # Some slots may have long texts including the protocol, description,
  # references, etc. about the PubChem instances. Such information will be
  # printed to R console if '.verbose = TRUE'.

  if (.verbose){
    if ("PC_Assay" %in% class(object)){
      cat("\n")
      if (.slot %in% c("description", "protocol", "comment")){
        cat(" PubChem Assay Details (", .slot, ")", "\n\n", sep = "")

        for (i in 1:length(slotContents)){
          cat(" ", ifelse(is.null(names(slotContents[1])), "", paste0(" - ", names(slotContents[i]), ": ", sep = "")), slotContents[i], sep = "", "\n")
        }
        returnInvisible <- TRUE
      }

      if (.slot == "xref"){
        cat(" PubChem Assay Details (", .slot, ")", "\n\n", sep = "")
        for (i in 1:length(slotContents)){
          slotNames <- names(slotContents[[i]])
          for (j in 1:length(slotContents[[i]])){
            ref <- slotContents[[i]][[j]]
            refName <- names(ref)
            cat(ifelse(j == 1, " > ", "   "), sep = "")
            cat(ifelse(is.null(slotNames[j]), "", paste0(slotNames[j], ifelse(is.null(refName), ": ", paste0(" (", refName, "): ")))), ref, sep = "", "\n")
          }
          cat("\n")
        }
        returnInvisible <- TRUE
      }
    }

    cat("\n")
  }

  if (returnInvisible){
    invisible(resDF)
  } else {
    return(resDF)
  }
}

#' @export
retrieve.PubChemInstanceList <- function(object, .which = NULL, .slot = NULL, .to.data.frame = TRUE,
                                         .combine.all = FALSE, ...){
  if (is.null(.which)){
    idx <- 1
  } else {
    if (!(.which %in% request_args(object, "identifier"))){
      stop("Unknown instance identifier. Run 'request_args(object, \"identifier\")' to see all the requested instance identifiers.")
    }
    idx <- which(request_args(object, "identifier") == .which)
  }

  dots <- list(...)
  args <- c(list(object = object$result[[idx]], .slot = .slot, .to.data.frame = .to.data.frame, .combine.all = .combine.all), dots)

  do.call("retrieve", args)
  # extract(object = object$result[[idx]], .slot = .slot, .to.data.frame = .to.data.frame, .combine.all = .combine.all, ...)
}

# PubChemInstance_AIDs ----
#' @export
AIDs.PubChemInstance_AIDs <- function(object, .to.data.frame = TRUE, ...){
  tmp <- object$result

  if (.to.data.frame){
    res <- lapply(tmp, function(x){
      if (!x$success){
        return(NULL)
      }

      tmp2 <- bind_cols(x$result$InformationList$Information)

      if (request_args(x, "namespace") != "cid"){
        tbl <- tibble(request_args(x, .which = "identifier"))
        names(tbl) <- stringr::str_to_title(request_args(x, .which = "namespace"))
        tmp2 <- bind_cols(tbl, tmp2) %>%
          select(-CID)
      }

      return(tmp2)
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


# get_properties(...) ----
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





