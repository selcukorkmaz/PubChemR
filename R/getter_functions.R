# Getter Functions ----

#' @title Retrieve Function Input(s)
#'
#' @param object An object returned from the related request functions of the PubChem database.
#' @param .which A string specifying which argument's content to retrieve from \code{object}. If NULL, all
#' function inputs will be returned.
#' @param ... Additional arguments. These have no effect on the returned outputs and are included for
#' compatibility with S3 methods in the PubChemR package.
#'
#' @return A list or string vector containing the options used in the function call.
#'
#' @examples
#' request <- get_cids("aspirin", namespace = "name")
#'
#' request_args(request, "identifier")
#' request_args(request)
#'
#' @export
request_args <- function(object, .which = NULL, ...){
  if (is.null(.which)){
    return(object[["request_args"]])
  } else {
    return(object[["request_args"]][[.which]])
  }
}

#' @param .which A string specifying which instance's results to return. If NULL, the results of the first instance in
#' the \code{object} are returned. The default is NULL.
#'
#' @rdname instance
#' @name instance
#' @order 2
#'
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

#' @title Retrieve Information for Requested Instances
#'
#' @description
#' This function extracts the results of a PubChem instance from an \code{object}. It is useful for extracting
#' information about a compound from a complete list where multiple elements (assay, compound, etc.) are requested.
#'
#' @param object An object of class \code{'PubChemInstanceList'} returned from a PubChem request.
#' @param ... Additional arguments passed to other methods. Currently, these have no effect.
#'
#' @rdname instance
#' @name instance
#' @order 1
#'
#' @examples
#' compounds <- get_compounds(
#'   identifier = c("aspirin", "ibuprofen"),
#'   namespace = "name"
#' )
#'
#' instance(compounds)  # returns the results for "aspirin"
#' instance(compounds, "ibuprofen")
#'
#' @export
instance <- function(object, ...){
  UseMethod("instance")
}


## Global function for extracting elements. ----

#' @title Retrieve Information from PubChem Instances
#'
#' @description
#' This generic function extracts a specific slot from a PubChem instance.
#'
#' @param object An object returned from a PubChem request.
#' @param ... Additional arguments passed to other methods. Currently, these have no effect.
#'
#' @rdname retrieve
#' @name retrieve
#' @order 1
#'
#' @export
retrieve <- function(object, ...){
  UseMethod("retrieve")
}

#' @param .slot A string specifying which slot to return. Should not be NULL with some exceptions. See notes for details.
#' @param .to.data.frame a logical. If TRUE, returned object will be forced to be converted into a data.frame (or tibble).
#' If failed to convert into a data.frame, a list will be returned with a warning. Be careful for complicated lists
#' (i.e., many elements nested within each other) since it may be time consuming to convert such lists into a data frame.
#' Furthermore, \code{.to.data.frame} is ignored for specific scenarios.
#' @param .verbose a logical. Should the resulting object printed into R Console? If TRUE the object is returned invisibly
#' and the output is nicely printed to R Console. This option may not be available for some slots (or classes).
#' See Notes/Details.
#'
#' @rdname retrieve
#' @order 2
#'
#' @note
#' If object is from \code{'PC_Properties'} class, \code{.slot} definition will be omitted and all the requested
#' properties will be retrieved from \code{object}.
#'
#' \subsection{Use of \code{'.verbose'} argument}
#'
#' \code{retrieve} returns output silently (invisible) when \code{.verbose = TRUE}. However, the function treats differently
#' under following scenarios:
#' \itemize{
#'   \item{\code{.verbose} is ignored if \code{.combine.all = TRUE}. Output is returned silently.}
#'   \item{\code{.verbose} is ignored if requested slot is not printable to R Console because it is too complicated to print.}
#' }
#'
#' @importFrom dplyr bind_cols bind_rows full_join mutate_all
#' @importFrom tibble as_tibble as_tibble_col tibble
#'
#' @examples
#' compounds <- get_compounds(identifier = c(
#'   "aspirin", "ibuprofen", "rstudio"),
#'   namespace = "name"
#'  )
#'
#' # Extract information for "aspirin"
#' aspirin <- instance(compounds, "aspirin")
#' # print(aspirin)
#'
#' # Extract a specific slot from "aspirin" compound.
#' retrieve(aspirin, "props", .to.data.frame = TRUE)
#'
#' @export
retrieve.PubChemInstance <- function(object, .slot = NULL, .to.data.frame = TRUE, .verbose = FALSE, ...){
  dots <- list(...)
  returnInvisible <- FALSE

  if (!object$success){
    warning("'object' encountered an error. Nothing to return. \n See error details in 'object'.")
    return(NULL)
  }

  if (!("PC_Properties" %in% class(object)) & is.null(.slot)){
    warning("Which slot do you want to return? '.slot' is not defined. Returning NULL.")
    return(NULL)
  }

  # Gather all the elements from selected slot. ----
  slotContents <- if ("PC_Compounds" %in% class(object)){
    object$result[[1]][[1]][[.slot]]
  } else if ("PC_Assay" %in% class(object)){
    object$result$PC_AssayContainer[[1]]$assay$descr[[.slot]]
  } else if ("PC_Properties" %in% class(object)){
    object$result[[1]][[1]][[1]]
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

#' @param .combine.all a logical. If TRUE, properties of all requested instances are combined into single
#' data frame (or list if \code{.to.data.frame = FALSE}).
#'
#' @rdname retrieve
#' @order 3
#'
#' @examples
#' # EXAMPLES (PubChemInstanceList)
#' retrieve(compounds, "aspirin", "props", .to.data.frame = TRUE)
#'
#' # Verbose Assay References to R Console
#' assays <- get_assays(identifier = c(1234, 7815), namespace = "aid")
#'
#' instance(assays, "7815")
#' retrieve(assays, "7815", "xref", .verbose = TRUE)
#'
#' # Print assay protocol to R Console (if available)
#' # Note that it may be too long to print for some assays.
#' # retrieve(assays, "1234", "protocol", .verbose = TRUE)
#'
#' # No protocol is available for assay "1234".
#' # retrieve(assays, "7815", "protocol", .verbose = TRUE)
#'
#' # Ignores ".verbose" and ".which" if ".combine.all = TRUE".
#' retrieve(assays, .slot = "xref", .verbose = TRUE, .combine.all = TRUE)
#'
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#'
#' @export
retrieve.PubChemInstanceList <- function(object, .which = NULL, .slot = NULL, .to.data.frame = TRUE,
                                         .combine.all = FALSE, ...){

  dots <- list(...)
  args <- c(list(.slot = .slot, .to.data.frame = .to.data.frame,
                 .combine.all = .combine.all), dots)

  if (!.combine.all){
    if (is.null(.which)){
      idx <- 1
    } else {
      if (length(.which) > 1){
        .which <- .which[1]
        warning("Multiple instances is not allowed in '.which'. Only the first instance is returned.")
      }

      if (!(.which %in% request_args(object, "identifier"))){
        stop("Unknown instance identifier. Run 'request_args(object, \"identifier\")' to see all the requested instance identifiers.")
      }
      idx <- which(request_args(object, "identifier") == .which)
    }

    args$object <- object$result[[idx]]
    res <- do.call("retrieve", args)
  } else {
    args[[".verbose"]] <- FALSE
    res <- suppressMessages({
      lapply(request_args(object, "identifier"), function(x){
        tmp <- instance(object, .which = x)
        args$object <- tmp
        success <- try({
          tmp2 <- do.call("retrieve", args)
        })

        if (inherits(success, "try-error")){
          return(NULL)
        } else {
          if (args$.to.data.frame){
            tmp2 <- bind_cols(tibble(Identifier = x), tmp2)
          } else {
            tmp2 <- c(list(Identifier = x), tmp2)
          }
        }

        return(tmp2)
      })
    })

    if (args$.to.data.frame){
      res <- bind_rows(res)
    }
  }

  if (args[[".verbose"]]){
    invisible(res)
  } else {
    return(res)
  }
}

#' @param .idx an integer. Which substance results should be returned? PubChem request may return multiple
#' substances in the output. \code{.idx} is the index of returned substance to be extracted from complete list.
#'
#' @rdname retrieve
#' @order 4
#'
#' @importFrom tibble as_tibble_row as_tibble_col tibble
#' @importFrom dplyr mutate_all bind_rows bind_cols
#'
#' @export
retrieve.PC_Substance <- function(object, .slot = NULL, .idx = 1, .to.data.frame = TRUE, .verbose = FALSE, ...){

  returnInvisible <- FALSE

  if (!object$success){
    warning("'object' encountered an error. Nothing to return. \n See error details in 'object'.")
    return(NULL)
  }

  if (!("PC_Substance" %in% class(object)) & is.null(.slot)){
    warning("Which slot do you want to return? '.slot' is not defined. Returning NULL.")
    return(NULL)
  }

  .idx <- ifelse(is.null(.idx), 1, .idx)

  slotContents <- if ("PC_Substance" %in% class(object)){
    object$result$PC_Substances[[.idx]][[.slot]]
  }

  # Walk through next layer of slotContents list, if it is, until the last layer.
  slotContents <- find_last_layer(slotContents)

  if (is.null(slotContents)){
    return(NULL)
  }

  # If the structure of "slotContents" is a "vector"
  vectorSlot <- !any(is.list(slotContents), is.matrix(slotContents),
                     is.data.frame(slotContents), is.array(slotContents))

  successDF <- TRUE
  if (.to.data.frame){
    successDF <- try({
      if (!vectorSlot){
        if (.slot == "xref"){
          resDF <- suppressMessages({
            lapply(slotContents, function(x){
              tibble(source = names(x), value = x) %>%
                mutate_all(as.character)
            }) %>%
              bind_rows
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
          as_tibble_row(slotContents)
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
    cat("\n")
    if (.slot %in% c("comment")){
      cat(" PubChem Substance Details (", .slot, ")", "\n\n", sep = "")

      for (i in 1:length(slotContents)){
        cat(" ", ifelse(is.null(names(slotContents[1])), "", paste0(" - ", names(slotContents[i]), ": ", sep = "")), slotContents[i], sep = "", "\n")
      }
      returnInvisible <- TRUE
    }

    if (.slot == "xref"){
      cat(" PubChem Substance Details (", .slot, ")", "\n\n", sep = "")
      for (i in 1:length(slotContents)){
        slotName <- names(slotContents[[i]])

        cat(" > Source: ", slotName, sep = "", "\n")
        cat("    Value: ", slotContents[[i]], sep = "", "\n")
        cat("\n")
      }
      returnInvisible <- TRUE
    }

    cat("\n")
  }

  if (returnInvisible){
    invisible(resDF)
  } else {
    return(resDF)
  }
}

#' @rdname retrieve
#' @order 5
#'
#' @section Details on \code{'PugViewInstance'} and \code{'PugViewSection'}:
#' Pug View API returns a detailed list about PubChem request. The 'Section' slot in this list is structured into
#' a sub-class \code{'PugViewSection'}. This object contains information through several sections (or sub-sections),
#' which can be retrieved using \emph{section-specific} functions such as \link{section} and \link{sectionList}.
#'
#' The function argument \code{.to.data.frame} is ignored if "Section" slot is being extracted from complete list.
#' For other slots, \code{.to.data.frame} is considered as usual. See examples.
#'
#' @examples
#' ### PUG VIEW EXAMPLES ####
#' pview <- get_pug_view(identifier = "2244", annotation = "data", domain = "compound")
#'
#' # PugViewSectionList object.
#' # This object contains all the section information about the PubChem request.
#' sect <- retrieve(pview, .slot = "Section")
#' print(sect)
#'
#' retrieve(pview, .slot = "RecordType", .to.data.frame = TRUE)
#'
#' @importFrom dplyr bind_cols bind_rows full_join mutate_all
#' @importFrom tibble as_tibble as_tibble_col tibble
#'
#' @export
retrieve.PugViewInstance <- function(object, .slot = NULL, .to.data.frame = TRUE, ...){

  dots <- list(...)

  if (!object$success){
    warning("'object' encountered an error. Nothing to return. \n See error details in 'object'.")
    return(NULL)
  }

  if (is.null(.slot)){
    warning("Which slot do you want to return? '.slot' is not defined. Returning NULL.")
    return(NULL)
  }

  slotContents <- if (request_args(object, "annotation") == "data"){
    object$result[[1]][[.slot]]
  } else {
    object$result[[1]][[.slot]]
  }

  # Walk through next layer of slotContents list, if it is, until the last layer.
  slotContents <- find_last_layer(slotContents)

  if (is.null(slotContents)){
    return(NULL)
  }

  # If there is one section in the slotCotents, we move it into a 1-element list to be compatible with the
  # structure of PugViewSectionList class.
  if ("TOCHeading" %in% names(slotContents)){
    slotContents <- list(slotContents)
  }

  if (.slot %in% c("Section", "Reference")){
    tmpList <- structure(
      list(
        result = slotContents,
        recordInformation = list(
          RecordType = object$result[[1]][["RecordType"]],
          RecordNumber = object$result[[1]][["RecordNumber"]],
          RecordTitle = object$result[[1]][["RecordTitle"]]
        ),
        success = TRUE,
        error = NULL
      )
    )

    if (.slot == "Section"){
      class(tmpList) <- c("PugViewSectionList")
    } else if (.slot == "Reference"){
      class(tmpList) <- c("PugViewReferenceList")
    }

    return(tmpList)
  }

  # If the structure of "slotContents" is a "vector"
  vectorSlot <- !any(is.list(slotContents), is.matrix(slotContents),
                     is.data.frame(slotContents), is.array(slotContents))

  successDF <- TRUE
  if (.to.data.frame){
    successDF <- try({
      if (!vectorSlot){
        resDF <- bind_cols(slotContents)
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

  return(resDF)
}


#' @rdname retrieve
#' @order 6
#'
#' @importFrom dplyr bind_cols
#' @importFrom tibble as_tibble_col tibble
#'
#' @export
retrieve.PugViewSection <- function(object, .slot = NULL, .to.data.frame = FALSE, ...){
  dots <- list(...)

  if (!object$success){
    warning("'object' encountered an error. Nothing to return. \n See error details in 'object'.")
    return(NULL)
  }

  if (is.null(.slot)){
    warning("Which slot do you want to return? '.slot' is not defined. Returning NULL.")
    return(NULL)
  }

  slotContents <- object$result[[.slot]]

  # Walk through next layer of slotContents list, if it is, until the last layer.
  slotContents <- find_last_layer(slotContents)

  if (is.null(slotContents)){
    return(NULL)
  }

  # If the structure of "slotContents" is a "vector"
  vectorSlot <- !any(is.list(slotContents), is.matrix(slotContents),
                     is.data.frame(slotContents), is.array(slotContents))

  successDF <- TRUE
  if (.to.data.frame){
    successDF <- try({
      if (!vectorSlot){
        resDF <- bind_cols(slotContents)
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

  return(resDF)
}



# PubChemInstance_AIDs ----
#' @param .to.data.frame a logical. If TRUE, returned object will be forced to be converted into a data.frame (or tibble).
#' If failed to convert into a data.frame, a list will be returned with a warning. Be careful for complicated lists
#' (i.e., many elements nested within each other) since it may be time consuming to convert such lists into a data frame.
#'
#' @rdname AIDs-SIDs-CIDs
#'
#' @export
AIDs.PubChemInstance_AIDs <- function(object, .to.data.frame = TRUE, ...) {
  tmp <- object$result
  nms = toupper(request_args(object, "namespace"))

  if (.to.data.frame) {
    res <- lapply(tmp, function(x) {
      if (!x$success) {
        return(NULL)
      }

      if (nms == "FORMULA") {
        tmp2 <- tibble(CID = integer(), AID = integer())

        for (info in x$result$InformationList$Information) {
          if (is.list(info) && !is.null(info$AID)) {
            for (aid in info$AID) {
              tmp2 <- add_row(result_tibble,
                              CID = info$CID,
                              AID = aid)
            }
          }
        }
      } else {
        tmp2 <- bind_cols(x$result$InformationList$Information)
      }

      if (colnames(tmp2)[1] != nms) {
        tbl <- tibble(request_args(x, .which = "identifier"))
        names(tbl) <- toupper(stringr::str_to_title(request_args(x, .which = "namespace")))
        tmp2 <- bind_cols(tbl, tmp2)
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

#' @title Assay, Compound, and Substance IDs
#'
#' @description
#' These functions are used to get ID information of assays, substances, and compounds.
#'
#' @param object An object returned from a PubChem request, mainly from functions \link{get_cids}, \link{get_aids},
#' and \link{get_sids}.
#' @param ... Additional arguments passed to other methods. Currently, these have no effect.
#'
#' @rdname AIDs-SIDs-CIDs
#' @name AIDs-SIDs-CIDs
#' @order 1
#'
#' @examples
#' # Assay IDs
#' aids <- get_aids(identifier = c("aspirin", "caffein"), namespace = "name")
#' AIDs(aids)
#'
#' @export
AIDs <- function(object, ...){
  UseMethod("AIDs")
}

# PubChemInstance_CIDs ----
#' @rdname AIDs-SIDs-CIDs
#'
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

#' @rdname AIDs-SIDs-CIDs
#' @order 2
#'
#' @examples
#' # Compound IDs
#' cids <- get_cids(identifier = c("aspirin", "caffein"), namespace = "name")
#' CIDs(cids)
#'
#' @export
CIDs <- function(object, ...){
  UseMethod("CIDs")
}


# PubChemInstance_SIDs ----
#' @rdname AIDs-SIDs-CIDs
#'
#' @importFrom dplyr bind_rows
#' @importFrom tidyr as_tibble
#'
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

#' @rdname AIDs-SIDs-CIDs
#' @order 3
#'
#' @examples
#' # Substance IDs
#' sids <- get_sids(identifier = c("aspirin", "caffein"), namespace = "name")
#' SIDs(sids)
#'
#' @export
SIDs <- function(object, ...){
  UseMethod("SIDs")
}

# PubChemInstance_Synonyms ----
#' @param .to.data.frame a logical. If TRUE, returned object will be forced to be converted into a data.frame (or tibble).
#' If failed to convert into a data.frame, a list will be returned with a warning. Be careful for complicated lists
#' (i.e., many elements nested within each other) since it may be time consuming to convert such lists into a data frame.
#'
#' @rdname synonyms
#'
#' @importFrom dplyr bind_rows
#' @importFrom tidyr as_tibble
#'
#' @export
synonyms.PubChemInstance_Synonyms <- function(object, .to.data.frame = TRUE, ...){
  tmp <- object$result

  if (.to.data.frame){
    res <- lapply(tmp, function(x){
      xx <- suppressMessages({
        list(x$request_args$identifier, Synonyms = x$result$InformationList$Information[[1]]$Synonym) %>%
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

#' @title Getter function of 'Synonyms'
#'
#' @description
#' Extract synonyms data from PubChem request using the function \link{get_synonyms}.
#'
#' @param object an object of class \code{'PubChemInstance_Synonyms'}.
#' @param ... Additional arguments passed to other methods. Currently, these have no effect.
#'
#' @rdname synonyms
#' @name synonyms
#' @order 1
#'
#' @return a data.frame (or list) object containing the synonym data.
#'
#' @examples
#' syns <- get_synonyms(identifier = c("aspirin", "caffein"), namespace = "name")
#' synonyms(syns)
#'
#' @export
synonyms <- function(object, ...){
  UseMethod("synonyms")
}



# Sections ----
#' @export
section <- function(object, ...){
  UseMethod("section")
}

#' @export
section.PugViewInstance <- function(object, .id = "S1", .verbose = FALSE, ...){
  dots <- list(...)
  call_args <- c(list(object = retrieve(object, .slot = "Section"), .id = .id, .verbose = .verbose), dots)
  do.call("section", call_args)
}

#' @export
section.PugViewSectionList <- function(object, .id = "S1", .verbose = FALSE, ...){
  if (!object$success){
    warning("'object' encountered an error. Nothing to return. \n See error details in 'object'.")
    return(NULL)
  }

  if (is.null(.id)){
    .id <- "S1"
  }

  sectionInfo <- sectionList(object)

  if (is.null(sectionInfo)){
    warning("There is no section data within 'object'. Returning NULL.")
    return(NULL)
  }

  # If requested section ID is not available.
  if (!is.null(nrow(sectionInfo))){
    if (!(.id %in% sectionInfo$SectionID)) {
      stop("Unknown section ID (.id). Please check available sections and use correct section ID.")
    }
  }

  idx <- which(sectionInfo[["SectionID"]] == .id)
  sectionContents <- object$result[[idx]]

  tmpList <- structure(
    list(
      result = sectionContents,
      recordInformation = object$recordInformation,
      success = TRUE,
      error = NULL
    ),
    class = "PugViewSection"
  )

  if (.verbose){
    printSectionDetails(sectionContents)
    invisible(tmpList)
  } else {
    return(tmpList)
  }
}

#' @export
section.PugViewSection <- function(object, .id = "S1", .verbose = FALSE, ...){
  if (!object$success){
    warning("'object' encountered an error. Nothing to return. \n See error details in 'object'.")
    return(NULL)
  }

  if (is.null(.id)){
    .id <- "S1"
  }

  sectionInfo <- sectionList(object)

  if (is.null(sectionInfo)){
    warning("There is no section data within 'object'. Returning NULL.")
    return(NULL)
  }

  # If requested section ID is not available.
  if (!is.null(nrow(sectionInfo))){
    if (!(.id %in% sectionInfo$SectionID)) {
      stop("Unknown section ID (.id). Please check available sections and use correct section ID.")
    }
  }

  idx <- which(sectionInfo[["SectionID"]] == .id)
  sectionContents <- object$result$Section[[idx]]

  tmpList <- structure(
    list(
      result = sectionContents,
      recordInformation = object$recordInformation,
      success = TRUE,
      error = NULL
    ),
    class = "PugViewSection"
  )

  if (.verbose){
    printSectionDetails(sectionContents)
    invisible(tmpList)
  } else {
    return(tmpList)
  }
}


#' @export
sectionList <- function(object, ...){
  UseMethod("sectionList")
}

#' @export
sectionList.PugViewInstance <- function(object, ...){
  dots <- list(...)
  call_args <- c(list(object = retrieve(object, .slot = "Section")), dots)
  do.call("sectionList", call_args)
}

#' @importFrom tibble tibble
#' @importFrom tidyr ends_with starts_with contains
#' @export
sectionList.PugViewSectionList <- function(object, .pattern = NULL, .match_type = c("contain", "match", "start", "end"), ...){

  .match_type <- match.arg(.match_type)
  sectionList <- object$result

  if (is.null(sectionList) | length(sectionList) == 0){
    warning("No section data available. Returning NULL.")
    return(NULL)
  }

  sectionHeadings <- unlist(lapply(sectionList, "[[", "TOCHeading"))
  if (length(sectionHeadings) > 0){
    resDF <- tibble(SectionID = paste0("S", 1:length(sectionHeadings)), Headings = sectionHeadings)
  }

  # Filter sections using ".pattern" and ".match_type"
  filteredSections <- NULL
  if (!is.null(.pattern)){
    if (!is.character(.pattern)){
      stop("Match pattern (.pattern) should be 'character' type.")
    }

    filteredSections <- sapply(.pattern, function(x){
      idx <- if (.match_type == "start"){
        starts_with(match = x, vars = sectionHeadings, ignore.case = TRUE)
      } else if (.match_type == "end"){
        ends_with(match = x, vars = sectionHeadings, ignore.case = TRUE)
      } else if (.match_type == "match"){
        x <- tolower(x)
        which(x == tolower(sectionHeadings))
      } else {
        contains(match = x, vars = sectionHeadings, ignore.case = TRUE)
      }

      if (length(idx) == 0){
        return(NULL)
      }

      return(idx)
    }, simplify = FALSE)

    filteredSections <- sort(unique(unlist(filteredSections)))

    if (!is.null(filteredSections)){
      return(resDF[filteredSections, ])
    } else {
      warning("No section with given criteria has been found. Returning NULL.")
      return(NULL)
    }
  }

  return(resDF)
}


#' @export
sectionList.PugViewSection <- function(object, .pattern = NULL, .match_type = c("contain", "match", "start", "end"), ...){

  .match_type <- match.arg(.match_type)
  sectionList <- object$result

  if (is.null(sectionList) | length(sectionList) == 0){
    warning("No section data available. Returning NULL.")
    return(NULL)
  }

  if (!("Section" %in% names(sectionList))){
    warning("There is no section data within 'object'. Returning NULL.")
    return(NULL)
  }

  sectionHeadings <- unlist(lapply(sectionList$Section, "[[", "TOCHeading"))
  if (length(sectionHeadings) > 0){
    resDF <- tibble(SectionID = paste0("S", 1:length(sectionHeadings)), Headings = sectionHeadings)
  }

  # Filter sections using ".pattern" and ".match_type"
  filteredSections <- NULL
  if (!is.null(.pattern)){
    if (!is.character(.pattern)){
      stop("Match pattern (.pattern) should be 'character' type.")
    }

    filteredSections <- sapply(.pattern, function(x){
      idx <- if (.match_type == "start"){
        starts_with(match = x, vars = sectionHeadings, ignore.case = TRUE)
      } else if (.match_type == "end"){
        ends_with(match = x, vars = sectionHeadings, ignore.case = TRUE)
      } else if (.match_type == "match"){
        x <- tolower(x)
        which(x == tolower(sectionHeadings))
      } else {
        contains(match = x, vars = sectionHeadings, ignore.case = TRUE)
      }

      if (length(idx) == 0){
        return(NULL)
      }

      return(idx)
    }, simplify = FALSE)

    filteredSections <- sort(unique(unlist(filteredSections)))

    if (!is.null(filteredSections)){
      return(resDF[filteredSections, ])
    } else {
      warning("No section with given criteria has been found. Returning NULL.")
      return(NULL)
    }
  }

  return(resDF)
}

