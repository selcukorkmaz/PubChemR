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
    warning("'object' encountered an error. Nothing to return. \n See error details in 'object'.")
    return(NULL)
  }

  if (is.null(.slot)){
    .slot <- ""
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

#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
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

  return(res)
}

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

  .slot <- ifelse(is.null(.slot), "", .slot)
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
    .slot <- ""
  }

  slotContents <- if (request_args(object, "annotation") == "data"){
    object$result[[1]][[.slot]]
  }

  # Walk through next layer of slotContents list, if it is, until the last layer.
  slotContents <- find_last_layer(slotContents)

  if (is.null(slotContents)){
    return(NULL)
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


#' @importFrom dplyr bind_cols
#' @importFrom tibble as_tibble_col tibble
#'
#' @export
retrieve.PugViewSection <- function(object, .slot = NULL, .verbose = FALSE, .to.data.frame = FALSE, ...){
  dots <- list(...)

  if (!object$success){
    warning("'object' encountered an error. Nothing to return. \n See error details in 'object'.")
    return(NULL)
  }

  if (is.null(.slot)){
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

# PubChemInstance_Synonyms ----
#' @importFrom dplyr bind_rows
#' @importFrom tidyr as_tibble
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

#' @export
synonyms <- function(object, ...){
  UseMethod("synonyms")
}




#' @export
section <- function(object, ...){
  UseMethod("section")
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
  idx <- which(sectionInfo[["SectionID"]] == .id)

  tmpList <- structure(
    list(
      result = object$result[[idx]],
      recordInformation = object$recordInformation,
      success = TRUE,
      error = NULL
    ),
    class = "PugViewSection"
  )

  return(tmpList)
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

  idx <- which(sectionInfo[["SectionID"]] == .id)

  tmpList <- structure(
    list(
      result = object$result$Section[[idx]],
      recordInformation = object$recordInformation,
      success = TRUE,
      error = NULL
    ),
    class = "PugViewSection"
  )

  return(tmpList)
}



#' @export
sectionList <- function(object, ...){
  UseMethod("sectionList")
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

    filteredSections <- sapply(.pattern, function(xx){
      idx <- if (.match_type == "start"){
        starts_with(match = xx, vars = sectionHeadings, ignore.case = TRUE)
      } else if (.match_type == "end"){
        ends_with(match = xx, vars = sectionHeadings, ignore.case = TRUE)
      } else if (.match_type == "match"){
        x <- tolower(xx)
        which(xx == tolower(sectionHeadings))
      } else {
        contains(match = xx, vars = sectionHeadings, ignore.case = TRUE)
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

    filteredSections <- sapply(.pattern, function(xx){
      idx <- if (.match_type == "start"){
        starts_with(match = xx, vars = sectionHeadings, ignore.case = TRUE)
      } else if (.match_type == "end"){
        ends_with(match = xx, vars = sectionHeadings, ignore.case = TRUE)
      } else if (.match_type == "match"){
        x <- tolower(xx)
        which(xx == tolower(sectionHeadings))
      } else {
        contains(match = xx, vars = sectionHeadings, ignore.case = TRUE)
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

