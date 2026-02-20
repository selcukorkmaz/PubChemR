# Getter Functions ----

#' @title Retrieve Function Inputs
#'
#' @description
#' This function retrieves the input arguments from a specified PubChem database request object.
#'
#' @param object An object returned from related request functions of the PubChem database.
#' @param .which A string specifying which argument's content to retrieve from \code{object}. If \code{NULL}, all
#' function inputs will be returned.
#' @param ... Additional arguments. These have no effect on the returned outputs and are included for
#' compatibility with S3 methods in the PubChemR package.
#'
#' @return A list or string vector containing the options used in the function call.
#'
#' @examples
#' \donttest{
#' request <- get_cids("aspirin", namespace = "name")
#'
#' request_args(request, "identifier")
#' request_args(request)
#' }
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
#' the \code{object} are returned. The default value is NULL.
#'
#' @rdname instance
#' @order 2
#'
#' @export
instance.PubChemInstanceList <- function(object, .which = NULL, ...){
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
#' This function extracts the results of a PubChem instance from an \code{object}. It is designed to retrieve
#' information about a compound from a comprehensive list where multiple elements (such as assay, compound, etc.) are requested.
#'
#' @param object An object of class \code{'PubChemInstanceList'} returned from a PubChem request.
#' @param ... Additional arguments passed to other methods. Currently, these have no effect.
#'
#' @rdname instance
#' @name instance
#' @order 1
#'
#' @examples
#' \donttest{
#' compounds <- get_compounds(
#'   identifier = c("aspirin", "ibuprofen"),
#'   namespace = "name"
#' )
#'
#' instance(compounds)  # Returns the results for "aspirin"
#' instance(compounds, "ibuprofen")
#'}
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
#'
#' @rdname retrieve
#' @name retrieve
#' @order 1
#'
#' @export
retrieve <- function(object, ...){
  if (is.null(object)) {
    warning("The object passed to 'retrieve' is NULL. Please provide a valid object of the expected class.")
  }else{
    UseMethod("retrieve")
  }
}

#' @param .slot A string specifying which slot to return. Should not be NULL or length of >1 with some exceptions. See the notes for details.
#' @param .to.data.frame A logical value. If TRUE, the returned object will be converted into a data.frame (or tibble).
#' If conversion to a data.frame fails, a list will be returned with a warning. Be cautious with complex lists
#' (i.e., many elements nested within each other) as it may be time-consuming to convert such lists into a data frame.
#' Additionally, \code{.to.data.frame} is ignored in specific scenarios.
#' @param .verbose A logical value. Should the resulting object be printed to the R console? If TRUE, the object is returned invisibly
#' and the output is printed nicely to the R console. This option may not be available for some slots (or classes).
#' See Notes/Details.
#'
#' @rdname retrieve
#' @order 2
#'
#' @note
#' If the object is from the \code{'PC_Properties'} class, the \code{.slot} can be defined as NULL. If \code{.slot = NULL}, \code{retrieve()} will return all available properties. If \code{'object'} is of class other than \code{'PC_Properties'}, \code{.slot} should be length of 1.
#'
#' \subsection{Extracting multiple slots.}{
#' In some cases, it may be practical to extract multiple slots from \code{'object'}. For example, one may wish to extract properties from the output of \link{get_properties} by running the functions in a loop. See codes below for a practical example:
#'
#' \preformatted{
#'   library(dplyr)
#'
#'   props <- get_properties(
#'     properties = c("MolecularWeight", "MolecularFormula", "HBondDonorCount",
#'                    "HBondAcceptorCount", "InChIKey", "InChI"),
#'     identifier = 2244,
#'     namespace = "cid",
#'     propertyMatch = list(
#'       .ignore.case = TRUE,
#'       type = "contain"
#'     )
#'   )
#'
#'   bind_columns <- function(x, ...){
#'     part1 <- x[[1]][ ,"Identifier"]
#'     part2 <- lapply(x, "[", 2) %>%
#'       bind_cols()
#'
#'     bind_cols(part1, part2)
#'   }
#'
#'   propsToExtract <- c("MolecularWeight", "MolecularFormula", "HBondDonorCount")
#'   tmp <- lapply(propsToExtract, retrieve, object = props, .which = "2244")
#'   bind_columns(tmp)
#' }}
#'
#'
#' \subsection{Use of the \code{'.verbose'} argument}{
#' \code{retrieve} returns output silently (invisibly) when \code{.verbose = TRUE}. However, the function behaves differently
#' under the following scenarios:
#' \itemize{
#'   \item{\code{.verbose} is ignored if \code{.combine.all = TRUE}. The output is returned silently.}
#'   \item{\code{.verbose} is ignored if the requested slot is not printable to the R console because it is too complicated to print.}
#' }
#' }
#'
#' @importFrom dplyr bind_cols bind_rows full_join mutate_all
#' @importFrom tibble as_tibble as_tibble_col tibble
#' @importFrom magrittr '%>%'
#'
#' @examples
#' \donttest{
#' compounds <- get_compounds(
#'   identifier = c("aspirin", "ibuprofen", "rstudio"),
#'   namespace = "name"
#'  )
#'
#' # Extract information for "aspirin"
#' aspirin <- instance(compounds, "aspirin")
#' # print(aspirin)
#'
#' # Extract a specific slot from the "aspirin" compound.
#' retrieve(aspirin, "props", .to.data.frame = TRUE)
#'}
#' @export
retrieve.PubChemInstance <- function(object, .slot = NULL, .to.data.frame = TRUE, .verbose = FALSE, ...){
  dots <- list(...)
  returnInvisible <- FALSE

  if (!object$success){
    warning("'object' encountered an error. Nothing to return. \n See error details in 'object'.")
    return(NULL)
  }

  if (is.null(.slot)){
    if (!("PC_Properties" %in% class(object))){
      warning("Which slot do you want to return? '.slot' is not defined. Returning NULL.")
      return(NULL)
    }
  } else {
    # .slot should be length of 1.
    if (length(.slot) > 1){
      warning("'.slot' should be length of 1. Only the first element of '.slot' is used.")
      .slot <- .slot[1]
    }
  }

  # Gather all the elements from selected slot. ----
  slotContents <- if ("PC_Compounds" %in% class(object)){
    object$result[[1]][[1]][[.slot]]
  } else if ("PC_Assay" %in% class(object)){
    object$result$PC_AssayContainer[[1]]$assay$descr[[.slot]]
  } else if ("PC_Properties" %in% class(object)){
    if (is.null(.slot)){
      object$result[[1]][[1]][[1]]
    } else {
      object$result[[1]][[1]][[1]][[.slot]]
    }
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
        if ("PC_Compounds" %in% class(object)){
          if (.slot == "props"){
            slotContents <- lapply(slotContents, function(x){
              as.data.frame(as.matrix(bind_cols(x)))
            })

            resDF <- slotContents[[1]]
            for (i in 2:length(slotContents)){
              resDF <- suppressMessages(full_join(resDF, slotContents[[i]])) %>%
                as_tibble
            }
          } else {
            resDF <- bind_cols(slotContents)
          }
        } else if ("PC_Assay" %in% class(object)){
          if (.slot == "xref"){
            resDF <- suppressMessages({
              lapply(slotContents, function(x){
                tibble(source = names(x[[.slot]])) %>%
                  bind_cols(x) %>%
                  mutate_all(as.character)
              }) %>%
                bind_rows
            })
          } else if (.slot == "results"){
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
          resDF <- bind_cols(slotContents)
        }

      } else {
        slotNames <- names(slotContents)

        # If vector slot has names, it will be structured as two column tibble_df, with names in
        # the first column and values in the second column. Otherwise, a column tibbled_df will be
        # returned with values only.
        resDF <- if (is.null(slotNames)){
          cName <- ifelse(is.null(.slot), "Value", .slot)
          as_tibble_col(slotContents, column_name = cName)
        } else {
          tibble(Name = slotNames, Value = slotContents)
        }
      }

      TRUE
    })
  }

  # Bind identifier info to returned list or data.frame
  if (.to.data.frame & !inherits(successDF, "try-error")){
    resDF <- tibble(Identifier = request_args(object, "identifier")) %>%
      bind_cols(resDF)
  } else {
    resDF <- slotContents
    identifier <- list(Identifier = request_args(object, "identifier"))
    resDF <- c(identifier, resDF)
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

#' @param .which A character value. This is the identifier of the PubChem request that will be extracted from the complete list. It is ignored if \code{.combine.all = TRUE}.
#' @param .combine.all a logical value. If TRUE, the properties of all requested instances are combined into a single data frame (or a list if \code{.to.data.frame = FALSE}).
#' @param ... Additional arguments passed to other methods.
#'
#' @rdname retrieve
#' @order 3
#'
#' @examples
#' \donttest{
#' # Examples (PubChemInstanceList)
#' retrieve(compounds, "aspirin", "props", .to.data.frame = TRUE)
#'
#' # Verbose Assay References to R Console
#' assays <- get_assays(identifier = c(1234, 7815), namespace = "aid")
#'
#' instance(assays, "7815")
#' retrieve(assays, "7815", "xref", .verbose = TRUE)
#'
#' # Print assay protocol to R console (if available)
#' # Note that it may be too long to print for some assays.
#' # retrieve(assays, "1234", "protocol", .verbose = TRUE)
#'
#' # No protocol is available for assay "1234".
#' # retrieve(assays, "7815", "protocol", .verbose = TRUE)
#'
#' # Ignores ".verbose" and ".which" if ".combine.all = TRUE".
#' retrieve(assays, .slot = "xref", .verbose = TRUE, .combine.all = TRUE)
#'}
#' @importFrom dplyr bind_rows bind_cols
#' @importFrom tibble tibble
#'
#' @export
retrieve.PubChemInstanceList <- function(object, .which = NULL, .slot = NULL, .to.data.frame = TRUE,
                                         .combine.all = FALSE, ...) {
  dots <- list(...)
  args <- c(list(object = NULL, .slot = NULL, .to.data.frame = .to.data.frame, .combine.all = .combine.all), dots)

  returnInvisible <- FALSE

  # Ensure that the object is not NULL
  if (is.null(object)) {
    stop("The object provided is NULL. Please provide a valid PubChemInstanceList object.")
  }

  # Check if the results contain valid data
  if (all(sapply(object$result, is.null))) {
    stop("All instances in the object contain NULL results. Please check if the data was retrieved correctly.")
  }

  # Check if multiple slots are provided
  if (!is.null(.slot) && length(.slot) > 1) {

    # Retrieve the data for each identifier and combine them
    combined_results <- lapply(request_args(object, "identifier"), function(identifier) {
      # Check if the identifier's result is NULL
      idx <- which(request_args(object, "identifier") == identifier)
      if (is.null(object$result[[idx]])) {
        warning(paste("The result for the identifier '", identifier, "' is NULL. Skipping this identifier."))
        return(NULL)
      }

      # Retrieve data for each slot for the current identifier
      slot_data_list <- lapply(.slot, function(current_slot) {
        tryCatch({
          retrieve(object = object, .which = identifier, .slot = current_slot, .to.data.frame = TRUE, .combine.all = FALSE, ...)
        }, error = function(e) {
          warning(paste("Failed to retrieve slot '", current_slot, "' for identifier '", identifier, "': ", conditionMessage(e), sep = ""))
          return(NULL)
        })
      })

      # Combine slot data for the current identifier
      combined_slot_data <- Reduce(function(df1, df2) {
        if (is.null(df1)) return(df2)
        if (is.null(df2)) return(df1)
        merge(df1, df2, by = "Identifier", all = TRUE)
      }, slot_data_list)

      return(combined_slot_data)
    })

    # Remove NULL results from combined results
    combined_results <- combined_results[!sapply(combined_results, is.null)]

    # If combining all identifiers into a single dataframe
    if (.combine.all && length(combined_results) > 0) {
      combined_results_df <- bind_rows(combined_results)
      return(combined_results_df)
    } else if (.combine.all && length(combined_results) == 0) {
      warning("No valid data was found for any identifiers. Returning NULL.")
      return(NULL)
    } else {
      return(combined_results)
    }
  }

  # If handling a single slot or combining instances
  if (!.combine.all) {
    # Handle the case for a single instance of the PubChem object
    if (is.null(.which)) {
      idx <- 1
    } else {
      if (length(.which) > 1) {
        .which <- .which[1]
        warning("Multiple instances are not allowed in '.which'. Only the first instance is returned.")
      }

      if (!(.which %in% request_args(object, "identifier"))) {
        stop("Unknown instance identifier. Run 'request_args(object, \"identifier\")' to see all the requested instance identifiers.")
      }
      idx <- which(request_args(object, "identifier") == .which)
    }

    # If the selected instance is NULL, return an informative message
    if (is.null(object$result[[idx]])) {
      warning(paste("The result for the identifier '", .which, "' is NULL. No data to retrieve."))
      return(NULL)
    }

    # Assign the selected instance to the object argument
    args$object <- object$result[[idx]]
    args$.slot <- .slot  # Pass the single slot value to the function

    if (!is.null(args[[".verbose"]])) {
      returnInvisible <- args[[".verbose"]]
    }

    # Call retrieve with the selected instance object
    res <- tryCatch({
      do.call("retrieve", args)
    }, error = function(e) {
      warning(paste("Failed to retrieve data for the identifier '", .which, "': ", conditionMessage(e), sep = ""))
      return(NULL)
    })

  } else {
    # Handle multiple instances when .combine.all is TRUE
    if (!is.null(args[[".verbose"]])) {
      if (args[[".verbose"]]) {
        args[[".verbose"]] <- FALSE
        returnInvisible <- TRUE
      }
    }

    # Process each identifier separately and retrieve the desired slots
    res <- suppressMessages({
      lapply(request_args(object, "identifier"), function(x) {
        tmp <- instance(object, .which = x)

        # If the instance is NULL, return NULL with a warning
        if (is.null(tmp)) {
          warning(paste("The result for the identifier '", x, "' is NULL. Skipping this identifier."))
          return(NULL)
        }

        args$object <- tmp
        args$.slot <- .slot  # Pass the slot to the function

        # Attempt to retrieve data for the instance
        success <- tryCatch({
          do.call("retrieve", args)
        }, error = function(e) {
          warning(paste("Failed to retrieve data for the identifier '", x, "': ", conditionMessage(e), sep = ""))
          return(NULL)
        })

        return(success)
      })
    })

    # Remove NULL results from the results
    res <- res[!sapply(res, is.null)]

    # Combine results into a single data frame if requested and valid data exists
    if (.to.data.frame && length(res) > 0) {
      res <- bind_rows(res)
    } else if (.to.data.frame && length(res) == 0) {
      warning("No valid data was found for any identifiers. Returning NULL.")
      return(NULL)
    }
  }

  if (returnInvisible) {
    invisible(res)
  } else {
    return(res)
  }
}


#' @param .idx An integer indicating which substance result should be returned. A PubChem request may return multiple
#' substances in the output. \code{.idx} specifies the index of the substance to be extracted from the complete list.
#'
#' @rdname retrieve
#' @order 4
#'
#' @importFrom tibble as_tibble_row as_tibble_col tibble
#' @importFrom dplyr mutate_all bind_rows bind_cols
#' @importFrom magrittr '%>%'
#'
#' @export
retrieve.PC_Substance <- function(object, .slot = NULL, .idx = 1, .to.data.frame = TRUE, .verbose = FALSE, ...){

  returnInvisible <- FALSE

  if (!object$success){
    warning("'object' encountered an error. Nothing to return. \n See error details in 'object'.")
    return(NULL)
  }

  if (is.null(.slot)){
    warning("Which slot do you want to return? '.slot' is not defined. Returning NULL.")
    return(NULL)
  }

  # .slot should be length of 1 except for "PC_Properties"
  if (length(.slot) > 1){
    warning("'.slot' should be length of 1. Only the first element of '.slot' is used.")
    .slot <- .slot[1]
  }

  if (is.null(.idx)){
    .idx <- 1
  } else {
    if (length(.idx) > 1){
      warning("'.idx' should be length of 1. Only the first element of '.idx' is used.")
      .idx <- .idx[1]
    }
  }

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
          cName <- ifelse(is.null(.slot), "Value", .slot)
          as_tibble_col(slotContents, column_name = cName)
        } else {
          tibble(Name = slotNames, Value = slotContents)
        }

        # slotNames <- names(slotContents)
        #
        # # If vector slot has names, it will be structured as two column tibble_df, with names in
        # # the first column and values in the second column. Otherwise, a column tibbled_df will be
        # # returned with values only.
        # resDF <- if (is.null(slotNames)){
        #   as_tibble_col(slotContents)
        # } else {
        #   as_tibble_row(slotContents)
        # }
      }

      TRUE
    })
  }

  # Bind identifier info to returned list or data.frame
  if (.to.data.frame & !inherits(successDF, "try-error")){
    resDF <- tibble(Identifier = request_args(object, "identifier")) %>%
      bind_cols(resDF)
  } else {
    resDF <- slotContents
    identifier <- list(Identifier = request_args(object, "identifier"))
    resDF <- c(identifier, resDF)
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
#' The PugView API returns a detailed list related to PubChem requests. The 'Section' slot in this list is structured into
#' a sub-class called \code{'PugViewSection'}. This object contains information organized through several sections (or sub-sections),
#' which can be retrieved using \emph{section-specific} functions such as \link{section} and \link{sectionList}.
#'
#' The function argument \code{.to.data.frame} is ignored if the "Section" slot is being extracted from the complete list.
#' For other slots, \code{.to.data.frame} is considered as usual. See examples for usage.
#'
#' @examples
#' \donttest{
#' ### PUG VIEW EXAMPLES ###
#' pview <- get_pug_view(identifier = "2244", annotation = "data", domain = "compound")
#'
#' # PugViewSectionList object.
#' # This object contains all the section information related to the PubChem request.
#' sect <- retrieve(pview, .slot = "Section")
#' print(sect)
#'
#' retrieve(pview, .slot = "RecordType", .to.data.frame = TRUE)
#' }
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
#' @importFrom magrittr '%>%'
#' @importFrom tibble add_row tibble
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom tidyr as_tibble
#' @importFrom stringr str_to_title
#'
#' @export
AIDs.PubChemInstance_AIDs <- function(object, .to.data.frame = TRUE, ...) {
  tmp <- object$result
  nms = toupper(request_args(object, "namespace"))

  if (.to.data.frame) {
    res <- lapply(tmp, function(x) {
      # if (!x$success) {
      #   return(NULL)
      # }

      if (nms == "FORMULA") {
        tmp2 <- tibble(CID = integer(), AID = integer())

        for (info in x$result$InformationList$Information) {
          if (is.list(info) && !is.null(info$AID)) {
            for (aid in info$AID) {
              tmp2 <- add_row(tmp2,
                              CID = info$CID,
                              AID = aid)
            }
          }
        }
      } else {
        tmp2 <- bind_cols(x$result$InformationList$Information)
      }

      if(nrow(tmp2) == 0){

        tmp2 = NULL

      }else{

        if (colnames(tmp2)[1] != nms) {
          tbl <- tibble(request_args(x, .which = "identifier"))
          names(tbl) <- toupper(stringr::str_to_title(request_args(x, .which = "namespace")))
          tmp2 <- bind_cols(tbl, tmp2)
        }
      }

      return(tmp2)
    }) %>%
      bind_rows %>%
      as_tibble
  } else {
    res <- lapply(tmp, "[[", "result")
  }

  return(res)
}

#' @title Assay, Compound, and Substance Identifiers
#'
#' @description
#' These functions are used to retrieve identification information for assays, substances, and compounds from the PubChem database.
#'
#' @param object An object returned from a PubChem request, typically generated by functions such as \link{get_cids}, \link{get_aids},
#' and \link{get_sids}.
#' @param ... Additional arguments passed to other methods. Currently, these arguments have no effect.
#'
#' @rdname AIDs-SIDs-CIDs
#' @name AIDs-SIDs-CIDs
#' @order 1
#'
#' @examples
#' \donttest{
#' # Retrieve Assay IDs
#' aids <- get_aids(identifier = c("aspirin", "caffeine"), namespace = "name")
#' AIDs(aids)
#' }
#'
#' @export
AIDs <- function(object, ...){
  UseMethod("AIDs")
}

# PubChemInstance_CIDs ----
#' @rdname AIDs-SIDs-CIDs
#'
#' @importFrom dplyr bind_rows bind_cols
#' @importFrom tidyr as_tibble
#'
#' @export
CIDs.PubChemInstance_CIDs <- function(object, .to.data.frame = TRUE, ...){
  tmp <- object$result

  if(is.null(object$result[[1]]$success)){

    res = NULL

  }else{

    if (.to.data.frame){
      res <- lapply(tmp, function(x){
        xx <- suppressMessages({
          CID_List <- if (request_args(x, "domain") == "compound"){
            list(CID = x$result$IdentifierList$CID)
          } else if (request_args(x, "domain") %in% c("substance", "assay")){
            list(CID = x$result$InformationList$Information[[1]]$CID)
          } else {
            list(CID = x$result$IdentifierList$CID)
          }

          c(list(x$request_args$identifier), CID_List) %>%
            bind_cols
        })
        names(xx)[1] <- namespace_text(x$request_args$namespace)
        return(xx)
      }) %>%
        bind_rows %>%
        as_tibble
    } else {
      res <- lapply(tmp, "[[", "result")
    }
  }

  return(res)
}

#' @rdname AIDs-SIDs-CIDs
#' @order 2
#'
#' @examples
#' \donttest{
#' # Compound IDs
#' cids <- get_cids(identifier = c("aspirin", "caffein"), namespace = "name")
#' CIDs(cids)
#' }
#'
#' @export
CIDs <- function(object, ...){
  UseMethod("CIDs")
}


# PubChemInstance_SIDs ----
#' @rdname AIDs-SIDs-CIDs
#'
#' @importFrom dplyr bind_rows bind_cols
#' @importFrom tidyr as_tibble
#'
#' @export
SIDs.PubChemInstance_SIDs <- function(object, .to.data.frame = TRUE, ...){
  tmp <- object$result

  if (.to.data.frame){
    res <- lapply(tmp, function(x){
      xx <- suppressMessages({
        # Modify the SID_List extraction if returned list has different structure for
        # different domains.
        SID_List <- list(SID = x$result$InformationList$Information[[1]]$SID)

        c(list(x$request_args$identifier), SID_List) %>%
          bind_cols
      })

      names(xx)[1] <- namespace_text(x$request_args$namespace)
      return(xx)
    }) %>%
      bind_rows %>%
      as_tibble
  } else {
    res <- lapply(tmp, "[[", "result")
  }

  return(res)
}

#' @rdname AIDs-SIDs-CIDs
#' @order 3
#'
#' @examples
#' \donttest{
#' # Substance IDs
#' sids <- get_sids(identifier = c("aspirin", "caffein"), namespace = "name")
#' SIDs(sids)
#' }
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
#' @importFrom dplyr bind_rows bind_cols
#' @importFrom tidyr as_tibble
#'
#' @export
synonyms.PubChemInstance_Synonyms <- function(object, .to.data.frame = TRUE, ...){
  tmp <- object$result

  if (.to.data.frame){
    res <- lapply(tmp, function(x){
      xx <- suppressMessages({
        list(x$request_args$identifier, Synonyms = x$result$InformationList$Information[[1]]$Synonym) %>%
          bind_cols
      })

      names(xx)[1] <- namespace_text(x$request_args$namespace)
      return(xx)
    }) %>%
      bind_rows %>%
      as_tibble
  } else {
    res <- lapply(tmp, "[[", "result")
  }

  return(res)
}

#' @title Getter function for 'Synonyms'
#'
#' @description
#' Extracts synonym data from a PubChem request using the function \link{get_synonyms}.
#'
#' @param object An object of class \code{'PubChemInstance_Synonyms'}.
#' @param ... Additional arguments passed to other methods. Currently, these have no effect.
#'
#' @rdname synonyms
#' @name synonyms
#' @order 1
#'
#' @return A \code{data.frame} (or \code{list}) object containing the synonym data.
#'
#' @examples
#' \donttest{
#' syns <- get_synonyms(identifier = c("aspirin", "caffeine"), namespace = "name")
#' synonyms(syns)
#' }
#'
#' @export
synonyms <- function(object, ...){
  UseMethod("synonyms")
}



# Sections ----

#' @title Extract Sections from Pug View Request
#'
#' @description
#' \code{section} returns section details from a Pug View request.
#'
#' @param object an object returned from \link{get_pug_view}.
#' @param ... other arguments. Currently has no effect on the outputs. Can be ignored.
#'
#' @rdname section
#' @name section
#' @order 1
#'
#' @export
section <- function(object, ...){
  UseMethod("section")
}

#' @param .id A character value that corresponds to the ID of a specific section. Detailed information about the section with the given section ID will be returned. If NULL, the first section (i.e., "S1") is returned. If there is no section under \code{object}, it returns NULL with a warning. See Note/Details for more information.
#' @param .verbose A logical value. Should the resulting object be printed to the R console? If TRUE, the object is returned invisibly and the output is printed nicely to the R console. This option may not be available for some slots (or classes). See Notes/Details.
#'
#' @rdname section
#' @order 2
#'
#' @note
#' \subsection{Sections in a Pug View Request}{
#' A Pug View Request returns a detailed list from the PubChem database. This list may include data under many nested
#' sections, each corresponding to a different property structured within further nested sections. The
#' complicated structure of the returned object makes it impossible to print all information to the R console at once.
#' Therefore, it is recommended to print sections selectively.
#' Furthermore, one may navigate through the nested sections using the \link{section} function. See Examples.
#'
#' Use the \link{sectionList} function to list available sections (or subsections of a section) of a Pug View request and related section IDs.
#' }
#'
#' \subsection{Use of \code{'.verbose'} to Print Section Details}{
#' It is possible to print section details to the R console. If \code{.verbose = TRUE}, the resulting object is returned invisibly and a summary of section details is printed to the R console. This might be useful to navigate through nested sections and sequentially print multiple sections to the R console. For example, consider following command:
#'
#' \code{> section(section(request, "S1", .verbose = TRUE), "S3", .verbose = TRUE)}
#'
#' This command will print section "S1" and the subsection "S3" located under "S1" to the R console. One may navigate through sections under other sections, similar to exploring dreams within dreams as depicted in the exceptional movie \bold{Inception}. (\bold{SPOILER WARNING!!}) However, be careful not to get lost or stuck in the dreams!! Also, note that this strategy works only if \code{.verbose = TRUE} for all sections and/or subsections.
#' }
#'
#' @seealso \link{sectionList}
#'
#' @examples
#' \donttest{
#' # Pug View request for the compound "aspirin (CID = 2244)".
#' pview <- get_pug_view(identifier = "2244", annotation = "data", domain = "compound")
#'
#' section(pview, "S1")
#' section(pview, "S1", .verbose = TRUE)
#'
#' # List all available sections
#' sectionList(pview)
#'
#' # Subsections under the section "S1"
#' sectionList(section(pview, "S1"))
#'
#' # Print multiple sections
#' # section(section(pview, "S1", .verbose = TRUE), "S3", .verbose = TRUE)
#'}
#' @export
section.PugViewInstance <- function(object, .id = "S1", .verbose = FALSE, ...){
  dots <- list(...)
  call_args <- c(list(object = retrieve(object, .slot = "Section"), .id = .id, .verbose = .verbose), dots)
  do.call("section", call_args)
}

#' @rdname section
#' @order 3
#'
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

#' @rdname section
#' @order 4
#'
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


#' @title List Available Section/Subsections
#'
#' @description
#' This function may be used to list available sections (or subsections) of a PubChem request returned from \link{get_pug_view}. It is useful when one wants to extract a specific section (or subsection) from PubChem request. It supports patteern-specific searches within sections. See Detail/Note below for more information.
#'
#' @param object an object of PubChem request, generally returned from \link{get_pug_view}.
#' @param ... other arguments. Currently has no effect on the outputs. Can be ignored.
#'
#' @name sectionList
#' @rdname sectionList
#'
#' @seealso \link{section}
#'
#' @export
sectionList <- function(object, ...){
  UseMethod("sectionList")
}

#' @rdname sectionList
#'
#' @export
sectionList.PugViewInstance <- function(object, ...){
  dots <- list(...)
  call_args <- c(list(object = retrieve(object, .slot = "Section")), dots)
  do.call("sectionList", call_args)
}

#' @param .pattern a character vector. Each text pattern given here will be searched within Pug View sections by using the pattern matching strategy defined with \code{.match_type}. If not specified or NULL, all available sections will be returned.
#' @param .match_type a string. How should search patterns (i.e., \code{.pattern}) matched with section names? Available options are "contain", "match", "start", and "end" which can be used for partial and/or exact pattern matching. Default is "contain". See Details below for more information.
#'
#' @details
#' Pattern matching is used to filter sections that match user-defined patterns. It is useful when there are more sections than allowed to print R console. In such situations, it may be reasonable to print a subset of all section list to R console that meets search criteria. There are several pattern matching methods as described below
#' \itemize{
#'   \item \bold{Partial Matching} ("contain", "start", "end"): Returns the section names that contains or starts/ends by given text patterns.
#'   \item \bold{Exact Matching} ("match"): Returns the section names that exactly matches given text patterns.
#' }
#'
#' @rdname sectionList
#'
#' @importFrom tibble tibble
#' @importFrom tidyr ends_with starts_with contains
#'
#' @examples
#' \donttest{
#' pview <- get_pug_view(identifier = "2244", annotation = "data", domain = "compound")
#'
#' # List all section names
#' sectionList(pview)
#'
#' # Pattern-matched section names
#' sectionList(pview, .pattern = c("safety", "chemical"), .match_type = "contain")
#' sectionList(pview, .pattern = "safety", .match_type = "match")
#' sectionList(pview, .pattern = "properties", .match_type = "end")
#'
#' # Use section IDs to extract section data from Pug View request
#' section(pview, "S12") # Safety and Hazards
#'}
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


#' @rdname sectionList
#'
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


# pubChemData ----
#' @title Retrieve Raw Data from PUG REST Object
#'
#' @description
#' A short description...
#'
#' @param object an object of class 'PugRestInstance' returned from \link{get_pug_rest} function.
#' @param ... additional arguments. Currently has no effect on results.
#'
#' @name pubChemData
#' @rdname pubChemData
#'
#' @seealso \link{get_pug_rest}
#'
#' @return a vector, list, or data.frame containing the raw data retrieved from Pub Chem database through PUG REST API.
#'
#' @examples
#' \donttest{
#' result <- get_pug_rest(identifier = "2244", namespace = "cid", domain = "compound", output = "JSON")
#' pubChemData(result)
#' }
#'
#' @export
pubChemData <- function(object, ...){
  UseMethod("pubChemData")
}

#' @rdname pubChemData
#'
#' @export
pubChemData.PugRestInstance <- function(object, ...){
  object[["result"]]
}
