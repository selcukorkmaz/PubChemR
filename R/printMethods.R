# Print Methods ----
## PubChemInstanceList ----
#' @export
print.PubChemInstanceList <- function(x, ...){
  cat("\n")
  cat(" An object of class ", "'", primaryClass(x), "'", sep = "", "\n\n")
  cat(" Number of instances: ", length(x$result), "\n", sep = "")

  cat("  - Domain: ", domain_text(request_args(x, "domain")), sep = "", "\n")
  cat("  - Namespace: ", namespace_text(request_args(x, "namespace")), sep = "", "\n")

  identifiers <- request_args(x, "identifier")
  nIdentifiers <- length(identifiers)
  suffix_identifiers <- ""
  if (length(identifiers) > 2){
    identifiers <- identifiers[1:2]
    suffix_identifiers <- paste0(", ... and ", nIdentifiers - 2, " more.")
  }

  cat("  - Identifier(s): ", paste0(identifiers, collapse = ", "), suffix_identifiers, sep = "", "\n\n")
  cat(" * Run 'instance(...)' function to extract specific instances from the complete list, and", "\n", sep = "")
  cat("   'request_args(...)' to see all the requested instance identifiers.", sep = "", "\n")
  cat(" * See ?instance and ?request_args for details.", "\n\n")
}

## PubChemInstance ----
#' @export
print.PubChemInstance <- function(x, ...){
  cat("\n")
  cat(" An object of class ", "'", primaryClass(x), "'", sep = "", "\n\n")
  cat(" Request Details: ", "\n")
  cat("  - Domain: ", domain_text(request_args(x, "domain")), sep = "", "\n")
  cat("  - Namespace: ", namespace_text(request_args(x, "namespace")), sep = "", "\n")

  # Print extra information for 'PugRestInstance' class
  if ("PugRestInstance" %in% class(x)){
    cat("  - Operation: ", ifelse(is.null(request_args(x, "operation")), "<NULL>", request_args(x, "operation")), sep = "", "\n")
  }

  identifiers <- request_args(x, "identifier")
  nIdentifiers <- length(identifiers)
  suffix_identifiers <- ""
  if (length(identifiers) > 2){
    identifiers <- identifiers[1:2]
    suffix_identifiers <- paste0(", ... and ", nIdentifiers - 2, " more.")
  }

  cat("  - Identifier: ", paste0(identifiers, collapse = ", "), suffix_identifiers, sep = "", "\n\n")
  #cat(" Details:", "\n\n", sep = "")

  if (!x$success){
    cat(" Stopped with an ERROR. Details are below:", "\n\n")
    for (i in names(x$error)){
      cat("  - ", i, ": ", x$error[[i]], sep = "", "\n")
    }
    cat("\n\n")
  }

  if (x$success){
    cat(" Instance Details: ", "\n")
    instance_results <- if (request_args(x, "domain") %in% c("assay")){
       x$result$PC_AssayContainer[[1]]$assay$descr
    } else if (request_args(x, "domain") %in% c("compound")){
      if ("PC_Properties" %in% class(x)){
        x$result[[1]][[1]][[1]]
      } else {
        x$result[[1]][[1]]
      }
    }

    instanceNames <- names(instance_results)

    for (item in instanceNames){
      itemClass <- class(instance_results[[item]])[1]
      itemNames <- names(instance_results[[item]])

      if (!is.null(itemNames) & length(itemNames) > 4){
        itemNames <- c(itemNames[1:4], "...")
        named_unnamed <- "named"
      }

      named_unnamed <- ifelse(is.null(itemNames), "unnamed", "named")

      cat("  - ", item, " (", length(instance_results[[item]]), ")", ": ", "[<", named_unnamed, " ", itemClass, ">] ",
          paste(itemNames, collapse = ", "), sep = "", "\n")
    }

    # print notes for getter functions.
    if (!is.null(instanceNames)){
      cat("\n")
      if ("PC_Properties" %in% class(x)){

        cat(" NOTE: Run getter function 'retrieve(...)' to extract properties from requested elements. \n")
        cat("       See ?retrieve for details.", sep = "", "\n")
      } else {
        cat(" NOTE: Run getter function 'retrieve()' with element name above to extract data from corresponding list.", "\n")
        cat("       See ?retrieve for details. ", sep = "", "\n")
      }
    }
  }
}

## PubChemInstance_AIDs ----
#' @export
print.PubChemInstance_AIDs <- function(x, ...){
  cat("\n")
  cat(" Assay IDs (AIDs) from PubChem Database", sep = "", "\n\n")
  cat(" Request Details: ", "\n")
  cat("  - Domain: ", domain_text(x$request_args$domain), sep = "", "\n")
  cat("  - Namespace: ", namespace_text(x$request_args$namespace), sep = "", "\n")

  identifiers <- x$request_args$identifier
  nIdentifiers <- length(identifiers)
  suffix_identifiers <- ""
  if (length(identifiers) > 2){
    identifiers <- identifiers[1:2]
    suffix_identifiers <- paste0(", ... and ", nIdentifiers - 2, " more.")
  }

  cat("  - Identifier: ", paste0(identifiers, collapse = ", "), suffix_identifiers, sep = "", "\n\n")
  success <- unlist(lapply(x$result, "[[", "success"))

  if (!all(success)){
    if (any(success)){
      cat(" WARNING: AIDs cannot be retrieved succecfully for some instances.", "\n")
      cat("          Results were returned for elements which are successfully retrieved.", "\n\n")
    }

    if (all(!success)){
      cat(" Stopped with an ERROR. No results are returned.", "\n")
    }
  }

  if (any(success)){
    cat(" NOTE: run AIDs(...) to extract Assays ID data. See ?AIDs for help.", "\n\n")
  }
}


## PubChemInstance_CIDs ----
#' @export
print.PubChemInstance_CIDs <- function(x, ...){
  cat("\n")
  cat(" Compound IDs (CIDs) from PubChem Database", sep = "", "\n\n")
  cat(" Request Details: ", "\n")
  cat("  - Domain: ", domain_text(x$request_args$domain), sep = "", "\n")
  cat("  - Namespace: ", namespace_text(x$request_args$namespace), sep = "", "\n")

  identifiers <- x$request_args$identifier
  nIdentifiers <- length(identifiers)
  suffix_identifiers <- ""
  if (length(identifiers) > 2){
    identifiers <- identifiers[1:2]
    suffix_identifiers <- paste0(", ... and ", nIdentifiers - 2, " more.")
  }

  cat("  - Identifier: ", paste0(identifiers, collapse = ", "), suffix_identifiers, sep = "", "\n\n")
  success <- unlist(lapply(x$result, "[[", "success"))

  if (!all(success)){
    if (any(success)){
      cat(" WARNING: CIDs cannot be retrieved succecfully for some instances.", "\n")
      cat("          Results were returned for elements which are successfully retrieved.", "\n\n")
    }

    if (all(!success)){
      cat(" Stopped with an ERROR. No results are returned.", "\n")
    }
  }

  if (any(success)){
    cat(" NOTE: run CIDs(...) to extract Compound ID data. See ?CIDs for help.", "\n\n")
  }
}


## PubChemInstance_SIDs ----
#' @export
print.PubChemInstance_SIDs <- function(x, ...){
  cat("\n")
  cat(" Substance IDs (SIDs) from PubChem Database", sep = "", "\n\n")
  cat(" Request Details: ", "\n")
  cat("  - Domain: ", domain_text(x$request_args$domain), sep = "", "\n")
  cat("  - Namespace: ", namespace_text(x$request_args$namespace), sep = "", "\n")

  identifiers <- x$request_args$identifier
  nIdentifiers <- length(identifiers)
  suffix_identifiers <- ""
  if (length(identifiers) > 2){
    identifiers <- identifiers[1:2]
    suffix_identifiers <- paste0(", ... and ", nIdentifiers - 2, " more.")
  }

  cat("  - Identifier: ", paste0(identifiers, collapse = ", "), suffix_identifiers, sep = "", "\n\n")
  success <- unlist(lapply(x$result, "[[", "success"))

  if (!all(success)){
    if (any(success)){
      cat(" WARNING: SIDs cannot be retrieved succecfully for some instances.", "\n")
      cat("          Results were returned for elements which are successfully retrieved.", "\n\n")
    }

    if (all(!success)){
      cat(" Stopped with an ERROR. No results are returned.", "\n")
    }
  }

  if (any(success)){
    cat(" NOTE: run SIDs(...) to extract Substance ID data. See ?SIDs for help.", "\n\n")
  }
}


## PubChemInstance_Synonyms ----
#' @export
print.PubChemInstance_Synonyms <- function(x, ...){
  cat("\n")
  cat(" Synonyms from PubChem Database", sep = "", "\n\n")
  cat(" Request Details: ", "\n")
  cat("  - Domain: ", domain_text(x$request_args$domain), sep = "", "\n")
  cat("  - Namespace: ", namespace_text(x$request_args$namespace), sep = "", "\n")

  # Print extra information for 'PugRestInstance' class
  if ("PugRestInstance" %in% class(x)){
    cat("  - Operation: ", request_args(x, "operation"), sep = "", "\n")
  }

  identifiers <- x$request_args$identifier
  nIdentifiers <- length(identifiers)
  suffix_identifiers <- ""
  if (length(identifiers) > 2){
    identifiers <- identifiers[1:2]
    suffix_identifiers <- paste0(", ... and ", nIdentifiers - 2, " more.")
  }

  cat("  - Identifier: ", paste0(identifiers, collapse = ", "), suffix_identifiers, sep = "", "\n\n")
  success <- unlist(lapply(x$result, "[[", "success"))

  if (!all(success)){
    if (any(success)){
      cat(" WARNING: Synonyms cannot be retrieved succecfully for some instances.", "\n")
      cat("          Results were returned for elements which are successfully retrieved.", "\n\n")
    }

    if (all(!success)){
      cat(" Stopped with an ERROR. No results are returned.", "\n")
    }
  }

  if (any(success)){
    cat(" NOTE: run 'synonyms(...)' to extract synonyms data. See ?synonyms for help.", "\n\n")
  }
}

## PubChemInstance_Substances ----
#' @export
print.PC_Substance <- function(x, ...){
  cat("\n")
  cat(" Substance Data from PubChem Database", "\n\n")
  cat(" Request Details: ", "\n")
  cat("  - Domain: ", domain_text(request_args(x, "domain")), sep = "", "\n")
  cat("  - Namespace: ", namespace_text(request_args(x, "namespace")), sep = "", "\n")

  identifiers <- request_args(x, "identifier")
  nIdentifiers <- length(identifiers)
  suffix_identifiers <- ""
  if (length(identifiers) > 2){
    identifiers <- identifiers[1:2]
    suffix_identifiers <- paste0(", ... and ", nIdentifiers - 2, " more.")
  }

  cat("  - Identifier: ", paste0(identifiers, collapse = ", "), suffix_identifiers, sep = "", "\n\n")
  #cat(" Details:", "\n\n", sep = "")

  if (!x$success){
    cat(" Stopped with an ERROR. Details are below:", "\n\n")
    for (i in names(x$error)){
      cat("  - ", i, ": ", x$error[[i]], sep = "", "\n")
    }
    cat("\n\n")
  }

  if (x$success){
    instance_results <- find_last_layer(x$result)

    if (!is.null(names(instance_results))){
      instance_results <- list(instance_results)
    }
    cat(" Number of substances retrieved: ", length(instance_results), sep = "", "\n\n")
    cat(" Substances contain data within following slots;", "\n")

    substanceNamesList <- lapply(instance_results, names)
    substanceNames <- unique(unlist(lapply(instance_results, names)))

    indexData <- data.frame(slot = substanceNames, idx = NA_integer_)

    for (i in 1:length(substanceNames)){
      continue <- TRUE
      j <- 1
      while (continue){
        found <- substanceNames[i] %in% substanceNamesList[[j]]
        if (found){
          indexData[i, "idx"] <- j
          continue <- FALSE
        }
        j <- j + 1

        if (j > length(substanceNames)){
          break
        }
      }
    }

    for (i in 1:nrow(indexData)){
      substance.i <- instance_results[[indexData[i, "idx"]]]
      itemClass <- class(substance.i[[indexData[i, "slot"]]])[1]
      itemNames <- names(substance.i[[indexData[i, "slot"]]])

      if (!is.null(itemNames) & length(itemNames) > 4){
        itemNames <- c(itemNames[1:4], "...")
        named_unnamed <- "named"
      }

      named_unnamed <- ifelse(is.null(itemNames), "unnamed", "named")

      cat("  - ", indexData[i, "slot"], " (", length(instance_results[[indexData[i, "idx"]]][[indexData[i, "slot"]]]), ")", ": ",
          "[<", named_unnamed, " ", itemClass, ">] ", paste(itemNames, collapse = ", "), sep = "", "\n")
    }

    # print notes for getter functions.
    if (!is.null(substanceNames)){
      cat("\n")
      cat(" NOTE: Run getter function 'retrieve()' with element name above to extract data from corresponding list.", "\n")
      cat("       See ?retrieve for details. ", sep = "", "\n")
    }
  }
}


## PugViewInstance ----
#' @export
print.PugViewInstance <- function(x, ...){
  cat("\n")
  cat(" PUG View Data from PubChem Database", "\n\n")

  if (!x$success){
    cat(" Stopped with an ERROR. Details are below:", "\n\n")
    for (i in names(x$error)){
      cat("  - ", i, ": ", x$error[[i]], sep = "", "\n")
    }
    cat("\n\n")
  }

  if (x$success){
    cat(" Request Details: ", "\n")
    cat("  - Domain: ", domain_text(request_args(x, "domain")), sep = "", "\n")
    cat("  - Annotation: ", request_args(x, "annotation"), sep = "", "\n")

    identifiers <- request_args(x, "identifier")
    nIdentifiers <- length(identifiers)
    suffix_identifiers <- ""
    if (length(identifiers) > 2){
      identifiers <- identifiers[1:2]
      suffix_identifiers <- paste0(", ... and ", nIdentifiers - 2, " more.")
    }

    cat("  - Identifier: ", paste0(identifiers, collapse = ", "), suffix_identifiers, sep = "", "\n\n")
    #cat(" Details:", "\n\n", sep = "")
    #
    cat(" Pug View Details:", "\n")

    instance_results <- find_last_layer(x$result)
    instanceNames <- names(instance_results)

    printSlotDetails(instance_results, pugViewSection = TRUE)

    # print notes for getter functions.
    if (!is.null(instanceNames)){
      cat("\n")
      cat(" NOTE: Run getter function 'retrieve()' with element name above to extract data from corresponding list.", "\n")
      cat("       See ?retrieve for details. ", sep = "", "\n")
    }
  }
}

## PugViewSectionList ----
#' @export
print.PugViewSectionList <- function(x, ...){
  cat("\n")
  cat(" PUG View Data Sections", "\n\n")

  if (!x$success){
    cat(" Stopped with an ERROR. Details are below:", "\n\n")
    for (i in names(x$error)){
      cat("  - ", i, ": ", x$error[[i]], sep = "", "\n")
    }
    cat("\n\n")
  }

  if (x$success){
    if (!is.null(x$recordInformation)){
      cat(" Request Details:", "\n")

      recordInformation <- x$recordInformation
      for (item in names(recordInformation)){
        cat("  - ", gsub("([a-z])([A-Z])", "\\1 \\2", item), ": ", recordInformation[[item]], sep = "", "\n")
      }

      cat("\n")
    }

    if (!is.null(x$result)){
      sectionList <- x$result
      cat(" Section Details:", "\n")
      cat("  - Number of available sections: ", length(sectionList), "\n", sep = "")

      sectionHeadings <- unlist(lapply(sectionList, "[[", "TOCHeading"))
      sectionHeadingsText <- if (length(sectionHeadings) > 2){
        paste0(paste0(sectionHeadings[1:2], collapse = ", "), ", ...", " and ", length(sectionHeadings) - 2, " more.", sep = "")
      } else {
        paste0(sectionHeadings, collapse = ", ")
      }

      cat("  - Section headings: ", sectionHeadingsText, sep = "", "\n")

      # print notes for getter functions.
      cat("\n")
      cat(" NOTE: Run getter function 'section()' to extract section data. To list available sections, run 'sectionList()'.", "\n")
      cat("       See ?section and ?sectionList for details. ", sep = "", "\n")
    }
  }
}

## PugViewSection ----
#' @export
print.PugViewSection <- function(x, ...){
  sectionList <- x$result

  cat("\n")
  cat(" PUG View Data Sections", ifelse(is.null(sectionList$TOCHeading), "", paste0("(", sectionList$TOCHeading, ")")), "\n\n")

  if (!x$success){
    cat(" Stopped with an ERROR. Details are below:", "\n\n")
    for (i in names(x$error)){
      cat("  - ", i, ": ", x$error[[i]], sep = "", "\n")
    }
    cat("\n\n")
  }

  if (x$success){
    if (!is.null(x$recordInformation)){
      cat(" Request Details:", "\n")

      recordInformation <- x$recordInformation
      for (item in names(recordInformation)){
        cat("  - ", gsub("([a-z])([A-Z])", "\\1 \\2", item), ": ", recordInformation[[item]], sep = "", "\n")
      }

      cat("\n")
    }

    if (!is.null(x$result)){
      cat(" Section Details:", "\n")
      printSlotDetails(sectionList, pugViewSection = TRUE)

      # print notes for getter functions.
      if (!is.null(names(sectionList))){
        cat("\n")
        cat(" NOTE: Run getter function 'retrieve()' with element name above to extract data from corresponding list.", "\n")
        cat("       See ?retrieve for details. ", sep = "", "\n")

        if ("Section" %in% names(sectionList)){
          # print notes for getter functions.
          cat("\n")
          cat(" NOTE: Run getter function 'section()' to extract section data. To list available sections, run 'sectionList()'.", "\n")
          cat("       See ?section and ?sectionList for details. ", sep = "", "\n")
        }
      }
    }
  }
}

## PugRestInstance ----
#' @export
print.PugRestInstance <- function(x, ...){
  cat("\n")
  cat(" An object of class ", "'", primaryClass(x), "'", sep = "", "\n\n")
  cat(" Request Details: ", "\n")
  cat("  - Domain: ", domain_text(request_args(x, "domain")), sep = "", "\n")
  cat("  - Namespace: ", namespace_text(request_args(x, "namespace")), sep = "", "\n")

  # Print extra information for 'PugRestInstance' class
  if ("PugRestInstance" %in% class(x)){
    cat("  - Operation: ", ifelse(is.null(request_args(x, "operation")), "<NULL>", request_args(x, "operation")), sep = "", "\n")
  }

  identifiers <- request_args(x, "identifier")
  nIdentifiers <- length(identifiers)
  suffix_identifiers <- ""
  if (length(identifiers) > 2){
    identifiers <- identifiers[1:2]
    suffix_identifiers <- paste0(", ... and ", nIdentifiers - 2, " more.")
  }

  cat("  - Identifier: ", paste0(identifiers, collapse = ", "), suffix_identifiers, sep = "", "\n\n")

  if (!x$success){
    cat(" Stopped with an ERROR. Details are below:", "\n\n")
    for (i in names(x$error)){
      cat("  - ", i, ": ", x$error[[i]], sep = "", "\n")
    }
    cat("\n\n")
  }

  if (x$success){
    if (request_args(x, "save") | toupper(request_args(x, "output")) == "SDF"){
      cat(" Details [Saved File(s)]: ", "\n")
      cat("  - File name: '", x$fileDetails$Name, "'", "\n", sep = "")
      cat("  - Saved to: '", x$fileDetails$Path, "'", "\n", sep = "")
      cat("  - File type: ", x$fileDetails$Type, "\n", sep = "")
      if (!is.null(x$fileDetails$Size) && is.list(x$fileDetails$Size)) {
        cat("  - Size (", x$fileDetails$Size$unit, "): ", x$fileDetails$Size$size, "\n", sep = "")
      } else {
        cat("  - Size: Unknown\n")
      }
      cat("\n")
    }

    # Print notes for getter functions.
    cat(" NOTE: Run getter function 'pubChemData(...)' to extract raw data retrieved from PubChem Database. \n")
    cat("       See ?pubChemData for details.", sep = "", "\n")
  }
}
