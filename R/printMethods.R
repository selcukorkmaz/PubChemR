# Print Methods ----
## PubChemRequest class ----

#' Print Method Dispatch
#'
#' @param x An object to print.
#' @param ... Additional arguments.
#'
#' @name print
NULL

#' @rdname print
#' @importFrom dplyr case_when
#' @importFrom utils object.size
#' @export
print.PubChemRequest <- function(x, ...){

  cat("\n")
  cat(" An object of class ", "'", class(x)[1], "'", sep = "", "\n\n")

  # If PubChem retrieval has encountered with an error.
  if (!(x$success)){
    # Command as a string
    command_string <- x$Fault["Message"]

    # Parsing and evaluating the command string
    ERR_list <- eval(parse(text = command_string))

    cat(" Process stopped with an error.", "\n")
    cat("  - CODE: \"", ERR_list$Code, "\"", sep = "", "\n")
    cat("  - ERROR MESSAGE: \"", ERR_list$Message, "\"", sep = "", "\n\n")
  } else {
    call_args <- call_params(x)

    compound_identifier_text <- case_when(
      .default = "Domain-Specific",
      call_args$namespace == "name" ~ "Name",
      call_args$namespace == "cid" ~ "CID",
      call_args$namespace == "smiles" ~ "SMILES",
      call_args$namespace == "inchi" ~ "INCHI",
      call_args$namespace == "inchikey" ~ "INCHI Key",
      call_args$namespace == "sdf" ~ "SDF",
      call_args$namespace == "formula" ~ "Formula",
      call_args$namespace == "substructure" ~ "Sub-structure",
      call_args$namespace == "superstructure" ~ "Super-structure",
      call_args$namespace == "similarity" ~ "Similarity",
      call_args$namespace == "identity" ~ "Identity",
      call_args$namespace == "xref" ~ "Cross-Reference",
      call_args$namespace == "listkey" ~ "List Key",
      call_args$namespace == "fastidentity" ~ "Fast Identity",
      call_args$namespace == "fastsimilarity_2d" ~ "2-D Fast Similarity",
      call_args$namespace == "fastsimilarity_3d" ~ "3-D Fast Similarity",
      call_args$namespace == "fastsubstructure" ~ "Fast Sub-structure",
      call_args$namespace == "fastsuperstructure" ~ "Fast Super-structure",
      call_args$namespace == "fastformula" ~ "Fast Formula"
    )

    domain_text <- case_when(
      .default = "Substance",
      call_args$domain == "compound" ~ "Compound",
      call_args$domain == "assay" ~ "Assay",
      call_args$domain == "gene" ~ "Gene",
      call_args$domain == "protein" ~ "Protein",
      call_args$domain == "taxonomy" ~ "Taxonomy",
      call_args$domain == "cell" ~ "Cell",
      call_args$domain == "sources" ~ "Sources",
      call_args$domain == "sourcetable" ~ "Source Table",
      call_args$domain == "conformers" ~ "Conformers",
      call_args$domain == "annotations" ~ "Annotations",
      call_args$domain == "classification" ~ "Classification",
      call_args$domain == "standardize" ~ "Standardize"
    )

    cat(" A list with instance(s) retrieved from 'PubChem' Database using;", "\n")
    cat("   - Instance Identifier (", compound_identifier_text, "): ", paste0(call_args$identifier, collapse = ", ", sep = ""), sep = "", "\n")
    cat("   - Domain: ", domain_text, sep = "", "\n")
    #cat("   - No. of Compound(s) Matched: ", length(x[[1]][[1]]), sep = "", "\n")

    file_size <- as.numeric(object.size(x))
    file_size_unit <- case_when(
      .default = "Bytes",
      (file_size >= 1024 & file_size < 1024 ** 2) ~ "KB",
      (file_size >= 1024 ** 2 & file_size < 1024 ** 3) ~ "MB",
      (file_size >= 1024 ** 3 & file_size < 1024 ** 4) ~ "GB",
      file_size >= 1024 ** 4 ~ "PB",
    )

    file_size <- case_when(
      .default = file_size,
      file_size_unit == "KB" ~ file_size / 1024,
      file_size_unit == "MB" ~ file_size / (1024 ** 2),
      file_size_unit == "GB" ~ file_size / (1024 ** 3),
      file_size_unit == "PB" ~ file_size / (1024 ** 4)
    )
    cat("\n", " Downloaded File Size: ", round(file_size, digits = 2), " ", file_size_unit, sep = "", "\n\n")
  }
}

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

