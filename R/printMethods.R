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
  cat(" An object of class ", "'", class(x), "'", sep = "", "\n\n")
  cat(" Number of instances: ", length(x), "\n\n", sep = "")
}

## PubChemInstance ----
#' @export
print.PubChemInstance <- function(x, ...){
  cat("\n")
  cat(" An object of class ", "'", class(x), "'", sep = "", "\n\n")
  cat(" Details of the compound with CID=", unlist(x$id), ". ", "A list with following components.", "\n\n", sep = "")

  for (item in names(x)){
    itemNames <- names(x[[item]])
    if (length(itemNames) > 3){
      itemNames <- c(itemNames[1:3], "...")
    }

    if (is.null(itemNames)){
      itemNames <- paste0("<unnamed ", class(x[[item]]), ">")
    }

    cat("  - ", item, " (", length(x[[item]]), ")", ": ", paste0(itemNames, collapse = ", "), sep = "", "\n")
  }

  cat("\n")
  cat(" NOTE: Run getter function with compound name above to extract data from corresponding list, e.g., atoms(...).", "\n\n")
}

## get_aids ----
#' @export
print.get_aids <- function(x, ...){
  cat("\n")
  cat(" Assay IDs (AIDs) from PubChem Database", sep = "", "\n\n")

  call_args <- call_params(x)
  cat(" Number of elements: ", length(call_args$identifier), sep = "", "\n")

  itemNames <- call_args$identifier
  if (length(itemNames) > 3){
    itemNames <- c(itemNames[1:3], "...")
  }

  cat("  - Compounds (", compound_identifier_text(call_args$namespace), "): ", paste0(itemNames, collapse = ", "), sep = "", "\n")
  column_names <- names(x$AID)
  if (length(column_names) > 4){
    column_names <- c(column_names[1:4], "...")
  }
  cat("  - AIDs [a ", "\"", class(x$AID)[1], "\"", " object]: ", paste0(column_names, collapse = ", "), sep = "", "\n\n")
  cat(" NOTE: run AIDs(...) to extract AID data. See ?AIDs for help.", "\n")
}


# get_cids ----
#' @export
print.get_cids <- function(x, ...){
  cat("\n")
  cat(" Compound IDs (CIDs) from PubChem Database", sep = "", "\n\n")

  call_args <- call_params(x)
  cat(" Number of elements: ", length(call_args$identifier), sep = "", "\n")

  itemNames <- call_args$identifier
  if (length(itemNames) > 3){
    itemNames <- c(itemNames[1:3], "...")
  }

  cat("  - Compounds (", compound_identifier_text(call_args$namespace), "): ", paste0(itemNames, collapse = ", "), sep = "", "\n")
  column_names <- names(x$CID)
  if (length(column_names) > 4){
    column_names <- c(column_names[1:4], "...")
  }
  cat("  - CIDs [a ", "\"", class(x$CID)[1], "\"", " object]: ", paste0(column_names, collapse = ", "), sep = "", "\n\n")
  cat(" NOTE: run CIDs(...) to extract CID data. See ?CIDs for help.", "\n")
}


# get_sids ----
#' @export
print.get_sids <- function(x, ...){
  cat("\n")
  cat(" Substance IDs (SIDs) from PubChem Database", sep = "", "\n\n")

  call_args <- call_params(x)
  cat(" Number of elements: ", length(call_args$identifier), sep = "", "\n")

  itemNames <- call_args$identifier
  if (length(itemNames) > 3){
    itemNames <- c(itemNames[1:3], "...")
  }

  cat("  - Compounds (", compound_identifier_text(call_args$namespace), "): ", paste0(itemNames, collapse = ", "), sep = "", "\n")
  column_names <- names(x$SID)
  if (length(column_names) > 4){
    column_names <- c(column_names[1:4], "...")
  }
  cat("  - SIDs [a ", "\"", class(x$SID)[1], "\"", " object]: ", paste0(column_names, collapse = ", "), sep = "", "\n\n")
  cat(" NOTE: run SIDs(...) to extract CID data. See ?CIDs for help.", "\n")
}

