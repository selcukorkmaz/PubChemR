#' @title Retrieve JSON Data from PubChem
#'
#' @description This function sends a request to PubChem to retrieve JSON data based on specified parameters.
#' It handles errors and warnings gracefully, providing informative messages when they occur.
#' This function is used internally by all get_* functions, and users will not typically run this function directly.
#'
#' @param identifier A vector of positive integers (e.g., cid, sid, aid) or identifier strings (source, inchikey, formula). In some cases, only a single identifier string (name, smiles, xref; inchi, sdf by POST only).
#' @param namespace Specifies the namespace for the query. For the 'compound' domain, possible values include 'cid', 'name', 'smiles', 'inchi', 'sdf', 'inchikey', 'formula', 'substructure', 'superstructure', 'similarity', 'identity', 'xref', 'listkey', 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure', and 'fastformula'. For other domains, the possible namespaces are domain-specific.
#' @param domain Specifies the domain of the query. Possible values are 'substance', 'compound', 'assay', 'gene', 'protein', 'pathway', 'taxonomy', 'cell', 'sources', 'sourcetable', 'conformers', 'annotations', 'classification', and 'standardize'.
#' @param operation Specifies the operation to be performed on the input records. For the 'compound' domain, possible operations include 'record', 'property', 'synonyms', 'sids', 'cids', 'aids', 'assaysummary', 'classification', 'xrefs', and 'description'. The available operations are domain-specific.
#' @param searchtype Specifies the type of search to be performed. For structure searches, possible values are combinations of 'substructure', 'superstructure', 'similarity', 'identity' with 'smiles', 'inchi', 'sdf', 'cid'. For fast searches, possible values are combinations of 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure' with 'smiles', 'smarts', 'inchi', 'sdf', 'cid', or 'fastformula'.
#' @param options Additional parameters passed to \code{\link{get_json}}.
#' @param ... Other arguments. Deprecated.
#'
#' @return A list containing the parsed JSON response from PubChem. Returns NULL if an error or warning occurs.
#'
#' @importFrom RJSONIO fromJSON toJSON
#'
#' @keywords internal
#'
#' @noRd
get_json <- function(identifier, namespace = 'cid', domain = 'compound', operation = NULL, searchtype = NULL, options = NULL, ...) {

  PubChemList <- list(
    result = list(),
    request_args = list(
      namespace = namespace,
      identifier = identifier,
      domain = domain
    ),
    success = logical(),
    error = NULL
  )

  result <- fromJSON({
    tryCatch({
      get_pubchem(identifier, namespace, domain, operation, "JSON", searchtype, options)
    },
    error = function(e) {
      err.text <- c("Failed", e$message)
      names(err.text) <- c("Code", "Message")
      return(toJSON(list(Fault = err.text)))
    })
  })

  # If not failed with an error.
  if (is.null(result$Fault)){
    PubChemList[["result"]] <- result
    PubChemList[["success"]] <- TRUE
  } else {
    PubChemList[["success"]] <- FALSE

    # Command as a string
    command_string <- result$Fault["Message"]

    # Parsing and evaluating the command string
    error_details <- eval(parse(text = command_string))

    PubChemList[["error"]] <- error_details
  }

  structure(
    PubChemList,
    class = "PubChemInstance"
  )
}
