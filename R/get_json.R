#' Retrieve JSON Data from PubChem
#'
#' This function sends a request to PubChem to retrieve JSON data based on the specified parameters.
#' It handles errors and warnings gracefully, providing informative messages when they occur.
#' This function used internally by all get_* functions, and users will not typically run this function directly.
#'
#' @param identifier A vector of positive integers (e.g. cid, sid, aid) or identifier strings (source, inchikey, formula). In some cases, only a single identifier string (name, smiles, xref; inchi, sdf by POST only).
#' @param namespace Specifies the namespace for the query. For the 'compound' domain, possible values include 'cid', 'name', 'smiles', 'inchi', 'sdf', 'inchikey', 'formula', 'substructure', 'superstructure', 'similarity', 'identity', 'xref', 'listkey', 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure', and 'fastformula'. For other domains, the possible namespaces are domain-specific.
#' @param domain Specifies the domain of the query. Possible values are 'substance', 'compound', 'assay', 'gene', 'protein', 'pathway', 'taxonomy', 'cell', 'sources', 'sourcetable', 'conformers', 'annotations', 'classification', and 'standardize'.
#' @param operation Specifies the operation to be performed on the input records. For the 'compound' domain, possible operations include 'record', 'property', 'synonyms', 'sids', 'cids', 'aids', 'assaysummary', 'classification', 'xrefs', and 'description'. The available operations are domain-specific.
#' @param searchtype Specifies the type of search to be performed. For structure searches, possible values are combinations of 'substructure', 'superstructure', 'similarity', 'identity' with 'smiles', 'inchi', 'sdf', 'cid'. For fast searches, possible values are combinations of 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure' with 'smiles', 'smarts', 'inchi', 'sdf', 'cid', or 'fastformula'.
#' @param options Additional parameters passed to \code{\link{get_json}}.
#' @param ... other arguments. Deprecated.
#'
#' @return A list containing the parsed JSON response from PubChem. Returns NULL if an error or warning occurs.
#'
#' @export
#'
#' @importFrom RJSONIO fromJSON toJSON
#' @keywords internal
#'
#'
get_json <- function(identifier, namespace = 'cid', domain = 'compound', operation = NULL, searchtype = NULL, options = NULL, ...) {

  result <- tryCatch({
    get_pubchem(identifier, namespace, domain, operation, "JSON", searchtype, options)
  },
  error = function(e) {
    err.text <- c("Failed", e$message)
    names(err.text) <- c("Code", "Message")
    return(toJSON(list(Fault = err.text)))
  })

  result_list <- fromJSON(result)

  # If not failed with an error.
  if (is.null(result_list$Fault)){
    result_list[["success"]] <- TRUE

    result_list[["call_parameters"]] <- list(
      namespace = namespace,
      identifier = identifier,
      domain = domain
    )

    class(result_list[[1]]) <- "PubChemInstanceList"
    for (i in 1:length(result_list[[1]])){
      class(result_list[[1]][[i]]) <- "PubChemInstance"
    }

  } else {
    result_list[["success"]] <- FALSE
  }

  class(result_list) <- c("PubChemRequest")
  return(result_list)
}
