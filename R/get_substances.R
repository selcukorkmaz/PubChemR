#' Retrieve Substances from PubChem
#'
#' This function sends a request to PubChem to retrieve substance data based on the specified parameters.
#' It returns a list of substances corresponding to the provided identifiers.
#'
#' @param identifier A character or numeric vector specifying the identifiers for the request.
#' @param namespace A character string specifying the namespace for the request. Default is 'sid'.
#' @param operation Specifies the operation to be performed on the input records. For the 'compound' domain, possible operations include 'record', 'property', 'synonyms', 'sids', 'cids', 'aids', 'assaysummary', 'classification', 'xrefs', and 'description'. The available operations are domain-specific.
#' @param searchtype Specifies the type of search to be performed. For structure searches, possible values are combinations of 'substructure', 'superstructure', 'similarity', 'identity' with 'smiles', 'inchi', 'sdf', 'cid'. For fast searches, possible values are combinations of 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure' with 'smiles', 'smarts', 'inchi', 'sdf', 'cid', or 'fastformula'.
#' @param options Additional parameters passed to \code{\link{get_json}}.
#'
#' @return A named list where each element corresponds to a substance retrieved from PubChem.
#'         The names of the list elements are based on the provided identifiers.
#'         If no substance is found for a given identifier, the corresponding list element will contain the string "No substance".
#'
#' @importFrom RJSONIO fromJSON
#' @export
#'
#' @examples
#' get_substances(
#'   identifier = "aspirin",
#'   namespace = "name"
#' )
get_substances <- function(identifier, namespace = 'sid', operation = NULL, searchtype = NULL, options = NULL) {

  substances <- list()

  for (i in 1:length(identifier)) {
    # Retrieve the JSON data
    results <- get_json(identifier[i], namespace, 'substance', operation = operation, searchtype = searchtype, options = options)

    # Check if results are not empty
    if (!is.null(results)) {
      # Create a list of substances (here, you might want to define what an 'substance' contains)

      if (!is.null(results$PC_Substances)) {
        substances[[i]] <- results$PC_Substances
      } else {
        substances[[i]] <- results
      }
    }
  }

  names(substances) <- paste0("Substance_", identifier)
  results <- substances

  return(results)
}


