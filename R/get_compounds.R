#' Retrieve Compounds from PubChem
#'
#' This function sends a request to PubChem to retrieve compound data based on the specified parameters.
#' It returns a list of compounds corresponding to the provided identifiers.
#'
#' @param identifier A vector of positive integers (e.g. cid, sid, aid) or identifier strings (source, inchikey, formula). In some cases, only a single identifier string (name, smiles, xref; inchi, sdf by POST only).
#' @param namespace Specifies the namespace for the query. For the 'compound' domain, possible values include 'cid', 'name', 'smiles', 'inchi', 'sdf', 'inchikey', 'formula', 'substructure', 'superstructure', 'similarity', 'identity', 'xref', 'listkey', 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure', and 'fastformula'. For other domains, the possible namespaces are domain-specific.
#' @param operation The operation to be performed (default: NULL).
#' @param searchtype Specifies the type of search to be performed. For structure searches, possible values are combinations of 'substructure', 'superstructure', 'similarity', 'identity' with 'smiles', 'inchi', 'sdf', 'cid'. For fast searches, possible values are combinations of 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure' with 'smiles', 'smarts', 'inchi', 'sdf', 'cid', or 'fastformula'.
#' @param options Additional parameters passed to \code{\link{get_json}}.
#'
#' @return A named list where each element corresponds to a compound retrieved from PubChem.
#'         The names of the list elements are based on the provided identifiers.
#'         If no compound is found for a given identifier, the corresponding list element will contain the string "No compound".
#'
#' @export
#'
#' @examples
#' get_compounds(
#'   identifier = "aspirin",
#'   namespace = "name"
#' )
get_compounds <- function(identifier, namespace = 'cid', operation = NULL, searchtype = NULL, options = NULL) {

    compounds <- list()

  for (i in 1:length(identifier)) {
    # Retrieve the JSON data
    results <- get_json(identifier[i], namespace, operation = operation, searchtype = searchtype, options = options)

    # Check if results are not empty
    if (!is.null(results)) {
      # Create a list of compounds (here, you might want to define what an 'compound' contains)

    if (!is.null(results$PC_Compounds)) {
        compounds[[i]] <- results$PC_Compounds
      } else {
        compounds[[i]] <- results
      }
    }}

    if(length(compounds) > 1){

      names(compounds) <- paste0("'", identifier, "'")
      results <- compounds

      return(results)
  }
}

