#' Retrieve Assays from PubChem
#'
#' This function sends a request to PubChem to retrieve assay data based on the specified parameters.
#' It returns a list of assays corresponding to the provided identifiers.
#'
#' @param identifier A vector of positive integers (e.g. cid, sid, aid) or identifier strings (source, inchikey, formula). In some cases, only a single identifier string (name, smiles, xref; inchi, sdf by POST only).
#' @param namespace Specifies the namespace for the query. For the 'compound' domain, possible values include 'cid', 'name', 'smiles', 'inchi', 'sdf', 'inchikey', 'formula', 'substructure', 'superstructure', 'similarity', 'identity', 'xref', 'listkey', 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure', and 'fastformula'. For other domains, the possible namespaces are domain-specific.
#' @param operation The operation to be performed (default: NULL).
#' @param searchtype The type of search to be performed (default: NULL).
#' @param options Additional parameters. Currently has no effect on the results.
#'
#' @return A named list where each element corresponds to an assay retrieved from PubChem.
#'         The names of the list elements are based on the provided identifiers.
#'         If no assay is found for a given identifier, the corresponding list element will contain the string "No assay".
#'
#' @export
#'
#' @examples
#' get_assays(
#'   identifier = 1234,
#'   namespace = "aid"
#' )
get_assays <- function(identifier, namespace = 'aid', operation = NULL, searchtype = NULL, options = NULL) {

  assays <- list()
  data <- list()

  for (i in 1:length(identifier)) {
    # Retrieve the JSON data
    results <- get_json(identifier[i], namespace, 'assay', 'description', searchtype, options)

    # Check if results are not empty
    if (!is.null(results)) {
      # Create a list of assays (here, you might want to define what an 'Assay' contains)
      if (!is.null(results$PC_AssayContainer)) {
        assays[[i]] <- results$PC_AssayContainer
      } else {
        assays[[i]] <- "No assay"
      }
    }}

    names(assays) <- paste0("'", identifier, "'")
    results <- assays

    return(results)
  }
