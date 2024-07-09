
#' @title Retrieve Assay IDs (AIDs) from PubChem
#'
#' @description
#' This function queries the PubChem database to retrieve Assay IDs (AIDs) based on a given identifier.
#'
#' @param identifier A vector of positive integers (e.g., cid, sid, aid) or identifier strings (source, inchikey, formula). In some cases, it may be a single identifier string (e.g., name, smiles, xref; inchi, sdf by POST only). Multiple elements can be included as a vector. See Notes for details.
#' @param namespace Specifies the namespace for the query. For the 'compound' domain, possible values include 'cid', 'name', 'smiles', 'inchi', 'sdf', 'inchikey', 'formula', 'substructure', 'superstructure', 'similarity', 'identity', 'xref', 'listkey', 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure', and 'fastformula'. For other domains, the possible namespaces are domain-specific.
#' @param domain Specifies the domain of the query. Possible values are 'substance', 'compound', assay', 'gene', 'protein', 'pathway', 'taxonomy', 'cell', 'sources', 'sourcetable', 'conformers', 'annotations', 'classification', and 'standardize'.
#' @param searchtype Specifies the type of search to be performed. For structure searches, possible values are combinations of 'substructure', 'superstructure', 'similarity', 'identity' with 'smiles', 'inchi', 'sdf', 'cid'. For fast searches, possible values are combinations of 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure' with 'smiles', 'smarts', 'inchi', 'sdf', 'cid', or 'fastformula'.
#' @param options Additional arguments to be passed to the PubChem Database API.
#'
#' @return An object of class 'PubChemInstance_AIDs', which is a list containing information retrieved from the PubChem database. Assay IDs can be extracted from the returned object using the getter function \link{AIDs}.
#'
#' @note
#' To extract assay IDs from returned object, one may use \link{AIDs} function. See examples.
#'
#' @seealso \link{AIDs}, \link{get_pug_rest}
#'
#' @examples
#' # Request for multiple assays
#' # If assay identifier is unknown or incorrect, an error returns from PubChem Database
#' aids <- get_aids(
#'   identifier = c("aspirin", "ibuprofen", "rstudio"),
#'   namespace = "name"
#' )
#'
#' print(aids)
#'
#' # Return all Assay IDs.
#' AIDs(aids)
#'
#' @export
get_aids <- function(identifier, namespace = 'cid', domain = 'compound', searchtype = NULL, options = NULL) {
  # Try to get the response and parse JSON
  # Assuming 'get_json' is a function you've previously defined, similar to your Python environment
  result <- lapply(identifier, function(x){
    tmp <- get_json(identifier = x, namespace, domain, 'aids', searchtype, options)
    class(tmp) <- NULL
    return(tmp)
  })

  AIDs_List <- list(
    result = result,
    request_args = list(
      namespace = namespace,
      identifier = identifier,
      domain = domain
    ),
    success = logical(),
    error = NULL
  )

  structure(
    AIDs_List,
    class = c("PubChemInstance_AIDs")
  )
}
