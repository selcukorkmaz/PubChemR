#' @title Retrieve Compound IDs (CIDs) from PubChem
#'
#' @description This function sends a request to PubChem to retrieve Compound IDs (CIDs) for given identifier(s).
#'
#' @param identifier A vector of positive integers (e.g., cid, sid, aid) or identifier strings (e.g., source, inchikey, formula). In some cases, only a single identifier string is required (e.g., name, smiles, xref; inchi, sdf by POST only). Multiple elements can be included as a vector. See Notes for details.
#' @param namespace Specifies the namespace for the query. For the 'compound' domain, possible values include 'cid', 'name', 'smiles', 'inchi', 'sdf', 'inchikey', 'formula', 'substructure', 'superstructure', 'similarity', 'identity', 'xref', 'listkey', 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure', and 'fastformula'. For other domains, the possible namespaces are domain-specific.
#' @param domain Specifies the domain of the query. Possible values are 'substance', 'compound', 'assay', 'gene', 'protein', 'pathway', 'taxonomy', 'cell', 'sources', 'sourcetable', 'conformers', 'annotations', 'classification', and 'standardize'.
#' @param searchtype Specifies the type of search to be performed. For structure searches, possible values include combinations of 'substructure', 'superstructure', 'similarity', 'identity' with 'smiles', 'inchi', 'sdf', or 'cid'. For fast searches, possible values include combinations of 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure' with 'smiles', 'smarts', 'inchi', 'sdf', 'cid', or 'fastformula'.
#' @param options Additional arguments to be passed to the PubChem Database API.
#'
#' @return An object of class 'PubChemInstance_CIDs', which is a list containing information retrieved from the PubChem database. Compound IDs can be extracted from the returned object using the \link{CIDs} function.
#'
#' @seealso \link{CIDs}, \link{get_pug_rest}
#'
#' @note
#' To extract compoud IDs from returned object, one may use \link{CIDs} function. See examples.
#'
#' @examples
#' compound <- get_cids(
#'   identifier = "aspirin",
#'   namespace = "name"
#' )
#'
#' print(compound)
#'
#' # Extract compound IDs.
#' CIDs(compound)
#'
#' @export
get_cids <- function(identifier, namespace = 'name', domain = 'compound', searchtype = NULL, options = NULL) {

  result <- lapply(identifier, function(x){
    tmp <- get_json(identifier = x, namespace, domain, 'cids', searchtype, options)
    class(tmp) <- NULL
    return(tmp)
  })

  CIDs_List <- list(
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
    CIDs_List,
    class = c("PubChemInstance_CIDs")
  )
}

