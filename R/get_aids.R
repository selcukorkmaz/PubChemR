
#' Retrieve Assay IDs (AIDs) from PubChem
#'
#' This function sends a request to PubChem to retrieve Assay IDs (AIDs) for a given identifier.
#' It returns either a tibble (data frame) with the provided identifier and the corresponding AIDs
#' or a list of AIDs, depending on the `as_data_frame` parameter.
#'
#' @param identifier A vector of positive integers (e.g. cid, sid, aid) or identifier strings (source, inchikey, formula). In some cases, only a single identifier string (name, smiles, xref; inchi, sdf by POST only).
#' @param namespace Specifies the namespace for the query. For the 'compound' domain, possible values include 'cid', 'name', 'smiles', 'inchi', 'sdf', 'inchikey', 'formula', 'substructure', 'superstructure', 'similarity', 'identity', 'xref', 'listkey', 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure', and 'fastformula'. For other domains, the possible namespaces are domain-specific.
#' @param domain Specifies the domain of the query. Possible values are 'substance', 'compound', 'assay', 'gene', 'protein', 'pathway', 'taxonomy', 'cell', 'sources', 'sourcetable', 'conformers', 'annotations', 'classification', and 'standardize'.
#' @param searchtype Specifies the type of search to be performed. For structure searches, possible values are combinations of 'substructure', 'superstructure', 'similarity', 'identity' with 'smiles', 'inchi', 'sdf', 'cid'. For fast searches, possible values are combinations of 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure' with 'smiles', 'smarts', 'inchi', 'sdf', 'cid', or 'fastformula'.
#' @param as_data_frame A logical value indicating whether to return the results as a tibble (data frame). Default is TRUE.
#' @param options Additional arguments passed to \code{\link{get_json}}
#'
#' @return If `as_data_frame` is TRUE, a tibble (data frame) where each row corresponds to a provided identifier and its AID.
#'         The tibble has columns 'CID' and 'AID'. If `as_data_frame` is FALSE, a list of AIDs is returned.
#'
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @importFrom stringr str_to_title
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' get_aids(
#'   identifier = "aspirin",
#'   namespace = "name"
#' )
get_aids <- function(identifier, namespace = 'cid', domain = 'compound', searchtype = NULL, as_data_frame = TRUE, options = NULL) {

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
