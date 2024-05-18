# Global Variables and/or Functions
utils::globalVariables(c("data", "CID"))


#' Retrieve Compound IDs (CIDs) from PubChem
#'
#' This function sends a request to PubChem to retrieve Compound IDs (CIDs) for a given identifier.
#' It returns a tibble (data frame) with the provided identifier and the corresponding CIDs.
#'
#' @param identifier A vector of positive integers (e.g. cid, sid, aid) or identifier strings (source, inchikey, formula). In some cases, only a single identifier string (name, smiles, xref; inchi, sdf by POST only).
#' @param namespace Specifies the namespace for the query. For the 'compound' domain, possible values include 'cid', 'name', 'smiles', 'inchi', 'sdf', 'inchikey', 'formula', 'substructure', 'superstructure', 'similarity', 'identity', 'xref', 'listkey', 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure', and 'fastformula'. For other domains, the possible namespaces are domain-specific.
#' @param domain Specifies the domain of the query. Possible values are 'substance', 'compound', 'assay', 'gene', 'protein', 'pathway', 'taxonomy', 'cell', 'sources', 'sourcetable', 'conformers', 'annotations', 'classification', and 'standardize'.
#' @param searchtype Specifies the type of search to be performed. For structure searches, possible values are combinations of 'substructure', 'superstructure', 'similarity', 'identity' with 'smiles', 'inchi', 'sdf', 'cid'. For fast searches, possible values are combinations of 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure' with 'smiles', 'smarts', 'inchi', 'sdf', 'cid', or 'fastformula'.
#' @param options Additional arguments passed to \code{\link{get_json}}.
#'
#' @return A tibble (data frame) where each row corresponds to a provided identifier and its CID.
#'         The tibble has columns 'Compound' and 'CID'.
#'
#' @importFrom tibble tibble
#' @importFrom dplyr mutate row_number select
#' @importFrom tidyr unnest_wider unnest_longer
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' get_cids(
#'   identifier = "aspirin",
#'   namespace = "name"
#' )
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

