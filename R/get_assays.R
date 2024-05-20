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
#' @importFrom purrr keep map
#' @importFrom tibble as_tibble enframe
#' @importFrom tidyr unnest_wider
#' @export
#'
#' @examples
#' get_assays(
#'   identifier = c(1234, 7815),
#'   namespace = 'aid',
#'   to_dataframe = TRUE
#' )
get_assays <- function(identifier, namespace = 'aid', operation = NULL, searchtype = NULL, options = NULL) {

  # identifier = c(1234, 7815, "ssda")
  # namespace = 'aid'
  # operation = NULL
  # searchtype = NULL
  # options = NULL
  # to_dataframe = TRUE

  # Try to get the response and parse JSON
  # Assuming 'get_json' is a function you've previously defined, similar to your Python environment
  result <- lapply(identifier, function(x){
    tmp <- get_json(identifier = x, namespace, 'assay', 'description', searchtype, options)
    class(tmp) <- c(class(tmp), "PC_Assay")
    return(tmp)
  })

  Assays_List <- list(
    result = result,
    request_args = list(
      namespace = namespace,
      identifier = identifier,
      domain = "assay",
      operation = "description"
    ),
    success = logical(),
    error = NULL
  )

  structure(
    Assays_List,
    class = c("PubChemInstanceList")
  )
}
