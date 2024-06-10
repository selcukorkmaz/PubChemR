#' @title Retrieve Compounds from PubChem
#'
#' @description This function sends a request to the PubChem database to retrieve compound data based on specified parameters.
#'
#' @param identifier A vector of positive integers (e.g., cid, sid, aid) or identifier strings (source, inchikey, formula). In some cases, a single identifier string (e.g., name, smiles, xref; inchi, sdf by POST only) is sufficient.
#' @param namespace Specifies the namespace for the query. For the 'compound' domain, possible values include 'cid', 'name', 'smiles', 'inchi', 'sdf', 'inchikey', 'formula', 'substructure', 'superstructure', 'similarity', 'identity', 'xref', 'listkey', 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure', and 'fastformula'. For other domains, the possible namespaces are domain-specific.
#' @param operation The operation to be performed (default: NULL).
#' @param searchtype Specifies the type of search to be performed. For structure searches, possible values are combinations of 'substructure', 'superstructure', 'similarity', 'identity' with 'smiles', 'inchi', 'sdf', 'cid'. For fast searches, possible values are combinations of 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure' with 'smiles', 'smarts', 'inchi', 'sdf', 'cid', or 'fastformula'.
#' @param options Additional parameters passed to \code{get_json}.
#'
#' @return An object of class 'PubChemInstanceList' and 'PC_Compounds' containing compound information from the PubChem database.
#'
#' @seealso \link{retrieve}, \link{instance}
#'
#' @examples
#' compound <- get_compounds(
#'   identifier = c("aspirin", "ibuprofen", "rstudio"),
#'   namespace = "name"
#' )
#'
#' print(compound)
#'
#' # Return results for selected compound.
#' instance(compound, "aspirin")
#' instance(compound, "rstudio")
#' # instance(compound, "unknown"). # returns error.
#'
#' # Extract compound properties for the compound "aspirin".
#' # Use the 'retrieve()' function to extract specific slots from the compound list.
#' retrieve(instance(compound, "aspirin"), "props")
#'
#' @export
get_compounds <- function(identifier, namespace = 'cid', operation = NULL, searchtype = NULL, options = NULL) {
  # Try to get the response and parse JSON
  # Assuming 'get_json' is a function you've previously defined, similar to your Python environment
  result <- lapply(identifier, function(x){
    tmp <- get_json(identifier = x, namespace, operation = operation, searchtype = searchtype, options = options)
    class(tmp) <- c(class(tmp), "PC_Compounds")
    return(tmp)
  })

  Compounds_List <- list(
    result = result,
    request_args = list(
      namespace = namespace,
      identifier = identifier,
      domain = "compound",
      operation = "description",
      options = options,
      searchtype = searchtype
    ),
    success = logical(),
    error = NULL
  )

  structure(
    Compounds_List,
    class = c("PubChemInstanceList")
  )
}
