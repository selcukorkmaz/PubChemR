#' @title Retrieve Substance IDs (SIDs) from PubChem
#'
#' @description This function sends a request to PubChem to retrieve Substance IDs (SIDs) for a given identifier.
#'
#' @param identifier A numeric or character vector specifying the identifiers for the request.
#' @param namespace A character string specifying the namespace for the request. Default is 'cid'.
#' @param domain A character string specifying the domain for the request. Default is 'compound'.
#' @param searchtype A character string specifying the search type. Default is NULL.
#' @param options Additional arguments passed to the PubChem request.
#'
#' @return An object of class 'PubChemInstance_SIDs', which is a list containing information retrieved from the PubChem database. Substance IDs can be extracted from the returned object using the \link{SIDs} function.
#'
#' @examples
#' result <- get_sids(
#'   identifier = c("aspirin", "ibuprofen"),
#'   namespace = "name"
#' )
#'
#' # Extract substance IDs of all compounds
#' SIDs(result)
#'
#' @export
get_sids <- function(identifier, namespace = 'cid', domain = 'compound', searchtype = NULL, options = NULL) {

  # Try to get the response and parse JSON
  # Assuming 'get_json' is a function you've previously defined, similar to your Python environment
  result <- lapply(identifier, function(x){
    tmp <- get_json(identifier = x, namespace, domain, 'sids', searchtype, options)
    class(tmp) <- NULL
    return(tmp)
  })

  SIDs_List <- list(
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
    SIDs_List,
    class = c("PubChemInstance_SIDs")
  )
}

