#' Retrieve Substance IDs (SIDs) from PubChem
#'
#' This function sends a request to PubChem to retrieve Substance IDs (SIDs) for a given identifier.
#' It returns a tibble (data frame) with the provided identifier and the corresponding SIDs.
#'
#' @param identifier A numeric or character vector specifying the identifiers for the request.
#' @param namespace A character string specifying the namespace for the request. Default is 'cid'.
#' @param domain A character string specifying the domain for the request. Default is 'compound'.
#' @param searchtype A character string specifying the search type. Default is NULL.
#' @param options Additional arguments passed to \code{\link{get_json}}.
#'
#' @return A tibble (data frame) where each row corresponds to a provided identifier and its SID.
#'         The tibble has columns 'CID' and 'SID'.
#'
#' @importFrom RJSONIO fromJSON
#' @importFrom dplyr bind_rows
#' @importFrom tidyr as_tibble
#' @importFrom stringr str_to_title
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' get_sids(
#'   identifier = "aspirin",
#'   namespace = "name"
#' )
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

