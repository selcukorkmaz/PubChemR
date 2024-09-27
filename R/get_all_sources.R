#' Retrieve All Sources from PubChem
#'
#' This function retrieves a list of all current depositors of substances or assays from PubChem.
#'
#' @param domain A character string specifying the domain for which sources are to be retrieved.
#'
#'               Possible values are:
#'
#'               - `'substance'` (default)
#'
#'               - `'assay'`
#'
#' @return A character vector containing the names of all sources for the specified domain.
#'
#' @details
#' The PubChem PUG REST API provides a way to retrieve all current depositors (sources) for substances or assays.
#' For more detailed information, please refer to the
#' \href{https://pubchem.ncbi.nlm.nih.gov/sources}{PubChem Data Sources documentation}.
#'
#'
#' @importFrom RJSONIO fromJSON
#' @export
#'
#' @examples
#' get_all_sources(
#'   domain = 'substance'
#' )
get_all_sources <- function(domain = 'substance') {
  # Use the get function to retrieve the sources
  response_content <- get_pubchem(identifier = domain, namespace = NULL, domain = 'sources')

  # Convert the response content to a list using fromJSON
  results <- fromJSON(response_content)

  # Return the SourceName from the results
  if ("InformationList" %in% names(results) && "SourceName" %in% names(results$InformationList)) {
    return(results$InformationList$SourceName)
  } else {
    stop("Error: SourceName not found in the response.")
  }
}
