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
  # Validate the domain parameter
  if (!domain %in% c("substance", "compound", "assay")) {
    stop("Invalid domain specified. Please choose from 'substance', 'compound', or 'assay'.")
  }

  # Attempt to retrieve the sources using the get_pubchem function
  response_content <- tryCatch({
    get_pubchem(identifier = domain, namespace = NULL, domain = 'sources')
  }, error = function(e) {
    stop(paste("Failed to retrieve sources for the specified domain:", conditionMessage(e)))
  })

  # Validate the response content
  if (is.null(response_content) || nchar(response_content) == 0) {
    stop("Error: No response content received from the PubChem server.")
  }

  # Attempt to parse the JSON response content
  results <- tryCatch({
    fromJSON(response_content)
  }, error = function(e) {
    stop(paste("Failed to parse JSON response:", conditionMessage(e)))
  })

  # Check if 'SourceName' exists in the parsed response
  if ("InformationList" %in% names(results) && "SourceName" %in% names(results$InformationList)) {
    return(results$InformationList$SourceName)
  } else {
    stop("Error: 'SourceName' not found in the response. Check the response structure.")
  }
}
