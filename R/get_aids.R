
#' @title Retrieve Assay IDs (AIDs) from PubChem
#'
#' @description
#' This function queries the PubChem database to retrieve Assay IDs (AIDs) based on a given identifier.
#'
#' @param identifier A vector of identifiers, either numeric or character.
#'                   The type of identifier depends on the \code{namespace} and \code{domain} parameters.
#'                   **Note**: \code{identifier} must be provided; it cannot be \code{NULL}.
#' @param namespace A character string specifying the namespace of the identifier.
#'
#'                  Possible values depend on the \code{domain} parameter and include:
#'
#'                  - For \code{domain = 'compound'}: \code{cid}, \code{name}, \code{smiles}, \code{inchi}, \code{sdf}, \code{inchikey}, \code{formula}, etc.
#'
#'                  - For \code{domain = 'substance'}: \code{sid}, \code{sourceid/<source id>}, \code{sourceall/<source name>}, \code{name}, etc.
#'
#'                  - For \code{domain = 'assay'}: \code{aid}, \code{listkey}, \code{type/<assay type>}, \code{sourceall/<source name>}, etc.
#'
#'                  For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Input}{Input} section.
#'
#' @param domain A character string specifying the domain of the query.
#'
#'               Possible values are:
#'
#'               - \code{compound} (default)
#'
#'               - \code{substance}
#'
#'               - \code{assay}
#'
#'               - Other domains as specified in the API documentation.
#'
#' @param searchtype An optional character string specifying the search type.
#'
#'                   Possible values depend on the \code{namespace} and \code{domain}.
#'
#'                   Examples include:
#'
#'                   - \code{substructure}, \code{superstructure}, \code{similarity}, \code{identity} for structure searches.
#'
#'                   - \code{fastidentity}, \code{fastsimilarity_2d}, \code{fastsimilarity_3d}, etc. for fast searches.
#'
#'                   If \code{NULL} (default), no search type is specified.
#'
#' @param options A list of additional options for the request.
#'
#'                Available options depend on the specific request and the API.
#'
#'                Examples include:
#'
#'                - For similarity searches: \code{list(Threshold = 95)}
#'
#'                - For substructure searches: \code{list(MaxRecords = 100)}
#'
#'                If \code{NULL} (default), no additional options are included.
#'
#'                For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Structure-Search-Operations}{Structure Search Operations} section of the PUG REST API.
#'
#' @details
#' For more detailed information, please refer to the
#' \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest}{PubChem PUG REST API documentation}.
#'
#'
#' @return An object of class 'PubChemInstance_AIDs', which is a list containing information retrieved from the PubChem database. Assay IDs can be extracted from the returned object using the getter function \link{AIDs}.
#'
#' @note
#' To extract assay IDs from returned object, one may use \link{AIDs} function. See examples.
#'
#' @seealso \link{AIDs}, \link{get_pug_rest}
#'
#' @examples
#' \donttest{
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
#'}
#' @export
get_aids <- function(identifier, namespace = 'cid', domain = 'compound', searchtype = NULL, options = NULL) {

  # Initialize result list
  AIDs_List <- list(
    result = list(),
    request_args = list(
      namespace = namespace,
      identifier = identifier,
      domain = domain
    ),
    success = logical(),
    error = NULL
  )

  # Attempt to get the response and handle errors gracefully
  result <- lapply(identifier, function(x) {
    # Use tryCatch to handle potential errors
    response <- tryCatch({
      # Attempt to retrieve data using the get_json function
      tmp <- get_json(identifier = x, namespace, domain, 'aids', searchtype, options)
      class(tmp) <- NULL
      AIDs_List$success <- c(AIDs_List$success, TRUE)
      tmp
    }, error = function(e) {
      # Capture the error message
      error_message <- conditionMessage(e)

      # Determine the error type and assign an appropriate message
      if (grepl("Timeout", error_message, ignore.case = TRUE)) {
        error_message <- paste0("Request timeout: The server did not respond in time for identifier '", x, "'. Please try again later.")
      } else if (grepl("Could not resolve host", error_message, ignore.case = TRUE) ||
                 grepl("InternetOpenUrl", error_message, ignore.case = TRUE)) {
        error_message <- "Network error: Could not connect to the server. Please check your internet connection and try again."
      } else if (grepl("HTTP error", error_message, ignore.case = TRUE)) {
        error_message <- paste0("HTTP error: The server returned an error for identifier '", x, "'. Please check the server status or try again later.")
      } else {
        error_message <- paste0("An unknown error occurred for identifier '", x, "': ", error_message)
      }

      # Append error message to AIDs_List$error
      AIDs_List$error <- c(AIDs_List$error, error_message)
      AIDs_List$success <- c(AIDs_List$success, FALSE)

      # Return NULL for this identifier to indicate failure
      NULL
    })
    return(response)
  })

  # Store the results in the AIDs_List object
  AIDs_List$result <- result

  # Return the structured object with results and error details
  structure(
    AIDs_List,
    class = c("PubChemInstance_AIDs")
  )
}
