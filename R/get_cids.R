#' @title Retrieve Compound IDs (CIDs) from PubChem
#'
#' @description This function sends a request to PubChem to retrieve Compound IDs (CIDs) for given identifier(s).
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
#'                  - For \code{domain = 'substance'}:\code{sid}, \code{sourceid/<source id>}, \code{sourceall/<source name>}, \code{name}, etc.
#'
#'                  - For \code{domain = 'assay'}: \code{aid}, \code{listkey}, \code{type/<assay type>}, \code{sourceall/<source name>}, etc.
#'
#'                  For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Input}{Input} section of the PUG REST API.
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
#' @return An object of class 'PubChemInstance_CIDs', which is a list containing information retrieved from the PubChem database. Compound IDs can be extracted from the returned object using the \link{CIDs} function.
#'
#' @seealso \link{CIDs}, \link{get_pug_rest}
#'
#' @note
#' To extract compoud IDs from returned object, one may use \link{CIDs} function. See examples.
#'
#' @examples
#' \donttest{
#' compound <- get_cids(
#'   identifier = "aspirin",
#'   namespace = "name"
#' )
#'
#' compound
#'
#' # Extract compound IDs.
#' CIDs(compound)
#'}
#'
#' @export
get_cids <- function(identifier, namespace = 'name', domain = 'compound', searchtype = NULL, options = NULL) {

  # Initialize result list
  CIDs_List <- list(
    result = vector("list", length(identifier)),  # Create a list with the same length as identifiers
    request_args = list(
      namespace = namespace,
      identifier = identifier,
      domain = domain
    ),
    success = rep(FALSE, length(identifier)),  # Initialize success with FALSE for each identifier
    error = vector("character", length(identifier))  # Initialize an empty character vector for errors
  )

  # Attempt to get the response and handle errors gracefully
  result <- lapply(seq_along(identifier), function(i) {
    x <- identifier[i]
    # Use tryCatch to handle potential errors
    response <- tryCatch({
      # Attempt to retrieve data using the get_json function
      tmp <- get_json(identifier = x, namespace, domain, 'cids', searchtype, options)
      class(tmp) <- NULL
      CIDs_List$success[i] <- TRUE  # Set success to TRUE if request is successful
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

      # Append error message to CIDs_List$error at the corresponding index
      CIDs_List$error[i] <- error_message
      CIDs_List$success[i] <- FALSE  # Ensure success is set to FALSE for this identifier

      # Return NULL for this identifier to indicate failure
      NULL
    })
    return(response)
  })

  # Store the results in the CIDs_List object
  CIDs_List$result <- result

  # Return the structured object with results and error details
  structure(
    CIDs_List,
    class = c("PubChemInstance_CIDs")
  )
}


