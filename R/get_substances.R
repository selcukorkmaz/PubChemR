#' @title Retrieve Substances from PubChem
#'
#' @description This function sends a request to PubChem to retrieve substance data based on the specified parameters.
#'
#' @param identifier A vector of substance identifiers, either numeric or character.
#'                   The type of identifier depends on the \code{namespace} parameter.
#'                   **Note**: \code{identifier} must be provided; it cannot be \code{NULL}.
#' @param namespace A character string specifying the namespace of the identifier.
#'
#'                  Possible values include:
#'
#'                  - \code{sid}: PubChem Substance Identifier (default)
#'
#'                  - \code{sourceid/<source id>}: Source-specific substance ID
#'
#'                  - \code{sourceall/<source name>}: Source name
#'
#'                  - \code{name}: Substance name
#'
#'                  - \code{<xref>}: Cross-reference
#'
#'                  - \code{listkey}: A list key obtained from a previous query
#'
#'                  For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Input}{Input} section of the PUG REST API.
#'
#' @param operation A character string specifying the operation to perform.
#'
#'                  Possible values include:
#'
#'                  - \code{record}: Retrieve the full substance record (default)
#'
#'                  - \code{synonyms}: Retrieve synonyms for the substances
#'
#'                  - \code{sids}: Retrieve Substance IDs
#'
#'                  - \code{cids}: Retrieve Compound IDs related to the substances
#'
#'                  - \code{aids}: Retrieve Assay IDs related to the substances
#'
#'                  - \code{assaysummary}: Retrieve assay summary
#'
#'                  - \code{classification}: Retrieve substance classification
#'
#'                  - \code{<xrefs>}: Retrieve cross-references
#'
#'                  - \code{description}: Retrieve substance descriptions
#'
#'                  If \code{NULL} (default), the operation defaults to \code{record}.
#'
#'                  For a full list of operations, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Operation}{Operations} section of the PUG REST API.
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
#' @return An object of class 'PubChemInstanceList' containing all the substance information of requested compounds.
#'
#' @importFrom RJSONIO fromJSON
#'
#' @examples
#' \donttest{
#' subs <- get_substances(
#'   identifier = c("aspirin", "ibuprofen"),
#'   namespace = "name"
#' )
#'
#' instance(subs, "aspirin")
#' retrieve(instance(subs, "aspirin"), "source")
#' }
#'
#' @export
get_substances <- function(identifier, namespace = 'sid', operation = NULL, options = NULL) {

  # Initialize result list
  Substances_List <- list(
    result = vector("list", length(identifier)),  # Create a list with the same length as identifiers
    request_args = list(
      namespace = namespace,
      identifier = identifier,
      domain = "substance"
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
      tmp <- get_json(identifier = x, namespace, "substance", operation, NULL, options)
      class(tmp) <- c("PC_Substance", class(tmp))  # Add custom class to response
      Substances_List$success[i] <- TRUE  # Set success to TRUE if request is successful
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

      # Append error message to Substances_List$error at the corresponding index
      Substances_List$error[i] <- error_message
      Substances_List$success[i] <- FALSE  # Ensure success is set to FALSE for this identifier

      # Return NULL for this identifier to indicate failure
      NULL
    })
    return(response)
  })

  # Store the results in the Substances_List object
  Substances_List$result <- result

  # Return the structured object with results and error details
  structure(
    Substances_List,
    class = c("PubChemInstanceList")
  )
}


