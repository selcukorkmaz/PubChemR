#' @title Retrieve Assays from PubChem
#'
#' @description This function sends a request to PubChem to retrieve assay data based on the specified parameters.
#'
#' @param identifier A vector of positive integers (e.g., \code{cid}, \code{sid}, \code{aid}) or identifier strings (source, \code{inchikey}, \code{formula}).
#'                   In some cases, only a single identifier string (e.g., \code{name}, \code{smiles}, \code{xref}; \code{inchi}, \code{sdf} by POST only) can be provided.
#'                   Multiple elements can be included as a vector. See Notes for details.
#' @param namespace A character string specifying the namespace of the identifier.
#'
#'                  Possible values include:
#'
#'                  - \code{aid}: PubChem Assay Identifier (default)
#'
#'                  - \code{listkey}: A list key obtained from a previous query
#'
#'                  - \code{type/<assay type>}: Specify the assay type
#'
#'                  - \code{sourceall/<source name>}: Specify the source name
#'
#'                  - \code{target/<assay target>}: Specify the assay target
#'
#'                  - \code{activity/<activity column name>}: Specify the activity column name
#'
#'                  For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Input}{Input} section of the PUG REST API.
#'
#' @param operation A character string specifying the operation to perform.
#'
#'                  Possible values include:
#'
#'                  - \code{record}: Retrieve the full assay record
#'
#'                  - \code{concise}: Retrieve a concise summary of the assay
#'
#'                  - \code{aids}: Retrieve related Assay IDs
#'
#'                  - \code{sids}: Retrieve related Substance IDs
#'
#'                  - \code{cids}: Retrieve related Compound IDs
#'
#'                  - \code{description}: Retrieve assay descriptions (default)
#'
#'                  - \code{targets/<target type>}: Retrieve targets of the assay
#'
#'                  - \code{summary}: Retrieve a summary of the assay
#'
#'                  - \code{classification}: Retrieve assay classification
#'
#'                  If \code{NULL} (default), the operation defaults to \code{description}.
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
#' @return An object of class 'PubChemInstanceList' containing the information retrieved from the PubChem database.
#'
#' @note To extract information about a specific assay from the returned list, use the \link{instance} function.
#'
#' Each assay may include information on several properties. Specific information from the assay can be extracted using the \link{retrieve} function. See examples.
#'
#' @seealso \link{retrieve}, \link{instance}
#'
#' @examples
#' # Retrieve a list of assays from the PubChem database
#' assays <- get_assays(
#'   identifier = c(1234, 7815),
#'   namespace = 'aid'
#' )
#'
#' # Return assay information for assay ID '1234'
#' assay1234 <- instance(assays, "1234")
#' print(assay1234)
#'
#' # Retrieve specific elements from the assay output
#' retrieve(assay1234, "aid")
#'
#' @export
get_assays <- function(identifier, namespace = 'aid', operation = NULL, options = NULL) {

  # Initialize result list
  Assays_List <- list(
    result = vector("list", length(identifier)),  # Create a list with the same length as identifiers
    request_args = list(
      namespace = namespace,
      identifier = identifier,
      domain = "assay",
      operation = "description"
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
      tmp <- get_json(identifier = x, namespace, 'assay', 'description', NULL, options)
      class(tmp) <- c(class(tmp), "PC_Assay")  # Add custom class to response
      Assays_List$success[i] <- TRUE  # Set success to TRUE if request is successful
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

      # Append error message to Assays_List$error at the corresponding index
      Assays_List$error[i] <- error_message
      Assays_List$success[i] <- FALSE  # Ensure success is set to FALSE for this identifier

      # Return NULL for this identifier to indicate failure
      NULL
    })
    return(response)
  })

  # Store the results in the Assays_List object
  Assays_List$result <- result

  # Return the structured object with results and error details
  structure(
    Assays_List,
    class = c("PubChemInstanceList")
  )
}

