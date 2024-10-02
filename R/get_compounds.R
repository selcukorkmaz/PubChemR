#' @title Retrieve Compounds from PubChem
#'
#' @description This function sends a request to the PubChem database to retrieve compound data based on specified parameters.
#'
#' @param identifier A vector of positive integers (e.g., cid, sid, aid) or identifier strings (source, inchikey, formula).
#'                   In some cases, a single identifier string (e.g., name, smiles, xref; inchi, sdf by POST only) is sufficient.
#'                   **Note**: \code{identifier} must be provided; it cannot be \code{NULL}.
#' @param namespace A character string specifying the namespace of the identifier.
#'
#'                  Possible values include:
#'
#'                  - \code{cid}: PubChem Compound Identifier (default)
#'
#'                  - \code{name}: Chemical name
#'
#'                  - \code{smiles}: SMILES string
#'
#'                  - \code{inchi}: InChI string
#'
#'                  - \code{inchikey}: InChIKey
#'
#'                  - \code{formula}: Molecular formula
#'
#'                  For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Input}{Input} section of the PUG REST API.
#'
#' @param operation A character string specifying the operation to perform.
#'
#'                  Possible values include:
#'
#'                  - \code{synonyms}: Retrieve synonyms for the compounds.
#'
#'                  - \code{description}: Retrieve compound descriptions.
#'
#'                  - \code{sids}: Retrieve Substance IDs related to the compounds.
#'
#'                  - \code{aids}: Retrieve Assay IDs related to the compounds.
#'
#'                  - \code{classification}: Retrieve compound classification.
#'
#'                  - \code{cids}: Retrieve Compound IDs (used when the input is not CID).
#'
#'                  If \code{NULL} (default), the basic compound record is retrieved.
#'
#'                  For a full list of operations, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Operation}{Operations} section of the PUG REST API.
#'
#' @param searchtype An optional character string specifying the search type.
#'
#'                   Possible values include:
#'
#'                   - \code{similarity}
#'
#'                   - \code{substructure}
#'
#'                   - \code{superstructure}
#'
#'                   - \code{identity}
#'
#'                   If \code{NULL} (default), no search type is specified.
#'
#'                  For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Input}{Input} section of the PUG REST API.
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

  # Initialize result list
  Compounds_List <- list(
    result = vector("list", length(identifier)),  # Create a list with the same length as identifiers
    request_args = list(
      namespace = namespace,
      identifier = identifier,
      domain = "compound",
      operation = operation,
      options = options,
      searchtype = searchtype
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
      tmp <- get_json(identifier = x, namespace = namespace, operation = operation, searchtype = searchtype, options = options)
      class(tmp) <- c(class(tmp), "PC_Compounds")  # Add custom class to response
      Compounds_List$success[i] <- TRUE  # Set success to TRUE if request is successful
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

      # Append error message to Compounds_List$error at the corresponding index
      Compounds_List$error[i] <- error_message
      Compounds_List$success[i] <- FALSE  # Ensure success is set to FALSE for this identifier

      # Return NULL for this identifier to indicate failure
      NULL
    })
    return(response)
  })

  # Store the results in the Compounds_List object
  Compounds_List$result <- result

  # Return the structured object with results and error details
  structure(
    Compounds_List,
    class = c("PubChemInstanceList")
  )
}

