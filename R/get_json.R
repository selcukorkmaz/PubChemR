#' @title Retrieve JSON Data from PubChem
#'
#' @description This function sends a request to PubChem to retrieve JSON data based on specified parameters.
#' It handles errors and warnings gracefully, providing informative messages when they occur.
#' This function is used internally by all get_* functions, and users will not typically run this function directly.
#'
#' @param identifier A vector of identifiers, either numeric or character.
#'                   The type of identifier depends on the \code{namespace} and \code{domain} parameters.
#'                   **Note**: \code{identifier} must be provided and cannot be \code{NULL}.
#' @param namespace A character string specifying the namespace of the identifier.
#'
#'                  Possible values depend on the \code{domain} parameter.
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
#' @param operation A character string specifying the operation to perform.
#'
#'                  Possible values depend on the \code{domain} parameter.
#'
#'                  For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Operation}{Operations} section.
#'
#' @param searchtype An optional character string specifying the search type.
#'
#'                   Possible values depend on the \code{namespace} and \code{domain}.
#'
#'                   For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest}{PUG REST API documentation}.
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
#' @param ... Additional arguments passed to the underlying HTTP request functions.
#'
#' @details
#' This function constructs the appropriate API call to the PubChem PUG REST service and parses the JSON response.
#' It is designed to be an internal helper function and is not exported.
#'
#' @return A list containing the parsed JSON response from PubChem. Returns NULL if an error or warning occurs.
#'
#' @importFrom RJSONIO fromJSON toJSON
#'
#' @keywords internal
#'
#' @noRd
get_json <- function(identifier, namespace = 'cid', domain = 'compound', operation = NULL, searchtype = NULL, options = NULL, ...) {

  PubChemList <- list(
    result = list(),
    request_args = list(
      namespace = namespace,
      identifier = identifier,
      domain = domain
    ),
    success = logical(),
    error = NULL
  )

  result <- fromJSON({
    tryCatch({
      get_pubchem(identifier, namespace, domain, operation, "JSON", searchtype, options)
    },
    error = function(e) {
      err.text <- c("Failed", e$message)
      names(err.text) <- c("Code", "Message")
      return(toJSON(list(Fault = err.text)))
    })
  })

  # If not failed with an error.
  if (is.null(result$Fault)){
    PubChemList[["result"]] <- result
    PubChemList[["success"]] <- TRUE
  } else {
    PubChemList[["success"]] <- FALSE

    # Command as a string
    command_string <- result$Fault["Message"]

    # Parsing and evaluating the command string
    error_details <- eval(parse(text = command_string))

    PubChemList[["error"]] <- error_details
  }

  structure(
    PubChemList,
    class = "PubChemInstance"
  )
}
