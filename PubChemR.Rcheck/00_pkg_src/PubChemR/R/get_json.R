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
#' @importFrom RJSONIO fromJSON
#'
#' @keywords internal
#'
#' @noRd
get_json <- function(identifier, namespace = 'cid', domain = 'compound', operation = NULL, searchtype = NULL, options = NULL, ...) {

  req_args <- list(
    namespace = namespace,
    identifier = identifier,
    domain = domain,
    operation = operation,
    searchtype = searchtype,
    options = options
  )

  raw_json <- tryCatch(
    get_pubchem(identifier, namespace, domain, operation, "JSON", searchtype, options),
    error = function(e) {
      return(pc_make_failed_instance(
        request_args = req_args,
        code = "RequestFailed",
        message = conditionMessage(e),
        error_class = "request_error"
      ))
    }
  )

  if (inherits(raw_json, "PubChemInstance")) {
    return(raw_json)
  }

  result <- tryCatch(
    fromJSON(raw_json),
    error = function(e) {
      return(pc_make_failed_instance(
        request_args = req_args,
        code = "ParseError",
        message = paste0("Failed to parse PubChem JSON: ", conditionMessage(e)),
        details = list(raw = raw_json),
        error_class = "parse_error"
      ))
    }
  )

  if (inherits(result, "PubChemInstance")) {
    return(result)
  }

  if (!is.list(result)) {
    return(pc_make_failed_instance(
      request_args = req_args,
      code = "ParseError",
      message = "Parsed PubChem response is not a list.",
      details = list(raw = raw_json),
      error_class = "parse_error"
    ))
  }

  if (!is.null(result$Fault)) {
    return(pc_make_failed_instance(
      request_args = req_args,
      code = ifelse(is.null(result$Fault$Code), "PubChemFault", result$Fault$Code),
      message = pc_format_fault(result$Fault),
      details = result$Fault$Details,
      error_class = "pubchem_fault"
    ))
  }

  structure(
    list(
      result = result,
      request_args = pc_drop_null(req_args),
      success = TRUE,
      error = NULL
    ),
    class = "PubChemInstance"
  )
}
