#' @title Request Function for PubChem API
#'
#' @description Constructs a URL for the PubChem API based on the provided parameters.
#' This function used internally by all get_* functions, and users will not typically run this function directly.
#'
#' @param identifier The identifier for the compound.
#' @param namespace The namespace for the identifier (default: 'cid').
#' @param domain The domain for the request (default: 'compound').
#' @param operation The operation to be performed (default: NULL).
#' @param output The desired output format (default: 'JSON').
#' @param searchtype The type of search to be performed (default: NULL).
#' @param options Additional parameters. Currently has no effect on the results.
#'
#' @return A constructed URL for the PubChem API.
#'
#' @importFrom utils URLencode
#'
#' @keywords internal
#' @noRd
request <- function(identifier = NULL, namespace = 'cid', domain = 'compound',
                    operation = NULL, output = 'JSON', searchtype = NULL, options = NULL) {

  if (is.null(identifier)) {
    stop("identifier/cid cannot be NULL")
  }

  pc_build_url(
    domain = domain,
    namespace = namespace,
    identifier = identifier,
    operation = operation,
    output = output,
    searchtype = searchtype,
    options = options
  )
}
