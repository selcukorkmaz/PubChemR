#' @title Retrieve Substances from PubChem
#'
#' @description This function sends a request to PubChem to retrieve substance data based on the specified parameters.
#'
#' @param identifier A vector of substance identifiers, either numeric or character.
#' @param namespace A character string specifying the namespace of the identifier.
#' @param operation A character string specifying the operation to perform.
#' @param options A list of additional options for the request.
#'
#' @details
#' For more detailed information, please refer to the
#' \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest}{PubChem PUG REST API documentation}.
#'
#' @return An object of class 'PubChemInstanceList' containing all the substance information of requested compounds.
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
get_substances <- function(identifier, namespace = "sid", operation = NULL, options = NULL) {
  if (is.null(operation)) {
    operation <- "record"
  }

  structure(
    pc_collect_instances(
      identifier = identifier,
      namespace = namespace,
      domain = "substance",
      operation = operation,
      options = options,
      result_class = "PC_Substance"
    ),
    class = c("PubChemInstanceList")
  )
}
