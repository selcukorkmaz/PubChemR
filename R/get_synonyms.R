#' @title Retrieve Synonyms from PubChem
#'
#' @description This function sends a request to PubChem to retrieve synonyms for a given identifier.
#' It returns a list of synonyms corresponding to the provided identifier.
#'
#' @param identifier A character or numeric value specifying the identifier for the request.
#' @param namespace A character string specifying the namespace for the request. Default is 'cid'.
#' @param domain A character string specifying the domain for the request. Default is 'compound'.
#' @param searchtype A character string specifying the search type. Default is NULL.
#' @param options Additional arguments passed to PubChem request.
#'
#' @return An object of class 'PubChemInstance_Synonyms', which is a list containing information retrieved from the PubChem database. Synonyms data can be extracted from the returned object using the \link{synonyms} function.
#'
#' @examples
#' syns <- get_synonyms(
#'   identifier = "aspirin",
#'   namespace = "name"
#' )
#'
#' synonyms(syns)
#'
#' @export
get_synonyms <- function(identifier, namespace = 'cid', domain = 'compound', searchtype = NULL, options = NULL) {

  result <- lapply(identifier, function(x){
    tmp <- get_json(identifier = x, namespace, domain, 'synonyms', searchtype, options)
    class(tmp) <- NULL
    return(tmp)
  })

  Synonyms_List <- list(
    result = result,
    request_args = list(
      namespace = namespace,
      identifier = identifier,
      domain = domain,
      operation = "synonyms"
    ),
    success = logical(),
    error = NULL
  )

  structure(
    Synonyms_List,
    class = c("PubChemInstance_Synonyms")
  )
}

