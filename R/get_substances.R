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
  result <- lapply(identifier, function(x){
    tmp <- get_json(identifier = x, namespace, "substance", operation, NULL, options)
    class(tmp) <- c("PC_Substance", class(tmp))
    return(tmp)
  })

  Substances_List <- list(
    result = result,
    request_args = list(
      namespace = namespace,
      identifier = identifier,
      domain = "substance"
    ),
    success = logical(),
    error = NULL
  )

  structure(
    Substances_List,
    class = c("PubChemInstanceList")
  )
}


