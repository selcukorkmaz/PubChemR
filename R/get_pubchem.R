#' Get Data from PubChem API
#'
#' This function constructs a URL to query the PubChem API based on the provided parameters and returns the response content.
#'
#' @param identifier A vector of identifiers, either numeric or character.
#'                   The type of identifier depends on the \code{namespace} parameter.
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
#'                  - Other namespaces as specified in the API documentation.
#'
#'                  For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Input}{Input} section of the PUG REST API.
#'
#' @param domain A character string specifying the domain of the query.
#'
#'               Possible values include:
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
#'                  Examples include:
#'
#'                  - \code{record}: Retrieve the full record.
#'
#'                  - \code{property}: Retrieve specified properties.
#'
#'                  - \code{synonyms}: Retrieve synonyms.
#'
#'                  - \code{sids}: Retrieve Substance IDs.
#'
#'                  - \code{cids}: Retrieve Compound IDs.
#'
#'                  - \code{aids}: Retrieve Assay IDs.
#'
#'                  If \code{NULL} (default), the basic record is retrieved.
#'
#'                  For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Operation}{Operations} section of the PUG REST API.
#'
#' @param output A character string specifying the output format.
#'
#'               Possible values include:
#'
#'               - \code{JSON} (default)
#'
#'               - \code{XML}
#'
#'               - \code{CSV}
#'
#'               - \code{SDF}
#'
#'               - \code{TXT}
#'
#'               - \code{PNG}
#'
#'               For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Output}{Output} section of the PUG REST API.
#'
#' @param searchtype An optional character string specifying the search type.
#'
#'                   Possible values include:
#'
#'                   - \code{substructure}
#'
#'                   - \code{superstructure}
#'
#'                   - \code{similarity}
#'
#'                   - \code{identity}
#'
#'                   - Other search types as specified in the API documentation.
#'
#'                   If \code{NULL} (default), no search type is specified.
#'
#'                   For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Input}{Input} section of the PUG REST API.
#'
#' @param options A list of additional options for the request.
#'                Available options depend on the specific request and the API.
#'                If \code{NULL} (default), no additional options are included.
#'                For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Structure-Search-Operations}{Structure Search Operations} section of the PUG REST API.
#'
#' @details
#' The PubChem PUG REST API allows users to retrieve data about compounds, substances, assays, and more.
#' This function constructs the appropriate API call based on the provided parameters.
#' For more detailed information, please refer to the
#' \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest}{PubChem PUG REST API documentation}.

#'
#' @return Returns the response content from the PubChem API based on the constructed URL.
#'
#' @noRd
#'
#' @importFrom httr GET http_status content
#' @importFrom RJSONIO fromJSON
#'
#' @examples
#' get_pubchem(
#'   identifier = "aspirin",
#'   namespace = "name"
#' )
get_pubchem <- function(identifier, namespace = 'cid', domain = 'compound', operation = NULL,
                        output = 'JSON', searchtype = NULL, options = NULL) {
  response <- NULL
  status <- NULL

  # If the searchtype is not 'xref' or if the namespace is 'formula', handle it differently
  if ((!is.null(searchtype) && searchtype != 'xref') || (!is.null(namespace) && namespace == 'formula')) {
    response <- GET(request(identifier, namespace, domain, NULL, 'JSON', searchtype, options))

    content <- rawToChar(response$content)
    status <- fromJSON(content)

    # Check if the response is asking to wait and has a ListKey
    if ('Waiting' %in% names(status) && !is.null(status$Waiting[["ListKey"]])) {
      identifier <- status$Waiting[["ListKey"]]
      namespace <- 'listkey'

      iter <- 1
      while ('Waiting' %in% names(status) && !is.null(status$Waiting[["ListKey"]])) {
        # Delay before making the next request
        Sys.sleep(1.5)  # delay for 1.5 seconds not to blocked by PubChem API.
        # Make the next request
        response <- GET(request(identifier, namespace, domain, operation, 'JSON', options))
        content <- rawToChar(response$content)
        status <- fromJSON(content)

        iter <- iter + 1
        if (iter == 3){
          break
        }
      }
    }

    # If the final output is not JSON, we make another request for the correct output format
    if (output != 'JSON') {
      response <- GET(request(identifier, namespace, domain, operation, output, searchtype, options))
      content <- rawToChar(response$content)  # Assuming 'content' is the field with data
    }
  } else {
    # If it doesn't meet the conditions above, make a standard request
    response <- GET(request(identifier, namespace, domain, operation, output, searchtype, options))

    # Check if the request was successful
    if (http_status(response)$category != "Success") {
      stop(fromJSON(content(response, "text", encoding = "UTF-8")))
    } else {
      content <- rawToChar(response$content)
    }
  }

  return(content)
}




