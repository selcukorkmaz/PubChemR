#' Get Data from PubChem API
#'
#' This function constructs a URL to query the PubChem API based on the provided parameters and returns the response content.
#'
#' @param identifier A vector of positive integers (e.g. cid, sid, aid) or identifier strings (source, inchikey, formula). In some cases, only a single identifier string (name, smiles, xref; inchi, sdf by POST only).
#' @param namespace Specifies the namespace for the query. For the 'compound' domain, possible values include 'cid', 'name', 'smiles', 'inchi', 'sdf', 'inchikey', 'formula', 'substructure', 'superstructure', 'similarity', 'identity', 'xref', 'listkey', 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure', and 'fastformula'. For other domains, the possible namespaces are domain-specific.
#' @param domain Specifies the domain of the query. Possible values are 'substance', 'compound', 'assay', 'gene', 'protein', 'pathway', 'taxonomy', 'cell', 'sources', 'sourcetable', 'conformers', 'annotations', 'classification', and 'standardize'.
#' @param operation Specifies the operation to be performed on the input records. For the 'compound' domain, possible operations include 'record', 'property', 'synonyms', 'sids', 'cids', 'aids', 'assaysummary', 'classification', 'xrefs', and 'description'. The available operations are domain-specific.
#' @param output Specifies the desired output format. Possible values are 'XML', 'ASNT', 'ASNB', 'JSON', 'JSONP', 'SDF', 'CSV', 'PNG', and 'TXT'.
#' @param searchtype Specifies the type of search to be performed. For structure searches, possible values are combinations of 'substructure', 'superstructure', 'similarity', 'identity' with 'smiles', 'inchi', 'sdf', 'cid'. For fast searches, possible values are combinations of 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure' with 'smiles', 'smarts', 'inchi', 'sdf', 'cid', or 'fastformula'.
#' @param options Additional arguments passed to \code{\link{request}}.
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




