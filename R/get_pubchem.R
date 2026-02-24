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
#' @importFrom httr GET POST http_status content
#' @importFrom RJSONIO fromJSON
#'
#' @examples
#' \donttest{
#' get_pubchem(
#'   identifier = "aspirin",
#'   namespace = "name"
#' )
#' }
get_pubchem <- function(identifier, namespace = 'cid', domain = 'compound', operation = NULL,
                        output = 'JSON', searchtype = NULL, options = NULL) {
  response <- NULL
  status <- NULL
  content <- NULL
  binary_outputs <- c("PNG")

  response_to_content <- function(resp, out_format) {
    fmt <- if (is.null(out_format)) "JSON" else toupper(as.character(out_format)[1])
    payload <- resp$content
    if (fmt %in% binary_outputs) {
      return(payload)
    }
    rawToChar(payload)
  }

  safe_request <- function(url, post_body = NULL) {
    resp <- if (!is.null(post_body)) {
      POST(url, body = post_body, encode = "form")
    } else {
      GET(url)
    }
    status_obj <- httr::http_status(resp)
    if (!identical(status_obj$category, "Success")) {
      raw_text <- httr::content(resp, "text", encoding = "UTF-8")
      fault_obj <- tryCatch(fromJSON(raw_text), error = function(e) NULL)
      if (!is.null(fault_obj$Fault)) {
        stop(pc_format_fault(fault_obj$Fault), call. = FALSE)
      }
      stop(paste0("HTTP ", httr::status_code(resp), " request failed."), call. = FALSE)
    }
    resp
  }

  # Build URL and optional POST body.
  # Namespaces like smiles/inchi/sdf/smarts use POST to avoid URL-encoding
  # issues with / and \ characters in the identifier.
  build_req <- function(id, ns, dom, op, out, st, opts) {
    if (pc_use_post(ns)) {
      url <- pc_build_url(domain = dom, namespace = ns, identifier = NULL,
                          operation = op, output = out, searchtype = st,
                          options = opts)
      body <- stats::setNames(list(as.character(id)), tolower(ns))
      list(url = url, body = body)
    } else {
      list(url = request(id, ns, dom, op, out, st, opts), body = NULL)
    }
  }

  # If the searchtype is not 'xref' or if the namespace is 'formula', handle it differently
  if ((!is.null(searchtype) && searchtype != 'xref') || (!is.null(namespace) && namespace == 'formula')) {
    req <- build_req(identifier, namespace, domain, NULL, 'JSON', searchtype, options)
    response <- safe_request(req$url, req$body)

    content <- response_to_content(response, "JSON")
    status <- fromJSON(content)

    # Check if the response is asking to wait and has a ListKey
    if ('Waiting' %in% names(status) && !is.null(status$Waiting[["ListKey"]])) {
      identifier <- status$Waiting[["ListKey"]]
      namespace <- 'listkey'

      iter <- 1
      max_iter <- 10
      while ('Waiting' %in% names(status) && !is.null(status$Waiting[["ListKey"]])) {
        # Delay before making the next request
        Sys.sleep(1.5)  # delay for 1.5 seconds not to blocked by PubChem API.
        # Make the next request
        req <- build_req(identifier, namespace, domain, operation, 'JSON', NULL, options)
        response <- safe_request(req$url, req$body)
        content <- response_to_content(response, "JSON")
        status <- fromJSON(content)

        iter <- iter + 1
        if (iter >= max_iter){
          break
        }
      }

      if ('Waiting' %in% names(status) && !is.null(status$Waiting[["ListKey"]])) {
        stop(
          paste0(
            "PubChem request is still processing after ",
            max_iter,
            " polling attempts. Try again with the returned ListKey."
          ),
          call. = FALSE
        )
      }
    }

    # If the final output is not JSON, we make another request for the correct output format
    if (output != 'JSON') {
      req <- build_req(identifier, namespace, domain, operation, output, searchtype, options)
      response <- safe_request(req$url, req$body)
      content <- response_to_content(response, output)
    }
  } else {
    # If it doesn't meet the conditions above, make a standard request
    req <- build_req(identifier, namespace, domain, operation, output, searchtype, options)
    response <- safe_request(req$url, req$body)
    content <- response_to_content(response, output)
  }

  return(content)
}



