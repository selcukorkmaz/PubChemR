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
get_substances <- function(identifier, namespace = "sid", operation = NULL, options = NULL) {

  out <- list(
    result = vector("list", length(identifier)),
    request_args = list(
      namespace = namespace,
      identifier = identifier,
      domain = "substance"
    ),
    success = rep(FALSE, length(identifier))
  )

  # Helper: detect *top-level* PUG REST Fault only (be precise)
  has_top_fault <- function(x) {
    is.list(x) && !is.null(x$Fault)
  }

  fault_message <- function(x) {
    if (!is.list(x$Fault)) return("PubChem fault.")
    parts <- c(x$Fault$Code, x$Fault$Message, paste(x$Fault$Details, collapse = "; "))
    parts <- parts[!vapply(parts, is.null, logical(1))]
    msg <- paste(parts, collapse = " - ")
    if (nzchar(msg)) msg else "PubChem fault."
  }

  # Consider any non-empty list without a top-level Fault as meaningful
  is_meaningful <- function(x) {
    is.list(x) && length(x) > 0 && is.null(x$Fault)
  }

  make_error_list <- function(msg, cls = "not_found") {
    list(error = list(message = msg, class = cls))
  }

  for (i in seq_along(identifier)) {
    x <- identifier[i]

    res <- tryCatch({
      tmp <- get_json(
        identifier = x,
        namespace   = namespace,
        domain      = "substance",
        operation   = operation,
        path        = NULL,
        options     = options
      )

      # Fault from PubChem
      if (has_top_fault(tmp)) {
        stop(fault_message(tmp), call. = FALSE)
      }

      # Empty/meaningless payload -> treat as no hit
      if (!is_meaningful(tmp)) {
        stop(sprintf("Empty result for identifier '%s' in namespace '%s'.", x, namespace), call. = FALSE)
      }

      # SUCCESS: store the payload directly (so instance() keeps working)
      class(tmp) <- unique(c("PC_Substance", class(tmp)))
      out$result[[i]] <- tmp
      out$success[i]  <- TRUE
      NULL
    },
    error = function(e) {
      # FAILURE: store a list with $error (tests expect this)
      out$result[[i]] <- make_error_list(conditionMessage(e), class(e)[1])
      out$success[i]  <- FALSE
      NULL
    })
  }

  # Make selection by name easier for instance() implementations
  names(out$result) <- identifier

  structure(out, class = c("PubChemInstanceList"))
}


