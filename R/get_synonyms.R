#' @title Retrieve Synonyms from PubChem
#'
#' @description This function sends a request to PubChem to retrieve synonyms for a given identifier.
#' It returns a list of synonyms corresponding to the provided identifier.
#'
#' @param identifier A vector of identifiers, either numeric or character.
#'                   The type of identifier depends on the \code{namespace} and \code{domain} parameters.
#'                   **Note**: \code{identifier} must be provided; it cannot be \code{NULL}.
#' @param namespace A character string specifying the namespace of the identifier.
#'
#'                  Possible values depend on the \code{domain} parameter and include:
#'
#'                  - For \code{domain = 'compound'}: \code{cid}, \code{name}, \code{smiles}, \code{inchi}, \code{sdf}, \code{inchikey}, \code{formula}, etc.
#'
#'                  - For \code{domain = 'substance'}: \code{sid}, \code{sourceid/<source id>}, \code{sourceall/<source name>}, \code{name}, etc.
#'
#'                  - For \code{domain = 'assay'}: \code{aid}, \code{listkey}, \code{type/<assay type>}, \code{sourceall/<source name>}, etc.
#'
#'                   For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Input}{Input} section of the PUG REST API.
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
#' @param searchtype An optional character string specifying the search type.
#'
#'                   Possible values depend on the \code{namespace} and \code{domain}.
#'
#'                   Examples include:
#'
#'                   - \code{substructure}, \code{superstructure}, \code{similarity}, \code{identity} for structure searches.
#'
#'                   - \code{fastidentity}, \code{fastsimilarity_2d}, \code{fastsimilarity_3d}, etc. for fast searches.
#'
#'                   If \code{NULL} (default), no search type is specified.
#'
#'                   For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Input}{Input} section of the PUG REST API.
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
#' The PubChem PUG REST API allows for retrieving synonyms related to various domains.
#' The table below summarizes valid combinations for retrieving synonyms:
#' For more detailed information, please refer to the
#' \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest}{PubChem PUG REST API documentation}.

#'
#' @return An object of class 'PubChemInstance_Synonyms', which is a list containing information retrieved from the PubChem database. Synonyms data can be extracted from the returned object using the \link{synonyms} function.
#'
#' @examples
#' syns <- get_synonyms(
#'   identifier = "aspirin",
#'   namespace = "name"
#' )
#'
#' syns
#'
#' synonyms(syns)
#'
#' @export
get_synonyms <- function(identifier, namespace = 'cid', domain = 'compound', searchtype = NULL, options = NULL) {

  # Validate that identifier is not NULL
  if (is.null(identifier) || length(identifier) == 0) {
    stop("Error: 'identifier' cannot be NULL or empty. Please provide at least one valid identifier.")
  }

  # Initialize result list
  Synonyms_List <- list(
    result = list(),
    request_args = list(
      namespace = namespace,
      identifier = identifier,
      domain = domain,
      operation = "synonyms"
    ),
    success = logical(length(identifier)),  # Logical vector to track success/failure for each identifier
    error = NULL
  )

  # Perform the request for each identifier and handle potential errors
  Synonyms_List$result <- lapply(seq_along(identifier), function(i) {
    id <- identifier[i]
    result <- tryCatch({
      # Get JSON content using the get_json function (assuming get_json is defined elsewhere)
      tmp <- get_json(identifier = id, namespace, domain, 'synonyms', searchtype, options)
      class(tmp) <- NULL  # Remove class attribute for standardization
      Synonyms_List$success[i] <- TRUE  # Mark success for this identifier
      return(tmp)
    }, error = function(e) {
      # If an error occurs, mark failure and store error message
      Synonyms_List$success[i] <- FALSE
      message(paste("Failed to retrieve synonyms for identifier '", id, "': ", conditionMessage(e), sep = ""))
      return(NULL)  # Return NULL for failed identifiers
    })
    return(result)
  })

  # Check if any errors occurred
  if (!all(Synonyms_List$success)) {
    Synonyms_List$error <- paste("Failed to retrieve synonyms for the following identifiers: ",
                                 paste(identifier[!Synonyms_List$success], collapse = ", "), sep = "")
  }

  # Return the structured synonyms list
  structure(Synonyms_List, class = c("PubChemInstance_Synonyms"))
}


