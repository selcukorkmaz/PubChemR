#' @title Retrieve Assays from PubChem
#'
#' @description This function sends a request to PubChem to retrieve assay data based on the specified parameters.
#'
#' @param identifier A vector of positive integers (e.g., \code{cid}, \code{sid}, \code{aid}) or identifier strings (source, \code{inchikey}, \code{formula}).
#'                   In some cases, only a single identifier string (e.g., \code{name}, \code{smiles}, \code{xref}; \code{inchi}, \code{sdf} by POST only) can be provided.
#'                   Multiple elements can be included as a vector. See Notes for details.
#' @param namespace A character string specifying the namespace of the identifier.
#'
#'                  Possible values include:
#'
#'                  - \code{aid}: PubChem Assay Identifier (default)
#'
#'                  - \code{listkey}: A list key obtained from a previous query
#'
#'                  - \code{type/<assay type>}: Specify the assay type
#'
#'                  - \code{sourceall/<source name>}: Specify the source name
#'
#'                  - \code{target/<assay target>}: Specify the assay target
#'
#'                  - \code{activity/<activity column name>}: Specify the activity column name
#'
#'                  For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Input}{Input} section of the PUG REST API.
#'
#' @param operation A character string specifying the operation to perform.
#'
#'                  Possible values include:
#'
#'                  - \code{record}: Retrieve the full assay record
#'
#'                  - \code{concise}: Retrieve a concise summary of the assay
#'
#'                  - \code{aids}: Retrieve related Assay IDs
#'
#'                  - \code{sids}: Retrieve related Substance IDs
#'
#'                  - \code{cids}: Retrieve related Compound IDs
#'
#'                  - \code{description}: Retrieve assay descriptions (default)
#'
#'                  - \code{targets/<target type>}: Retrieve targets of the assay
#'
#'                  - \code{summary}: Retrieve a summary of the assay
#'
#'                  - \code{classification}: Retrieve assay classification
#'
#'                  If \code{NULL} (default), the operation defaults to \code{description}.
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
#' @return An object of class 'PubChemInstanceList' containing the information retrieved from the PubChem database.
#'
#' @note To extract information about a specific assay from the returned list, use the \link{instance} function.
#'
#' Each assay may include information on several properties. Specific information from the assay can be extracted using the \link{retrieve} function. See examples.
#'
#' @seealso \link{retrieve}, \link{instance}
#'
#' @examples
#' # Retrieve a list of assays from the PubChem database
#' assays <- get_assays(
#'   identifier = c(1234, 7815),
#'   namespace = 'aid'
#' )
#'
#' # Return assay information for assay ID '1234'
#' assay1234 <- instance(assays, "1234")
#' print(assay1234)
#'
#' # Retrieve specific elements from the assay output
#' retrieve(assay1234, "aid")
#'
#' @export
get_assays <- function(identifier, namespace = 'aid', operation = NULL, options = NULL) {
  # Try to get the response and parse JSON
  # Assuming 'get_json' is a function you've previously defined, similar to your Python environment
  result <- lapply(identifier, function(x){
    tmp <- get_json(identifier = x, namespace, 'assay', 'description', NULL, options)
    class(tmp) <- c(class(tmp), "PC_Assay")
    return(tmp)
  })

  Assays_List <- list(
    result = result,
    request_args = list(
      namespace = namespace,
      identifier = identifier,
      domain = "assay",
      operation = "description"
    ),
    success = logical(),
    error = NULL
  )

  structure(
    Assays_List,
    class = c("PubChemInstanceList")
  )
}
