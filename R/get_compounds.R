#' @title Retrieve Compounds from PubChem
#'
#' @description This function sends a request to the PubChem database to retrieve compound data based on specified parameters.
#'
#' @param identifier A vector of positive integers (e.g., cid, sid, aid) or identifier strings (source, inchikey, formula).
#'                   In some cases, a single identifier string (e.g., name, smiles, xref; inchi, sdf by POST only) is sufficient.
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
#'                  For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Input}{Input} section of the PUG REST API.
#'
#' @param operation A character string specifying the operation to perform.
#'
#'                  Possible values include:
#'
#'                  - \code{synonyms}: Retrieve synonyms for the compounds.
#'
#'                  - \code{description}: Retrieve compound descriptions.
#'
#'                  - \code{sids}: Retrieve Substance IDs related to the compounds.
#'
#'                  - \code{aids}: Retrieve Assay IDs related to the compounds.
#'
#'                  - \code{classification}: Retrieve compound classification.
#'
#'                  - \code{cids}: Retrieve Compound IDs (used when the input is not CID).
#'
#'                  If \code{NULL} (default), the basic compound record is retrieved.
#'
#'                  For a full list of operations, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Operation}{Operations} section of the PUG REST API.
#'
#' @param searchtype An optional character string specifying the search type.
#'
#'                   Possible values include:
#'
#'                   - \code{similarity}
#'
#'                   - \code{substructure}
#'
#'                   - \code{superstructure}
#'
#'                   - \code{identity}
#'
#'                   If \code{NULL} (default), no search type is specified.
#'
#'                  For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Input}{Input} section of the PUG REST API.
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
#' @return An object of class 'PubChemInstanceList' and 'PC_Compounds' containing compound information from the PubChem database.
#'
#' @seealso \link{retrieve}, \link{instance}
#'
#' @examples
#' compound <- get_compounds(
#'   identifier = c("aspirin", "ibuprofen", "rstudio"),
#'   namespace = "name"
#' )
#'
#' print(compound)
#'
#' # Return results for selected compound.
#' instance(compound, "aspirin")
#' instance(compound, "rstudio")
#' # instance(compound, "unknown"). # returns error.
#'
#' # Extract compound properties for the compound "aspirin".
#' # Use the 'retrieve()' function to extract specific slots from the compound list.
#' retrieve(instance(compound, "aspirin"), "props")
#'
#' @export
get_compounds <- function(identifier, namespace = 'cid', operation = NULL, searchtype = NULL, options = NULL) {
  # Try to get the response and parse JSON
  # Assuming 'get_json' is a function you've previously defined, similar to your Python environment
  result <- lapply(identifier, function(x){
    tmp <- get_json(identifier = x, namespace, operation = operation, searchtype = searchtype, options = options)
    class(tmp) <- c(class(tmp), "PC_Compounds")
    return(tmp)
  })

  Compounds_List <- list(
    result = result,
    request_args = list(
      namespace = namespace,
      identifier = identifier,
      domain = "compound",
      operation = "description",
      options = options,
      searchtype = searchtype
    ),
    success = logical(),
    error = NULL
  )

  structure(
    Compounds_List,
    class = c("PubChemInstanceList")
  )
}
