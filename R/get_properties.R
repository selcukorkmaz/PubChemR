#' @title Retrieve Compound Properties from PubChem
#'
#' @description This function sends a request to PubChem to retrieve compound properties based on the specified parameters.
#'
#' @param properties A character vector specifying the properties to retrieve.
#'                   If \code{NULL} (default), all available properties are retrieved.
#'                   Properties can be specified by exact names, partial matches, or patterns, controlled by the \code{propertyMatch} argument.
#'                   For a full list of properties, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Compound-Property-Tables}{Property Table}.
#' @param identifier A vector of compound identifiers, either numeric or character.
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
#'                  - Other namespaces as specified in the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Input}{API documentation}.
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
#'                   - Other search types as specified in the API documentation.
#'
#'                   If \code{NULL} (default), no search type is specified.
#'
#'                   For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Input}{API documentation}.
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
#' @param propertyMatch A list of arguments to control how properties are matched.
#'
#'                      The list can include:
#'
#'                      - \code{type}: The type of match. Possible values are \code{exact}, \code{contain}, \code{match}. Default is \code{contain}.
#'
#'                      - \code{.ignore.case}: Logical value indicating if the match should ignore case. Default is \code{FALSE}.
#'
#'                      - \code{x}: The properties to match (set internally; do not set manually).
#'
#'                      Default is \code{list(.ignore.case = FALSE, type = "contain")}.
#'
#' @details
#' For more detailed information, please refer to the
#' \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest}{PubChem PUG REST API documentation}.
#'
#' @return An object of class "PubChemInstanceList" containing all the properties of the requested compounds.
#'
#' @rdname get_properties
#' @order 1
#'
#' @examples
#' \donttest{
#' # Isomeric SMILES of the compounds
#' props <- get_properties(
#'   properties = c("MolecularWeight", "MolecularFormula", "InChI"),
#'   identifier = c("aspirin", "ibuprofen", "caffeine"),
#'   namespace = "name"
#' )
#'
#' # Properties for a selected compound
#' instance(props, "aspirin")
#' retrieve(props, .which = "aspirin", .slot = NULL)
#' retrieve(instance(props, "aspirin"), .slot = NULL)
#'
#' # Combine properties of all compounds into a single data frame (or list)
#' retrieve(props, .combine.all = TRUE)
#'
#' # Return selected properties
#' retrieve(props, .combine.all = TRUE,
#'   .slot = c("MolecularWeight", "MolecularFormula"))
#'
#' # Return properties for the compounds in a range of CIDs
#' props <- get_properties(
#'   properties = c("mass", "molecular"),
#'   identifier = 2244:2255,
#'   namespace = "cid",
#'   propertyMatch = list(
#'     type = "contain"
#'   )
#' )
#'
#' retrieve(props, .combine.all = TRUE, .to.data.frame = TRUE)
#'
#' # Return all available properties of the requested compounds
#' props <- get_properties(
#'   properties = NULL,
#'   identifier = 2244:2245,
#'   namespace = "cid",
#'   propertyMatch = list(
#'     type = "all"
#'   )
#' )
#'
#' retrieve(props, .combine.all = TRUE)
#'
#'}
#'
#' @export
get_properties <- function(properties = NULL, identifier, namespace = 'cid', searchtype = NULL, options = NULL,
                           propertyMatch = list(.ignore.case = FALSE, type = "contain")) {
  # If properties is a single string, split it into a vector
  # if (is.character(properties) && !grepl(",", properties)) {
  #   properties <- strsplit(properties, ",")[[1]]
  # }

  propertyMatch$x <- properties

  # Ignore case is FALSE if "match" pattern is set.
  if (propertyMatch$type == "match"){
    propertyMatch$.ignore.case = FALSE
  }

  # Get property names from available properties.
  propertyNames <- do.call("property_map", propertyMatch)

  # Create the properties string for the URL
  properties_str <- paste(propertyNames, collapse = ',')
  properties_endpoint <- paste('property', properties_str, sep = '/')

  # Try to get the response and parse JSON
  # Assuming 'get_json' is a function you've previously defined, similar to your Python environment
  result <- lapply(identifier, function(x){
    tmp <- get_json(identifier = x, namespace = namespace, domain = 'compound',
                    operation = properties_endpoint,
                    searchtype = searchtype, options)
    class(tmp) <- c(class(tmp), "PC_Properties")
    return(tmp)
  })

  Properties_List <- list(
    result = result,
    request_args = list(
      properties = properties,
      namespace = namespace,
      identifier = identifier,
      domain = "compound",
      operation = properties_endpoint,
      options = options,
      searchtype = searchtype,
      propertyMatch = propertyMatch
    ),
    success = logical(),
    error = NULL
  )

  structure(
    Properties_List,
    class = c("PubChemInstanceList")
  )
}


