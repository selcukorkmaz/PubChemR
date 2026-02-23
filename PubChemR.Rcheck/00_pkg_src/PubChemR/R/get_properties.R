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

  # Check if properties is a single string, and split it into a vector if needed
  if (is.character(properties) && length(properties) == 1 && !grepl(",", properties)) {
    properties <- strsplit(properties, ",")[[1]]
  }

  if (is.null(properties) && (!("type" %in% names(propertyMatch)) || propertyMatch$type != "all")) {
    propertyMatch$type <- "all"
  }

  propertyMatch$x <- properties

  # Set .ignore.case to FALSE if the type is "match"
  if (propertyMatch$type == "match") {
    propertyMatch$.ignore.case <- FALSE
  }

  # Get property names from available properties
  propertyNames <- do.call("property_map", propertyMatch)

  if (is.null(propertyNames) || length(propertyNames) == 0) {
    stop("No valid properties matched the requested pattern. Use property_map(type = 'all') to inspect available properties.")
  }

  # Create the properties string for the URL
  properties_str <- paste(propertyNames, collapse = ',')
  properties_endpoint <- paste('property', properties_str, sep = '/')

  structure(
    pc_collect_instances(
      identifier = identifier,
      namespace = namespace,
      domain = "compound",
      operation = properties_endpoint,
      searchtype = searchtype,
      options = options,
      result_class = "PC_Properties",
      request_args_extra = list(
        properties = properties,
        propertyMatch = propertyMatch
      )
    ),
    class = c("PubChemInstanceList")
  )
}


