#' @title Retrieve Compound Properties from PubChem
#'
#' @description This function sends a request to PubChem to retrieve compound properties based on the specified parameters.
#'
#' @param properties A character vector specifying the properties to be retrieved. It is ignored if all available properties are requested from PubChem. See examples.
#' @param identifier A vector of positive integers (e.g., CID, SID, AID) or identifier strings (e.g., source, InChIKey, formula). In some cases, only a single identifier string is allowed (e.g., name, SMILES, xref; InChI, SDF by POST only).
#' @param namespace Specifies the namespace for the query. For the 'compound' domain, possible values include 'cid', 'name', 'smiles', 'inchi', 'sdf', 'inchikey', 'formula', 'substructure', 'superstructure', 'similarity', 'identity', 'xref', 'listkey', 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure', and 'fastformula'. For other domains, the possible namespaces are domain-specific.
#' @param searchtype Specifies the type of search to be performed. For structure searches, possible values are combinations of 'substructure', 'superstructure', 'similarity', 'identity' with 'smiles', 'inchi', 'sdf', 'cid'. For fast searches, possible values are combinations of 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure' with 'smiles', 'smarts', 'inchi', 'sdf', 'cid', or 'fastformula'.
#' @param options Additional arguments passed to \code{get_json}.
#' @param propertyMatch A list containing the arguments passed to the \link{property_map} function. See examples of the \code{property_map()} function.
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


