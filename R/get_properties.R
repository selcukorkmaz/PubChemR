#' Retrieve Compound Properties from PubChem
#'
#' This function sends a request to PubChem to retrieve compound properties based on the specified parameters.
#' It returns a list or dataframe of properties corresponding to the provided identifiers.
#'
#' @param properties A character vector specifying the properties to be retrieved.
#' @param identifier A vector of positive integers (e.g. cid, sid, aid) or identifier strings (source, inchikey, formula). In some cases, only a single identifier string (name, smiles, xref; inchi, sdf by POST only).
#' @param namespace Specifies the namespace for the query. For the 'compound' domain, possible values include 'cid', 'name', 'smiles', 'inchi', 'sdf', 'inchikey', 'formula', 'substructure', 'superstructure', 'similarity', 'identity', 'xref', 'listkey', 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure', and 'fastformula'. For other domains, the possible namespaces are domain-specific.
#' @param searchtype Specifies the type of search to be performed. For structure searches, possible values are combinations of 'substructure', 'superstructure', 'similarity', 'identity' with 'smiles', 'inchi', 'sdf', 'cid'. For fast searches, possible values are combinations of 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure' with 'smiles', 'smarts', 'inchi', 'sdf', 'cid', or 'fastformula'.
#' @param options Additional arguments passed to \code{\link{get_json}}.
#'
#' @return If `as_dataframe` is FALSE, a named list where each element corresponds to the properties retrieved from PubChem.
#'         If `as_dataframe` is TRUE, a dataframe where each row corresponds to the properties retrieved from PubChem.
#'         The names of the list elements or row names of the dataframe are based on the provided identifiers.
#'
#' @importFrom RJSONIO fromJSON
#' @importFrom dplyr as_tibble contains
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' get_properties(
#'   properties = "IsomericSMILES",
#'   identifier = "aspirin",
#'   namespace = "name"
#' )
get_properties <- function(properties, identifier, namespace = 'cid', searchtype = NULL, options = NULL,
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


