#' Retrieve Compound Properties from PubChem
#'
#' This function sends a request to PubChem to retrieve compound properties based on the specified parameters.
#' It returns a list or dataframe of properties corresponding to the provided identifiers.
#'
#' @param properties A character vector specifying the properties to be retrieved.
#' @param identifier A vector of positive integers (e.g. cid, sid, aid) or identifier strings (source, inchikey, formula). In some cases, only a single identifier string (name, smiles, xref; inchi, sdf by POST only).
#' @param namespace Specifies the namespace for the query. For the 'compound' domain, possible values include 'cid', 'name', 'smiles', 'inchi', 'sdf', 'inchikey', 'formula', 'substructure', 'superstructure', 'similarity', 'identity', 'xref', 'listkey', 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure', and 'fastformula'. For other domains, the possible namespaces are domain-specific.
#' @param searchtype Specifies the type of search to be performed. For structure searches, possible values are combinations of 'substructure', 'superstructure', 'similarity', 'identity' with 'smiles', 'inchi', 'sdf', 'cid'. For fast searches, possible values are combinations of 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure' with 'smiles', 'smarts', 'inchi', 'sdf', 'cid', or 'fastformula'.
#' @param as_dataframe If TRUE it return a dataframe.
#' @param options Additional arguments passed to \code{\link{get_json}}.
#'
#' @return If `as_dataframe` is FALSE, a named list where each element corresponds to the properties retrieved from PubChem.
#'         If `as_dataframe` is TRUE, a dataframe where each row corresponds to the properties retrieved from PubChem.
#'         The names of the list elements or row names of the dataframe are based on the provided identifiers.
#'
#' @importFrom RJSONIO fromJSON
#' @importFrom dplyr as_tibble
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' get_properties(
#'   properties = "IsomericSMILES",
#'   identifier = "aspirin",
#'   namespace = "name"
#' )
get_properties <- function(properties, identifier, namespace = 'cid', searchtype = NULL, as_dataframe = FALSE, options = NULL) {
  # If properties is a single string, split it into a vector
  # if (is.character(properties) && !grepl(",", properties)) {
  #   properties <- strsplit(properties, ",")[[1]]
  # }

  if (any(!(properties %in% unlist(property_map))))
    stop(paste(properties[!(properties %in% unlist(property_map))], collapse = ", "), " is not in the property map!")

  # Map properties using the property_map, assuming it's available in your environment
  properties <- sapply(properties, function(p) {
    ifelse(!is.null(property_map[[p]]), property_map[[p]], p)
  })

  # Create the properties string for the URL
  properties_str <- paste(properties, collapse = ',')
  properties_endpoint <- paste('property', properties_str, sep = '/')

  # Here you would call the function that makes the HTTP request, e.g., get_json()
  # This is a placeholder for that function call, as its implementation depends on your setup.
  results <- get_json(identifier = identifier, namespace = namespace, domain = 'compound',
                      operation = properties_endpoint,
                      searchtype = searchtype, options)

  # Check if results are not empty and process them
  properties_results <- list()
  if (!is.null(results) && !is.null(results$PropertyTable) && !is.null(results$PropertyTable$Properties)) {
    properties_results <- results$PropertyTable$Properties
  }

  # If as_dataframe is TRUE, convert properties_results to a data frame
  if (as_dataframe) {
    # Assuming each item in properties_results is a list of properties, we bind them into a data frame
    # The actual structure depends on the results' format

    tryCatch({
      # Make the request. The 'get' function is expected to return the response content directly.
      # Check if the response is not empty or NULL before proceeding
      if (!is.null(properties_results)) {
        # Write the content to a file in SDF format in the current working directory
        properties_results <- do.call(rbind.data.frame, properties_results) %>% as_tibble()
      } else {
        return(NULL)
      }
    }, error = function(e) {
      # Here, you could check for specific types of errors (like NotFoundError)
      # and handle them as needed. For simplicity, we're just printing the error message.
      properties_results  # Return NULL to indicate no result or failure in the process
      message(paste("Error:", e$message))

    })
    # row.names(properties_df) <- sapply(properties_results, function(x) x$CID)  # if 'CID' is the identifier
  }

  return(properties_results)
}


