#' Retrieve Substances from PubChem
#'
#' This function sends a request to PubChem to retrieve substance data based on the specified parameters.
#' It returns a list of substances corresponding to the provided identifiers.
#'
#' @param identifier A character or numeric vector specifying the identifiers for the request.
#' @param namespace A character string specifying the namespace for the request. Default is 'sid'.
#' @param ... Additional parameters to be passed to the request.
#'
#' @return A named list where each element corresponds to a substance retrieved from PubChem.
#'         The names of the list elements are based on the provided identifiers.
#'         If no substance is found for a given identifier, the corresponding list element will contain the string "No substance".
#'
#' @examples
#' \dontrun{
#'   res <- get_substances(identifier = c("4594"), namespace = "sid")
#'   print(res$Substance_4594)
#' }
#'
#' @importFrom RJSONIO fromJSON
#'
#' @export
get_substances <- function(identifier, namespace='sid', ...) {

  substances <- list()
  
  for (i in 1:length(identifier)) {
    # Retrieve the JSON data
    results <- get_json(identifier[i], namespace, 'substance', operation = NULL, searchtype = NULL)
    
    # Check if results are not empty
    if (!is.null(results)) {
      # Create a list of substances (here, you might want to define what an 'substance' contains)
      
      if (!is.null(results$PC_Substances)) {
        substances[[i]] <- results$PC_Substances
      } else{
        substances[[i]] <- "No substance"
        
      }
      
    }}
  
  names(substances) = paste0("Substance_", identifier)
  
  results = substances
  
  return(results)
}

res = get_substances(c("4594"), "sid")
res$Substance_4594



