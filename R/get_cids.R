# Global Variables and/or Functions
utils::globalVariables(c("data", "CID"))


#' Retrieve Compound IDs (CIDs) from PubChem
#'
#' This function sends a request to PubChem to retrieve Compound IDs (CIDs) for a given identifier.
#' It returns a tibble (data frame) with the provided identifier and the corresponding CIDs.
#'
#' @param identifier A vector of positive integers (e.g. cid, sid, aid) or identifier strings (source, inchikey, formula). In some cases, only a single identifier string (name, smiles, xref; inchi, sdf by POST only).
#' @param namespace Specifies the namespace for the query. For the 'compound' domain, possible values include 'cid', 'name', 'smiles', 'inchi', 'sdf', 'inchikey', 'formula', 'substructure', 'superstructure', 'similarity', 'identity', 'xref', 'listkey', 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure', and 'fastformula'. For other domains, the possible namespaces are domain-specific.
#' @param domain Specifies the domain of the query. Possible values are 'substance', 'compound', 'assay', 'gene', 'protein', 'pathway', 'taxonomy', 'cell', 'sources', 'sourcetable', 'conformers', 'annotations', 'classification', and 'standardize'.
#' @param searchtype Specifies the type of search to be performed. For structure searches, possible values are combinations of 'substructure', 'superstructure', 'similarity', 'identity' with 'smiles', 'inchi', 'sdf', 'cid'. For fast searches, possible values are combinations of 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure' with 'smiles', 'smarts', 'inchi', 'sdf', 'cid', or 'fastformula'.
#' @param ... Additional arguments passed to \code{\link{get_json}}.
#'
#' @return A tibble (data frame) where each row corresponds to a provided identifier and its CID.
#'         The tibble has columns 'Compound' and 'CID'.
#'
#' @importFrom tibble tibble
#' @importFrom dplyr mutate row_number select
#' @importFrom tidyr unnest_wider unnest_longer
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' get_cids(
#'   identifier = "aspirin",
#'   namespace = "name"
#' )
get_cids <- function(identifier, namespace = 'name', domain = 'compound', searchtype = NULL, ...) {

  # Try to get the response and parse JSON
  result <- tryCatch({
    # Assuming 'get_json' is a function you've previously defined, similar to your Python environment

    cidsList <- list()

    for (i in 1:length(identifier)){
    response_json <- get_json(identifier[i], namespace, domain, 'cids', searchtype, ...)

    # Check if the response contains the expected information
    if (is.null(response_json)) {
      cidsList[[i]] <- list(Compound = identifier[i], CID = "No CID")

    } else if (!is.null(response_json$IdentifierList) && !is.null(response_json$IdentifierList$CID)) {
      cidsList[[i]] <- list(Compound = identifier[i], CID = response_json$IdentifierList$CID)

    } else if (!is.null(response_json$InformationList) && !is.null(response_json$InformationList$Information)) {
      cidsList[[i]] <- list(Compound = identifier[i], Info = response_json$InformationList$Information)

    } else {
      return(list())  # Return an empty list if neither CIDs nor Information is found
    }
    }
  }, error = function(e) {
    message(paste("An error occurred:", e$message))  # Log the error message
    return(list())  # Return an empty list in case of an error
  })

  # First, convert the list to a tibble (a type of data frame)
  cidsList <- lapply(cidsList, function(item) {
    item$CID <- as.character(item$CID)  # Convert CID to character
    return(item)
  })

  result <- tibble(data = cidsList)

  # Now, we need to unnest the data because it's in a complex form
  result <- result %>%
    mutate(row = row_number()) %>%  # This is to keep track of original list order
    unnest_wider(data) %>%  # This makes the Name and CID columns
    unnest_longer(CID) %>%  # This makes multiple rows if there are multiple CIDs
    select(-row)  # This removes the 'row' column, as we don't need it anymore

  return(result)
}

