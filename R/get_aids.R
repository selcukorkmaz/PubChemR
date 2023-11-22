
#' Retrieve Assay IDs (AIDs) from PubChem
#'
#' This function sends a request to PubChem to retrieve Assay IDs (AIDs) for a given identifier.
#' It returns either a tibble (data frame) with the provided identifier and the corresponding AIDs
#' or a list of AIDs, depending on the `as_data_frame` parameter.
#'
#' @param identifier A vector of positive integers (e.g. cid, sid, aid) or identifier strings (source, inchikey, formula). In some cases, only a single identifier string (name, smiles, xref; inchi, sdf by POST only).
#' @param namespace Specifies the namespace for the query. For the 'compound' domain, possible values include 'cid', 'name', 'smiles', 'inchi', 'sdf', 'inchikey', 'formula', 'substructure', 'superstructure', 'similarity', 'identity', 'xref', 'listkey', 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure', and 'fastformula'. For other domains, the possible namespaces are domain-specific.
#' @param domain Specifies the domain of the query. Possible values are 'substance', 'compound', 'assay', 'gene', 'protein', 'pathway', 'taxonomy', 'cell', 'sources', 'sourcetable', 'conformers', 'annotations', 'classification', and 'standardize'.
#' @param searchtype Specifies the type of search to be performed. For structure searches, possible values are combinations of 'substructure', 'superstructure', 'similarity', 'identity' with 'smiles', 'inchi', 'sdf', 'cid'. For fast searches, possible values are combinations of 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure' with 'smiles', 'smarts', 'inchi', 'sdf', 'cid', or 'fastformula'.
#' @param as_data_frame A logical value indicating whether to return the results as a tibble (data frame). Default is TRUE.
#' @param ... Additional arguments passed to \code{\link{get_json}}
#'
#' @return If `as_data_frame` is TRUE, a tibble (data frame) where each row corresponds to a provided identifier and its AID.
#'         The tibble has columns 'CID' and 'AID'. If `as_data_frame` is FALSE, a list of AIDs is returned.
#'
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @importFrom stringr str_to_title
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' get_aids(
#'   identifier = "aspirin",
#'   namespace = "name"
#' )
get_aids <- function(identifier, namespace = 'cid', domain = 'compound', searchtype = NULL, as_data_frame = TRUE, ...) {

  # Try to get the response and parse JSON
  result <- tryCatch({
    # Assuming 'get_json' is a function you've previously defined, similar to your Python environment
    aidsList <- list()

    for (i in 1:length(identifier)){
      response_json <- get_json(identifier[i], namespace, domain, 'aids', searchtype, ...)

      # Check if the response contains the expected information
      if (is.null(response_json)) {
        aidsList[[i]] <- list(Compound = identifier[i], aid = "No aid")

      } else if (!is.null(response_json$IdentifierList) && !is.null(response_json$IdentifierList$aid)) {
        aidsList[[i]] <- response_json$IdentifierList$aid

      } else if (!is.null(response_json$InformationList) && !is.null(response_json$InformationList$Information)) {
        aidsList[[i]] <- response_json$InformationList$Information[[1]]

      } else {
        return(list())  # Return an empty list if neither aids nor Information is found
      }
    }
  }, error = function(e) {
    message(paste("An error occurred:", e$message))  # Log the error message
    return(list())  # Return an empty list in case of an error
  })

  if (as_data_frame){
    # Initialize empty data frame
    df <- data.frame(CID = numeric(), AID = numeric(), stringsAsFactors = FALSE)
    resultList <- list()

    # Loop through each list
    for (i in seq_along(aidsList)) {
        # Extract CID. It assumes there's only one CID per sublist
        current_CID <- aidsList[[i]]$CID

        # Check if aids are present and are numeric
        if (is.numeric(aidsList[[i]]$AID)) {
          current_aid <- aidsList[[i]]$AID

          # Create a temporary data frame for current CID and its aids
          temp_df <- data.frame(CID = rep(current_CID, length(current_aid)),
                                AID = current_aid,
                                stringsAsFactors = FALSE)

          if (namespace == "name"){
            temp_df <- cbind(Identifier = identifier[i], temp_df)
            names(temp_df)[1] <- str_to_title(domain)
          }

          # Bind to the main data frame
          # df <- bind_rows(df, temp_df)
          resultList[[i]] <- temp_df

        } else if (is.character(aidsList[[i]]$AID)) {
          # Handle the case for "No aids" or similar cases
          # Here, we add the CID with an NA or a specific indicator for the aid
          temp_df <- data.frame(CID = current_CID,
                                aid = NA,  # or "No aids" or another indicator
                                stringsAsFactors = FALSE)

          if (namespace == "name"){
            temp_df <- cbind(Identifier = identifier[i], temp_df)
            names(temp_df)[1] <- str_to_title(domain)
          }

          # Bind to the main data frame
          # df <- bind_rows(df, temp_df)
          resultList[[i]] <- temp_df

        } else if (is.character(aidsList[[i]]$AID)) {
          # Handle the case for "No aids" or similar cases
          # Here, we add the CID with an NA or a specific indicator for the aid
          temp_df <- data.frame(CID = current_CID,
                                aid = NA,  # or "No aids" or another indicator
                                stringsAsFactors = FALSE)

          # Bind to the main data frame
          # df <- bind_rows(df, temp_df)
          resultList[[i]] <- temp_df
        }

      # Perhaps, we might prefer using DoCall(...) function under DescTools packace, which is claimed
      # faster alternative to base "do.call" function.
      df <- do.call(rbind.data.frame, resultList)
    }

    result <- df %>% as_tibble()

  } else {
    names(aidsList) <- paste0("'", identifier, "'")
    result <- aidsList
  }

  return(result)
}
