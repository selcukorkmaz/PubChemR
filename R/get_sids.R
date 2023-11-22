#' Retrieve Substance IDs (SIDs) from PubChem
#'
#' This function sends a request to PubChem to retrieve Substance IDs (SIDs) for a given identifier.
#' It returns a tibble (data frame) with the provided identifier and the corresponding SIDs.
#'
#' @param identifier A numeric or character vector specifying the identifiers for the request.
#' @param namespace A character string specifying the namespace for the request. Default is 'cid'.
#' @param domain A character string specifying the domain for the request. Default is 'compound'.
#' @param searchtype A character string specifying the search type. Default is NULL.
#' @param ... Additional arguments passed to \code{\link{get_json}}.
#'
#' @return A tibble (data frame) where each row corresponds to a provided identifier and its SID.
#'         The tibble has columns 'CID' and 'SID'.
#'
#' @importFrom RJSONIO fromJSON
#' @importFrom dplyr bind_rows
#' @importFrom tidyr as_tibble
#' @importFrom stringr str_to_title
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' get_sids(
#'   identifier = "aspirin",
#'   namespace = "name"
#' )
get_sids <- function(identifier, namespace = 'cid', domain = 'compound', searchtype = NULL, ...) {

  # Try to get the response and parse JSON
  result <- tryCatch({
    # Assuming 'get_json' is a function you've previously defined, similar to your Python environment
    sidsList = list()

    for (i in 1:length(identifier)){
      response_json <- get_json(identifier[i], namespace, domain, 'sids', searchtype, ...)

      # Check if the response contains the expected information
      if (is.null(response_json)) {
        sidsList[[i]] <- list(Compound = identifier[i], SID = "No SID")

      } else if (!is.null(response_json$IdentifierList) && !is.null(response_json$IdentifierList$SID)) {
        sidsList[[i]] <- response_json$IdentifierList$SID

      } else if (!is.null(response_json$InformationList) && !is.null(response_json$InformationList$Information)) {
        sidsList[[i]] <- response_json$InformationList$Information

      } else {
        return(list())  # Return an empty list if neither SIDs nor Information is found
      }

    }
  }, error = function(e) {
    message(paste("An error occurred:", e$message))  # Log the error message
    return(list())  # Return an empty list in case of an error
  })

  # Initialize empty data frame
  df <- data.frame(CID = numeric(), SID = numeric(), stringsAsFactors = FALSE)
  resultList <- list()

  # Loop through each list
  for (i in seq_along(sidsList)) {
    for (j in seq_along(sidsList[[i]])) {
      # Extract CID. It assumes there's only one CID per sublist
      current_CID <- sidsList[[i]][[j]]$CID

      # Check if SIDs are present and are numeric
      if (is.numeric(sidsList[[i]][[j]]$SID)) {
        current_SID <- sidsList[[i]][[j]]$SID

        # Create a temporary data frame for current CID and its SIDs
        temp_df <- data.frame(CID = rep(current_CID, length(current_SID)),
                              SID = current_SID,
                              stringsAsFactors = FALSE)

        # Bind to the main data frame
        if (namespace == "name"){
          temp_df <- cbind(Identifier = identifier[i], temp_df)
          names(temp_df)[1] <- str_to_title(domain)
        }

        # Bind to the main data frame
        # df <- bind_rows(df, temp_df)
        resultList[[i]] <- temp_df

      } else if (is.character(sidsList[[i]][[j]]$SID)) {
        # Handle the case for "No SIDs" or similar cases
        # Here, we add the CID with an NA or a specific indicator for the SID
        temp_df <- data.frame(CID = current_CID,
                              SID = NA,  # or "No SIDs" or another indicator
                              stringsAsFactors = FALSE)

        # Bind to the main data frame
        if (namespace == "name"){
          temp_df <- cbind(Identifier = identifier[i], temp_df)
          names(temp_df)[1] <- str_to_title(domain)
        }

        # Bind to the main data frame
        # df <- bind_rows(df, temp_df)
        resultList[[i]] <- temp_df
      }
    }
  }

  df <- do.call(rbind.data.frame, resultList)
  result <- df %>% as_tibble()

  return(result)
}

