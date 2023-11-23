#' Summarize Data from PubChem Based on Identifier
#'
#' This function provides a comprehensive summary of data from the PubChem database for a given identifier. It can retrieve information about compounds, substances, assays, and additional properties, including synonyms and SDF files.
#'
#' @param identifier A character string or numeric value representing the identifier for which the summary is required. It can be a compound ID (CID), substance ID (SID), assay ID (AID), or a name.
#' @param namespace A character string specifying the namespace of the identifier. Possible values include 'cid' for compound ID, 'sid' for substance ID, 'aid' for assay ID, and 'name' for common names or synonyms.
#' @param type A character vector indicating the type of data to retrieve. Possible values are "compound", "substance", and "assay". This parameter determines the kind of information fetched from PubChem.
#' @param properties An optional vector of property names to retrieve for the given identifier. If specified, the function fetches these properties from PubChem.
#' @param include_synonyms Logical; if TRUE, the function also retrieves synonyms for the given identifier.
#' @param include_sdf Logical; if TRUE, the function downloads the Structure-Data File (SDF) for the given identifier.
#' @param sdf_path An optional file path for saving the downloaded SDF file. If NULL and `include_sdf` is TRUE, the file is saved into a temporary folder with the identifier as its name.
#' @param sdf_file_name a character indicating the name of SDF file withoud ".sdf" extension. If NULL, default name is retrieved from \code{identifier} argument.
#' @param ... Additional arguments passed to internal functions.
#'
#' @return A list containing the requested data. The structure of the list depends on the parameters provided. It may include compound data, substance data, assay data, CIDs, SIDs, AIDs, synonyms, properties, and an SDF file path.
#'
#' @examples
#' \donttest{
#'   summary_data <- pubchem_summary(
#'     identifier = "aspirin",
#'     namespace = 'name',
#'     type = c("compound", "substance", "assay"),
#'     properties = "IsomericSMILES",
#'     include_synonyms = TRUE,
#'     include_sdf = TRUE
#'   )
#' }
#'
#' @importFrom RJSONIO fromJSON
#' @importFrom httr GET
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @export
pubchem_summary <- function(identifier,
                            namespace = 'cid',
                            type = c("compound", "substance", "assay"),
                            properties = NULL,
                            include_synonyms = FALSE,
                            include_sdf = FALSE,
                            sdf_path = NULL,
                            sdf_file_name = NULL,
                            ...) {

  summary_data <- list()
  check_request <- url.exists(request(identifier, namespace))

  # Fetch compound/substance/assay details based on namespace
  if ((namespace == 'cid' || namespace == "name") && any(type == "compound")) {
    # Retrieve Compound Data
    tryCatch({
      summary_data$Compound <- get_compounds(identifier, namespace)

      if (!is.null(summary_data$Compound)) {
        message("Successfully retrieved compound data.")
      } else {
        message("Failed to retrieve compound data.")
      }
    }, error = function(e) {
      message(e$message)
    })

    # Retrieve CIDs
    if (check_request){
      tryCatch({
        summary_data$CIDs <- get_cids(identifier, namespace)
        if (dim(summary_data$CIDs)[1] > 0) {
          message("Successfully retrieved CIDs.")
        } else {
          message("Failed to retrieve CIDs.")
        }
      }, error = function(e) {
        message(e$message)
      })
    } else {
      message("Failed to retrieve CIDs.")
    }
  }

  if ((namespace == 'sid' || namespace == "name") && any(type == "substance")) {
    # Retrieve Substance Data
    tryCatch({
      summary_data$Substance <- get_substances(identifier, namespace)

      if (!is.null(summary_data$Substance)) {
        message("Successfully retrieved substance data.")
      } else{
        message("Failed to retrieve substance data.")
      }
    }, error = function(e) {
      message(e$message)
    })

    # Retrieve SIDs
    if (check_request){
      tryCatch({
        summary_data$SIDs <- get_sids(identifier, namespace)
        if (dim(summary_data$SIDs)[1] > 0) {
          message("Successfully retrieved SIDs")
        } else {
          message("Failed to retrieve SIDs")
        }
      }, error = function(e) {
        message(e$message)
      })
    } else {
      message("Failed to retrieve SIDs.")
    }
  }

  if (namespace == 'aid' && any(type == "assay")) {
    # Retrieve Assay Data
    tryCatch({
      summary_data$Assay <- get_assays(identifier, namespace)

      if (!is.null(summary_data$Assay)) {
        message("Successfully retrieved assay data.")
      } else {
        message("Failed to retrieve assay data.")
      }
    }, error = function(e) {
      message(e$message)
    })

    # Retrieve AIDs
    if(check_request){
      tryCatch({
        summary_data$AIDs <- get_aids(identifier, namespace)
        if (dim(summary_data$AIDs)[1] > 0) {
          message("Successfully retrieved AIDs.")
        } else {
          message("Failed to retrieve AIDs.")
        }
      }, error = function(e) {
        message(e$message)
      })
    } else {
      message("Failed to retrieve AIDs.")
    }
  }

  # Fetch synonyms
  if (include_synonyms){
    if (check_request){
      tryCatch({
        summary_data$Synonyms <- get_synonyms(identifier, namespace)
        if (length(summary_data$Synonyms) > 0) {
          message("Successfully retrieved synonyms data.")
        } else{
          message("Failed to retrieve synonyms data.")
        }
      }, error = function(e) {
        message(e$message)
      })
    } else {
      message("Failed to retrieve synonyms data.")
    }
  }

  # Fetch properties if specified
  if (!is.null(properties)){
    if (check_request){
      tryCatch({
        summary_data$Properties <- get_properties(properties, identifier, namespace, as_dataframe = TRUE)

        if (nrow(summary_data$Properties) > 0){
          message("Successfully retrieved properties data.")
        } else {
          message("Failed to retrieve properties data.")
        }
      }, error = function(e) {
        message(e$message)
      })} else {
        message("Failed to retrieve properties data.")
      }
  }

  # Download SDF file if requested
  if (include_sdf) {
    if (check_request){
      tryCatch({
        if (is.null(sdf_file_name)){
          sdf_file_name <- identifier
        }
        if (is.null(sdf_path)){
          sdf_path <- tempdir(check = TRUE)
        }
        get_sdf(identifier, namespace, path = sdf_path, file_name = sdf_file_name)
        summary_data$SDF_File <- paste0(sdf_path, "/", sdf_file_name, ".sdf")
        message("Successfully downloaded SDF file.")
      }, error = function(e) {
        message(e$message)
      })
    } else {
      message("Failed to download SDF file.")
    }
  }

  return(summary_data)
}
