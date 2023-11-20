pubchem_summary <-
  function(identifier,
           namespace = 'cid',
           type = c("compound", "substance", "assay"),
           properties = NULL,
           include_synonyms = FALSE,
           include_sdf = FALSE,
           sdf_path = NULL,
           ...) {

    summary_data <- list()
    check_request <- url.exists(request(identifier, namespace))

    # Fetch compound/substance/assay details based on namespace

    if ((namespace == 'cid' ||
         namespace == "name") && any(type == "compound")) {
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

    if ((namespace == 'sid' ||
         namespace == "name") && any(type == "substance")) {
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
          sdf_filename <- ifelse(is.null(sdf_path), paste0(identifier, ".sdf"), sdf_path)
          get_sdf(identifier, namespace, path = sdf_filename)
          summary_data$SDF_File <- sdf_filename
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
# Example usage
# r <-
#   pubchem_summary(
#     identifier = "aspirin",
#     namespace = 'name',
#     type = c("compound", "substance", "assay"),
#     properties = "IsomericSMILES",
#     include_synonyms = TRUE,
#     include_sdf = TRUE,
#     sdf_path = NULL
#   )
