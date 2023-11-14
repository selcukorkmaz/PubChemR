pubchem_summary <- function(identifier, namespace = 'cid', type = c("compound", "substance", "assay"),
                            properties = NULL, include_sdf = FALSE, sdf_path = NULL, ...) {

  result <- tryCatch({
    # Assuming 'get_json' is a function you've previously defined, similar to your Python environment
    summary_data <- list()

    # Fetch compound/substance/assay details based on namespace
    if ((namespace == 'cid' || namespace == "name") && any(type == "compound")) {

      summary_data$Compound <- get_compounds(identifier, namespace)
      summary_data$CIDs <- get_cids(identifier, namespace)

      if(!is.null(summary_data$Compound) && !is.null(summary_data$CIDs)){

        message("Successfully retrieved compound data.")

      }
    }

    if ((namespace == 'sid' || namespace == "name") && any(type == "substance")) {
      summary_data$Substance <- get_substances(identifier, namespace)
      summary_data$SIDs <- get_sids(identifier, namespace)

      if(!is.null(summary_data$Substance) && !is.null(summary_data$SIDs)){

        message("Successfully retrieved substance data.")

      }
    }

    if (namespace == 'aid' && any(type == "assay")) {
      summary_data$Assay <- get_assays(identifier, namespace)
      summary_data$AIDs <- get_aids(identifier, namespace)

      if(!is.null(summary_data$Assay) && !is.null(summary_data$AIDs)){

        message("Successfully retrieved assay data.")

      }
    }

    # Fetch synonyms
    summary_data$Synonyms <- get_synonyms(identifier, namespace)

    if(!is.null(summary_data$Synonyms)){

      message("Successfully retrieved synonyms data.")

    }

    # Fetch properties if specified
    if (!is.null(properties)) {
      summary_data$Properties <- get_properties(properties, identifier, namespace, as_dataframe = TRUE)

      if(!is.null(summary_data$Properties)){

        message("Successfully retrieved properties data.")

      }
    }

    # Download SDF file if requested
    if (include_sdf) {
      sdf_filename <- ifelse(is.null(sdf_path), paste0(identifier, ".sdf"), sdf_path)
      get_sdf(identifier, namespace, path = sdf_filename)
      summary_data$SDF_File <- sdf_filename

      message("Successfully downloaded SDF file.")

    }

    return(summary_data)


  }, error = function(e) {
    # message(paste("An error occurred:", e$message))  # Log the error message
    return(list())  # Return an empty list in case of an error
  })

  return(result)

}

r = pubchem_summary(identifier= 12345, namespace = 'aid', type = c("compound", "assay"), properties = c("IsomericSMILES", "MolecularWeight"),
                            include_sdf = FALSE, sdf_path = NULL)


