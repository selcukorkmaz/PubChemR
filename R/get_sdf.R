#' @title Retrieve/Save SDF Data from PubChem
#'
#' @description This function sends a request to PubChem to retrieve data in SDF format based on the specified parameters.
#' It then saves the retrieved data as an SDF file in the current working directory (or into the system-specific temporary folder).
#'
#' @param identifier A vector of compound identifiers, either numeric or character.
#'                   The type of identifier depends on the \code{namespace} parameter.
#'                   **Note**: \code{identifier} must be provided; it cannot be \code{NULL}.
#' @param namespace A character string specifying the namespace of the identifier.
#'
#'                  Possible values include:
#'
#'                  - \code{cid}: PubChem Compound Identifier (default)
#'
#'                  - \code{name}: Chemical name
#'
#'                  - \code{smiles}: SMILES string
#'
#'                  - \code{inchi}: InChI string
#'
#'                  - \code{inchikey}: InChIKey
#'
#'                  - \code{formula}: Molecular formula
#'
#'                  - Other namespaces as specified in the API documentation.
#'
#'                  For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Input}{Input} section of the PUG REST API.
#'
#' @param domain A character string specifying the domain of the query.
#'
#'               Possible values include:
#'
#'               - \code{compound} (default)
#'
#'               - Other domains as specified in the API documentation.
#'
#' @param operation A character string specifying the operation to perform.
#'                  For SDF retrieval, the operation is typically \code{NULL} or \code{record}.
#'                  If \code{NULL} (default), the basic compound record is retrieved.
#'                  For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Operation}{Operations} section of the PUG REST API.
#' @param searchtype An optional character string specifying the search type.
#'
#'                   Possible values include:
#'
#'                   - \code{substructure}
#'
#'                   - \code{superstructure}
#'
#'                   - \code{similarity}
#'
#'                   - \code{identity}
#'
#'                   - Other search types as specified in the API documentation.
#'
#'                   If \code{NULL} (default), no search type is specified.
#'
#'                   For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Input}{Input} section of the PUG REST API.
#'
#' @param path A character string specifying the directory path where the SDF file will be saved.
#'             If \code{NULL} (default), the file is saved in a temporary directory.
#' @param file_name A character string specifying the name of the SDF file (without file extension).
#'                  If \code{NULL} (default), a file name is generated based on the \code{identifier} and timestamp.
#' @param options A list of additional options for the request.
#'                Available options depend on the specific request and the API.
#'                If \code{NULL} (default), no additional options are included.
#'                For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Structure-Search-Operations}{Structure Search Operations} section of the PUG REST API.
#'
#' @details
#' The PubChem PUG REST API allows users to retrieve compound data in various formats, including SDF.
#' This function constructs the appropriate API call and saves the SDF data to a file.
#' For more detailed information, please refer to the
#' \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest}{PubChem PUG REST API documentation}.

#'
#' @return The function saves the retrieved data as an SDF file in the current working directory and prints a
#' message indicating the file's location.
#'
#' @importFrom utils download.file
#' @importFrom RCurl url.exists
#' @export
#'
#' @examples
#' get_sdf(
#'   identifier = "aspirin",
#'   namespace = "name",
#'   path = NULL
#' )
#'
#' @export
get_sdf <- function(identifier, namespace = 'cid', domain = 'compound', operation = NULL,
                    searchtype = NULL, path = NULL, file_name = NULL, options = NULL) {

  if (is.null(file_name)){
    # Generate a file name based on the identifier, ensuring it ends with the .sdf extension
    file_name <- paste0(identifier, "_", Sys.time(), ".sdf")  # Adding a timestamp for uniqueness
    file_name <- trimws(file_name) # Remove leading and trailing white spaces.
    file_name <- gsub(" ", "_", gsub(":", "_", file_name))  # Replace spaces with underscores, if any
  } else {
    file_name <- paste0(file_name, ".sdf")
  }

  if (is.null(path)){
    message("'path' is not specified. Saving files into a temporary folder.")
    path <- tempdir(check = TRUE)
  } else {
    if (!file.exists(path)){
      dir.create(path, recursive = TRUE)
    }
  }

  # Initialize the full path
  full_path <- file.path(path, file_name)

  # Use tryCatch to handle errors gracefully
  result <- tryCatch({
    # Make the request. The 'get' function is expected to return the response content directly.
    response_sdf <- request(identifier, namespace, domain, operation, 'SDF', searchtype, options)

    # Check if the response is not empty or NULL before proceeding
    if (url.exists(response_sdf)) {
      # Write the content to a file in SDF format in the current working directory
      download.file(response_sdf, full_path)
      message("  SDF file to save --> '", file_name, "'", sep = "", "\n")
      message("  Saved into folder --> ", path, sep = "", "\n")
      message("  Completed options", "\n")
    } else {
      message("Received no content to write to the SDF file.")
      return(NULL)
    }
  }, error = function(e) {
    # Here, you could check for specific types of errors (like NotFoundError)
    # and handle them as needed. For simplicity, we're just printing the error message.
    message(paste("Info:", e$message))
    return(NULL)  # Return NULL to indicate no result or failure in the process
  })

  # Return the full path invisibly if no error occurred
  return(invisible(full_path))
}
