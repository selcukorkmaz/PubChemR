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
#' @export
#'
#' @examples
#' \donttest{
#' get_sdf(
#'   identifier = "aspirin",
#'   namespace = "name",
#'   path = NULL
#' )
#' }
#'
#' @export
get_sdf <- function(identifier, namespace = 'cid', domain = 'compound', operation = NULL,
                    searchtype = NULL, path = NULL, file_name = NULL, options = NULL) {

  # Validate inputs
  if (is.null(identifier) || length(identifier) == 0) {
    stop("Error: 'identifier' cannot be NULL. Please provide a valid identifier.")
  }
  ids <- as.character(identifier)

  # Handle file path and create directories if necessary
  if (is.null(path)) {
    message("'path' is not specified. Saving files into a temporary folder.")
    path <- tempdir(check = TRUE)
  } else {
    # Create the directory if it doesn't exist
    if (!dir.exists(path)) {
      dir_created <- tryCatch({
        dir.create(path, recursive = TRUE)
      }, error = function(e) {
        stop(paste("Error: Unable to create directory at specified 'path':", path, "\n", conditionMessage(e)))
      })
    }
  }

  if (!is.null(file_name)) {
    if (!is.character(file_name) || length(file_name) == 0) {
      stop("'file_name' must be NULL or a character vector.")
    }
    if (!(length(file_name) == 1 || length(file_name) == length(ids))) {
      stop("'file_name' must have length 1 or match length(identifier).")
    }
  }

  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  clean_id <- function(x) gsub("[^A-Za-z0-9._-]", "_", x)
  make_name <- function(i) {
    if (is.null(file_name)) {
      nm <- paste0(clean_id(ids[[i]]), "_", stamp)
    } else if (length(file_name) == 1 && length(ids) > 1) {
      nm <- paste0(file_name[[1]], "_", clean_id(ids[[i]]))
    } else {
      nm <- file_name[[i]]
    }

    if (!grepl("\\.sdf$", nm, ignore.case = TRUE)) {
      nm <- paste0(nm, ".sdf")
    }
    nm
  }

  saved <- character(0)
  for (i in seq_along(ids)) {
    id_i <- ids[[i]]
    file_i <- make_name(i)
    full_path <- file.path(path, file_i)

    one <- tryCatch({
      response_sdf <- request(id_i, namespace, domain, operation, 'SDF', searchtype, options)

      sdf_check <- tryCatch(httr::HEAD(response_sdf), error = function(e) NULL)
      if (!is.null(sdf_check) && httr::status_code(sdf_check) == 200) {
        download.file(response_sdf, full_path, quiet = TRUE, mode = "wb")
        message(
          "SDF file saved successfully:\n  Identifier: '", id_i,
          "'\n  File Name: '", file_i,
          "'\n  Saved at: ", path
        )
        full_path
      } else {
        message("Error: Received no content for identifier '", id_i, "'. URL may be invalid or content is missing.")
        NULL
      }
    }, error = function(e) {
      message("Failed to download SDF file for identifier '", id_i, "'. Error: ", conditionMessage(e))
      NULL
    })

    if (!is.null(one)) {
      saved <- c(saved, one)
    }
  }

  if (length(saved) == 0) {
    return(NULL)
  }
  if (length(saved) == 1) {
    return(invisible(saved[[1]]))
  }
  invisible(saved)
}
