#' Retrieve SDF Data from PubChem and Save as File
#'
#' This function sends a request to PubChem to retrieve data in SDF format based on the specified parameters.
#' It then saves the retrieved data as an SDF file in the current working directory.
#'
#' @param identifier A character or numeric value specifying the identifier for the request.
#' @param namespace A character string specifying the namespace for the request. Default is 'cid'.
#' @param domain A character string specifying the domain for the request. Default is 'compound'.
#' @param operation An optional character string specifying the operation for the request.
#' @param searchtype An optional character string specifying the search type.
#' @param path A string indicating the path to the folder where the SDF files will be saved. Default is NULL (i.e., saves the file into a temporary folder).
#' @param file_name A string. File name for downloaded SDF file. If NULL, "file" is used as the file name. Default is NULL.
#' @param options Additional parameters to be passed to the \code{\link{request}}.
#'
#' @return NULL. The function saves the retrieved data as an SDF file in the current working directory and prints a
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
get_sdf <- function(identifier, namespace = 'cid', domain = 'compound', operation = NULL, searchtype = NULL, path = NULL, file_name = NULL, options = NULL) {

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

  # Use tryCatch to handle errors gracefully
  result <- tryCatch({
    # Make the request. The 'get' function is expected to return the response content directly.
    response_sdf <- request(identifier, namespace, domain, operation, 'SDF', searchtype, options)

    # Check if the response is not empty or NULL before proceeding
    if (url.exists(response_sdf)) {
      # Write the content to a file in SDF format in the current working directory
      download.file(response_sdf, file.path(path, file_name))
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
    # return(NULL)  # Return NULL to indicate no result or failure in the process
  })
}

