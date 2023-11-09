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
#' @param ... Additional parameters to be passed to the \code{\link{request}}.
#'
#' @return NULL. The function saves the retrieved data as an SDF file in the current working directory and prints a
#' message indicating the file's location.
#'
#' @importFrom utils download.file
#'
#' @examples
#' \dontrun{
#'  get_sdf(
#'   identifier = "aspirin",
#'   namespace = "name",
#'  )
#' }
#'
#' @export
get_sdf <- function(identifier, namespace = 'cid', domain = 'compound', operation = NULL, searchtype = NULL, ...) {

  # Generate a file name based on the identifier, ensuring it ends with the .sdf extension
  file_name <- paste0(identifier, "_", Sys.time(), ".sdf")  # Adding a timestamp for uniqueness
  file_name <- trimws(file_name) # Remove leading and trailing white spaces.
  file_name <- gsub(" ", "_", gsub(":", "_", file_name))  # Replace spaces with underscores, if any

  # Use tryCatch to handle errors gracefully
  result <- tryCatch({
    # Make the request. The 'get' function is expected to return the response content directly.
    response_sdf <- request(identifier, namespace, domain, operation, 'SDF', searchtype, ...)

    # Check if the response is not empty or NULL before proceeding
    if (!is.null(response_sdf) && nzchar(response_sdf)) {
      # Write the content to a file in SDF format in the current working directory
      download.file(response_sdf, paste0("./", file_name))
      message(paste("SDF file has been saved in the current working directory as:", paste0(getwd(), "/", file_name)))
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

  return(result)
}

