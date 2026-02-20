#' Retrieve Data from PubChem PUG REST API
#'
#' This function sends a request to the PubChem PUG REST API to retrieve various types of data
#' for a given identifier. It supports fetching data in different formats and allows saving the output.
#'
#'
#' @param identifier A vector of identifiers for the query, either numeric or character.
#'                   The type of identifier depends on the \code{namespace} parameter.
#'                   **Note**: \code{identifier} must be provided; it cannot be \code{NULL}.
#' @param namespace A character string specifying the namespace for the request.
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
#'                  - \code{sid}: Substance ID
#'
#'                  For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Input}{PUG REST API documentation}.
#'
#' @param domain A character string specifying the domain for the request.
#'
#'               Possible values include:
#'
#'               - \code{compound} (default)
#'
#'               - \code{substance}
#'
#'               - \code{assay}
#'
#'               For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Input}{PUG REST API documentation}.
#'
#' @param operation An optional character string specifying the operation for the request.
#'
#'                  Possible values depend on the \code{domain} and \code{namespace}.
#'
#'                  Examples include:
#'
#'                  - \code{property}
#'
#'                  - \code{synonyms}
#'
#'                  - \code{classification}
#'
#'                  - \code{conformers}
#'
#'                  - \code{cids}, \code{sids}, \code{aids} (to get related compound, substance, or assay IDs)
#'
#'                  If \code{NULL} (default), the default operation for the specified \code{domain} and \code{namespace} is used.
#'
#'                  For a full list of operations, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Operation}{PUG REST API documentation}.
#'
#' @param output A character string specifying the output format.
#'
#'               Possible values are:
#'
#'               - \code{JSON} (default)
#'
#'               - \code{JSONP}
#'
#'               - \code{XML}
#'
#'               - \code{CSV}
#'
#'               - \code{SDF}
#'
#'               - \code{TXT}
#'
#'               - \code{PNG}
#'
#'               For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Output}{PUG REST API documentation}.
#'
#' @param searchtype An optional character string specifying the search type.
#'
#'                   Possible values include:
#'
#'                   - \code{similarity}
#'
#'                   - \code{substructure}
#'
#'                   - \code{superstructure}
#'
#'                   If \code{NULL} (default), no search type is specified.
#'
#'                   For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Input}{PUG REST API documentation}.
#'
#' @param property An optional character string specifying the property or properties to retrieve.
#'
#'                 This is typically used when \code{operation} is \code{property}.
#'
#'                 Examples include:
#'
#'                 - \code{MolecularWeight}
#'
#'                 - \code{MolecularFormula}
#'
#'                 - \code{IUPACName}
#'
#'                 - \code{InChI}
#'
#'                 - \code{InChIKey}
#'
#'                 If \code{NULL} (default), all available properties are returned.
#'
#'                 For a full list of properties, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Compound-Property-Tables}{Compound Property Tables}.
#'
#' @param options A list of additional options for the request.
#'
#'                Available options depend on the specific request and the API.
#'
#'                Examples include:
#'
#'                - For similarity searches: \code{list(Threshold = 95)}
#'
#'                - For substructure searches: \code{list(MaxRecords = 100)}
#'
#'                If \code{NULL} (default), no additional options are included.
#'
#'                For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Structure-Search-Operations}{Structure Search Operations} section of the PUG REST API.
#'
#' @param save A logical value indicating whether to save the output as a file or image.
#'             Default is \code{FALSE}. If \code{TRUE}, the output will be saved to the path specified by \code{path}.
#' @param dpi An integer specifying the DPI for image output when \code{output} is \code{PNG}. Default is \code{300}.
#' @param path A character string specifying the directory path where the output file will be saved if \code{save} is \code{TRUE}.
#'             If \code{NULL} (default), a temporary directory will be used.
#' @param file_name A character string specifying the name of the file (without file extension) to save.
#'                  If \code{NULL} (default), the file name is set as \code{"files_downloaded"}.
#' @param ... Additional arguments passed to the underlying HTTP request functions.
#'
#' @details
#' For more information on the possible values for parameters such as \code{namespace}, \code{domain}, \code{operation},
#' \code{output}, \code{searchtype}, and \code{property}, please refer to the
#' \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=URL-based-API}{PUG REST API documentation}.
#'
#' @return An object of class `'PugRestInstance'` containing:
#' \describe{
#'   \item{`success`}{Logical value indicating if the request was successful.}
#'   \item{`error`}{If `success` is `FALSE`, a list containing error messages.}
#'   \item{`result`}{The content retrieved from the API; format depends on `output`.}
#'   \item{`request_args`}{A list of the arguments used in the request.}
#'   \item{`fileDetails`}{If `save` is `TRUE`, details about the saved file.}
#' }
#'
#' @examples
#' \donttest{
#'   result <- get_pug_rest(identifier = "2244",
#'                          namespace = "cid",
#'                          domain = "compound",
#'                          output = "JSON"
#'                         )
#'   pubChemData(result)
#' }
#'
#'
#' @importFrom httr GET RETRY
#' @importFrom RJSONIO fromJSON
#' @importFrom magick image_read
#' @importFrom png readPNG writePNG
#' @importFrom RCurl getURLContent curlEscape url.exists
#' @importFrom utils read.csv write.csv read.table write.table
#' @importFrom stringr str_split
#'
#' @export

get_pug_rest <- function(identifier = NULL, namespace = 'cid', domain = 'compound',
                         operation = NULL, output = 'JSON', searchtype = NULL, property = NULL, options = NULL,
                         save = FALSE, dpi = 300, path = NULL, file_name = NULL, ...) {
  # Create a structured object to return results or errors
  createPugRestObject <- function(success = TRUE, error = NULL, result = list(),
                                  request_args = list(), fileDetails = NULL) {
    structure(list(
      result = result,
      request_args = request_args,
      success = success,
      error = error,
      fileDetails = fileDetails
    ), class = "PugRestInstance")
  }

  # Function to format file size into size and unit
  format_file_size <- function(size_in_bytes) {
    units <- c("bytes", "KB", "MB", "GB", "TB")
    if (is.na(size_in_bytes) || size_in_bytes <= 0) {
      return(list(size = 0, unit = "bytes"))
    }
    exp <- floor(log(size_in_bytes, 1024))
    exp <- min(exp, length(units) - 1)
    size <- round(size_in_bytes / (1024 ^ exp), 2)
    unit <- units[exp + 1]
    return(list(size = size, unit = unit))
  }

  # Load required packages
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("The 'httr' package is required. Please install it using install.packages('httr').")
  }

  # Validate output parameter before any use
  if (is.null(output)) {
    return(createPugRestObject(
      success = FALSE,
      error = list(Message = "Invalid input: 'output' cannot be NULL."),
      request_args = list(
        identifier = identifier, namespace = namespace, domain = domain, operation = operation,
        output = output, searchtype = searchtype, property = property, options = options,
        dpi = dpi, path = path, save = save
      )
    ))
  } else {
    output <- toupper(output)
  }

  # Load 'png' package if needed
  if (output == "PNG") {
    if (!requireNamespace("png", quietly = TRUE)) {
      stop("The 'png' package is required for handling PNG outputs. Please install it using install.packages('png').")
    }
  }

  # Validate identifier
  if (is.null(identifier)) {
    return(createPugRestObject(
      success = FALSE,
      error = list(Message = "'identifier' cannot be NULL."),
      request_args = list(
        identifier = identifier, namespace = namespace, domain = domain, operation = operation,
        output = output, searchtype = searchtype, property = property, options = options,
        dpi = dpi, path = path, save = save
      )
    ))
  }

  # Validate options parameter
  if (!is.null(options)) {
    if (is.null(names(options)) || any(names(options) == "")) {
      return(createPugRestObject(
        success = FALSE,
        error = list(Message = "Invalid input: 'options' must be a named list."),
        request_args = list(
          identifier = identifier, namespace = namespace, domain = domain, operation = operation,
          output = output, searchtype = searchtype, property = property, options = options,
          dpi = dpi, path = path, save = save
        )
      ))
    }
  }

  # Handle file name and path for saving outputs
  if (save) {
    if (is.null(file_name)) {
      file_name <- paste0("files_downloaded", ".", output)
    } else {
      file_name <- paste0(file_name, ".", output)
    }

    if (is.null(path)) {
      path <- tempdir(check = TRUE)
    } else {
      if (!dir.exists(path)) {
        tryCatch({
          dir.create(path, recursive = TRUE)
        }, error = function(e) {
          path <- tempdir(check = TRUE)
          warning(paste0("Cannot create the specified 'path'. Using temporary path: ", path))
        })
      }
    }

    # Store file details in a list
    file_details <- list(Name = file_name, Path = path, Type = output)
  } else {
    file_details <- NULL
  }

  # Construct the base URL for PUG REST
  base_url <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug"
  apiurl <- paste(base_url, domain, sep = "/")

  # Add searchtype, namespace, identifier, operation, and property to URL as needed
  if (!is.null(searchtype)) {
    apiurl <- paste(apiurl, searchtype, sep = "/")
  }

  if (!is.null(namespace)) {
    apiurl <- paste(apiurl, namespace, sep = "/")
  }

  if (!is.null(identifier)) {
    # Convert identifier to character vector
    identifier <- as.character(identifier)
    encoded_identifier <- URLencode(identifier, reserved = TRUE)
    apiurl <- paste(apiurl, paste(encoded_identifier, collapse = ","), sep = "/")
  }

  if (!is.null(operation)) {
    apiurl <- paste(apiurl, paste(operation, collapse = "/"), sep = "/")
  }

  if (!is.null(property)) {
    apiurl <- paste(apiurl, "property", paste(property, collapse = ","), sep = "/")
  }

  # Append the output format to the URL
  apiurl <- paste(apiurl, output, sep = "/")

  # Add options as query parameters
  if (!is.null(options)) {
    query_params <- paste(names(options), unlist(options), sep = "=", collapse = "&")
    options_query <- paste0("?", query_params)
    apiurl <- paste0(apiurl, options_query)
  }

  # Perform the HTTP GET request within a tryCatch block
  response <- tryCatch({
    RETRY("GET", apiurl, times = 3, pause_min = 1, pause_base = 2)
  }, error = function(e) {
    return(createPugRestObject(
      success = FALSE,
      error = list(Message = paste("Failed to make API request:", conditionMessage(e))),
      request_args = list(
        identifier = identifier, namespace = namespace, domain = domain, operation = operation,
        output = output, searchtype = searchtype, property = property, options = options,
        dpi = dpi, path = path, save = save
      )
    ))
  })

  # If response is an error object, return it immediately
  if (inherits(response, "PugRestInstance")) {
    return(response)
  }

  # If the response is NULL due to an error, return an error object
  if (is.null(response)) {
    return(createPugRestObject(
      success = FALSE,
      error = list(Message = "Failed to get a response from the server."),
      request_args = list(
        identifier = identifier, namespace = namespace, domain = domain, operation = operation,
        output = output, searchtype = searchtype, property = property, options = options,
        dpi = dpi, path = path, save = save
      )
    ))
  }

  # Handle non-200 status codes
  if (!inherits(response, "response") || response$status_code != 200) {
    status_code <- if (inherits(response, "response")) response$status_code else "Unknown"
    return(createPugRestObject(
      success = FALSE,
      error = list(Message = paste("Error in API request: HTTP", status_code)),
      request_args = list(
        identifier = identifier, namespace = namespace, domain = domain, operation = operation,
        output = output, searchtype = searchtype, property = property, options = options,
        dpi = dpi, path = path, save = save
      )
    ))
  }

  # Parse and handle the response based on the output type
  content <- tryCatch({
    switch(output,
           "PNG" = {
             img_content_raw <- content(response, "raw")
             img_content <- readPNG(img_content_raw)
             if (save) {
               writePNG(img_content, target = file.path(path, file_name))
             }
             img_content
           },
           "SDF" = {
             if (save) {
               download.file(response$url, destfile = file.path(path, file_name), quiet = TRUE)
               content <- list()
             } else {
               content_text <- content(response, "text", encoding = "UTF-8")
               content_text
             }
           },
           "CSV" = {
             content_text <- content(response, "text", encoding = "UTF-8")
             if (save) {
               writeLines(content_text, con = file.path(path, file_name))
             }
             read.csv(text = content_text)
           },
           "TXT" = {
             content_text <- content(response, "text", encoding = "UTF-8")
             if (save) {
               writeLines(content_text, con = file.path(path, file_name))
             }
             # Parse content_text into a data frame
             lines <- unlist(strsplit(content_text, "\n"))
             lines <- lines[lines != ""]
             data_frame <- data.frame(Text = lines, stringsAsFactors = FALSE)
             data_frame
           },
           {
             # Handle JSON and JSONP formats or fallback to raw text
             content_text <- content(response, "text", encoding = "UTF-8")
             if (output == "JSONP") {
               content_text <- sub('[^;\\{]*', '', content_text)
               content_text <- sub('\\)$', '', content_text)
             }
             parsed_content <- fromJSON(content_text)
             if (save) {
               writeLines(content_text, con = file.path(path, file_name))
             }
             parsed_content
           }
    )
  }, error = function(e) {
    return(createPugRestObject(
      success = FALSE,
      error = list(Message = paste("Failed to parse API response:", conditionMessage(e))),
      request_args = list(
        identifier = identifier, namespace = namespace, domain = domain, operation = operation,
        output = output, searchtype = searchtype, property = property, options = options,
        dpi = dpi, path = path, save = save
      )
    ))
  })

  # If content parsing failed, return an error object
  if (is.null(content)) {
    return(createPugRestObject(
      success = FALSE,
      error = list(Message = "Failed to parse the API response."),
      request_args = list(
        identifier = identifier, namespace = namespace, domain = domain, operation = operation,
        output = output, searchtype = searchtype, property = property, options = options,
        dpi = dpi, path = path, save = save
      )
    ))
  }

  # Check file size and update file details if the content was saved
  if (save && !is.null(file_details)) {
    file_size_bytes <- file.info(file.path(path, file_name))$size
    if (!is.na(file_size_bytes)) {
      size_info <- format_file_size(file_size_bytes)
      file_details$Size <- size_info
    } else {
      file_details$Size <- list(size = NA, unit = "Unknown")
    }
  }

  # Return the final result object
  createPugRestObject(
    success = TRUE,
    result = content,
    request_args = list(
      identifier = identifier, namespace = namespace, domain = domain, operation = operation,
      output = output, searchtype = searchtype, property = property, options = options,
      dpi = dpi, path = path, save = save
    ),
    fileDetails = file_details
  )
}
