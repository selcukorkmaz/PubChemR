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
#' # Retrieve compound information in JSON format
#' result <- get_pug_rest(identifier = "2244", namespace = "cid", domain = "compound", output = "JSON")
#'
#' # Retrieve molecular formula and molecular weight for a compound
#' result <- get_pug_rest(
#'   identifier = "2244",
#'   namespace = "cid",
#'   domain = "compound",
#'   operation = "property",
#'   property = c("MolecularFormula", "MolecularWeight"),
#'   output = "JSON"
#' )
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

  # Create empty Pug View structure to be used when error returns.
  createPugRestObject <- function(success = TRUE, error = NULL, result = list(),
                                  request_args = list(), ...){
    dots <- list(...)

    tmp <- list(
      result = result,
      request_args = request_args,
      success = success,
      error = error
    )

    if (length(dots) > 0){
      tmp <- c(tmp, dots)
    }

    structure(
      tmp,
      class = "PugRestInstance"
    )
  }

  # dots <- list(...)
  call_args <- list(identifier = identifier, namespace = namespace, domain = domain,
                    operation = operation, output = output, searchtype = searchtype,
                    property = property, options = options, dpi = dpi, path = path,
                    save = save)

  if (!is.null(output)){
    output = toupper(output)
  } else {
    PugREST_List <- createPugRestObject(
      success = FALSE,
      error = list(Message = "Incorrect input defined. 'output' cannot be NULL."),
      request_args = call_args
    )
    return(PugREST_List)
  }

  # If requested to save specific files, path is checked and created.
  if (output == 'SDF' | save){
    # Write the content to a file in SDF format in the given 'path'
    if (is.null(file_name)){
      file_name <- paste0("files_downloaded", ".", output)
    } else {
      if (length(file_name) > 1){
        file_name <- file_name[1]
      }
      file_name <- paste0(file_name, ".", output)
    }

    fileType <- case_when(
      .default = "JavaScript Object Notation (JSON/JSONP)",
      output == "CSV" ~ "Comma Separated Values (CSV)",
      output == "TXT" ~ "Text Files (TXT)",
      output == "SDF" ~ "Structure Data Files (SDF)",
      output == "PNG" ~ "Portable Network Graphic (PNG)"
    )

    if (is.null(path)){
      path <- tempdir(check = TRUE)
    } else {
      # Check if given path exists or successfully created
      if (!dir.exists(path)){
        try(dir.create(path, recursive = TRUE, showWarnings = FALSE))

        if (!dir.exists(path)){
          path <- tempdir(check = TRUE)
          warning(paste0("Cannot create given 'path'. Saving into temporary path '", path, "'"))
        }
      }
    }

    file_details <- list(Name = file_name, Path = path, Type = fileType,
                         Size = calculateObjectSize(file.path(path, file_name)))

    # replace path argument if it is set NULL or given path is not valid.
    call_args$path <- path
  }

  # Construct the base URL for PUG REST
  base_url <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug"

  # Build the URL with the given parameters
  apiurl <- paste0(base_url, "/", domain, "/", namespace, "/")

  # Add searchtype to the URL if provided
  if (!is.null(searchtype)) {
    searchtype <- paste0(searchtype, collapse = "/")
    apiurl <- paste0(base_url, "/", domain, "/", searchtype, "/", namespace, "/")
  }

  # Add identifier to the URL if provided
  if (!is.null(identifier)) {
    apiurl <- paste0(apiurl, paste0(toupper(identifier), collapse = ","), "/")
  }

  # Add operation to the URL if provided
  if (!is.null(operation)) {
    operation <- paste0(operation, collapse = "/")
    apiurl <- paste0(apiurl, operation, "/")
  }

  # Add searchtype to the URL if provided
  if (!is.null(property)) {
    if (length(property) > 1){
      property = paste0(property, collapse = ",")
    }

    apiurl <- paste0(apiurl, "property/", property, "/")
  }

  # Finalize URL with output format
  apiurl <- paste0(apiurl, output)

  # Add options to the URL if provided
  if (!is.null(options)) {
    if (namespace == "inchi"){
      options <- paste0("?", paste0("inchi=", curlEscape(unlist(options)), collapse = "&"))
      options <- gsub(" ", "", options)
    } else {
      options <- paste0("?", paste0( names(options), unlist(options), collapse = "&"))
      options <- gsub(" ", "", options)
    }

    apiurl <- paste0(apiurl, options)
  }

  if (output == "SDF"){
    if (url.exists(apiurl)){
      tmp <- try(download.file(apiurl, file.path(path, file_name)))

      PugREST_List <- if (inherits(tmp, "try-error")){
        createPugRestObject(
          success = FALSE,
          error = list(Message = "Cannot donwload SDF file. Please check inputs and try again."),
          request_args = call_args
        )
      } else {
        createPugRestObject(
          success = TRUE,
          request_args = call_args,
          fileDetails = file_details
        )
      }
    } else {
      PugREST_List <- createPugRestObject(
        success = FALSE,
        error = list(Message = "URL does not exist. Received no content to write SDF file."),
        request_args = call_args
      )
    }
    return(PugREST_List)
  } else {
    # Make the HTTP GET request and return the response
    # response <- GET(URLencode(apiurl))
    response <- RETRY("GET", URLencode(apiurl), times = 3, pause_min = 1, pause_base = 2)

    if (response$status_code != 400){
      # Check if the response is asking to wait and has a ListKey
      if ('Waiting' %in% names(content) && !is.null(content$Waiting[["ListKey"]])) {
        identifier <- content$Waiting[["ListKey"]]
        namespace <- 'listkey'

        iter <- 1
        while ('Waiting' %in% names(content) && !is.null(content$Waiting[["ListKey"]]) && iter < 4) {
          # Delay before making the next request
          Sys.sleep(2)  # delay for 2 seconds
          # Make the next request
          response <- GET(request(identifier, namespace, domain, operation, output, options))
          responseContent <- rawToChar(response$content)

          content <- fromJSON(responseContent)
          iter <- iter + 1
        }
      }

      if (output == "CSV"){
        content <- read.csv(response$url)
        if (save){
          write.csv(content, file = file.path(path, file_name), row.names = FALSE)
        }
      }

      # Handling response based on output format
      if (output %in% c('JSON', 'JSONP')) {
        savedContent <- content(response, "text", encoding = "UTF-8")

        if (output == "JSONP"){
          savedContent <- sub('[^;\\{]*', '', savedContent)  # remove function name and opening parenthesis
          savedContent <- sub('\\)$', '', savedContent) # remove closing parenthesis
        }

        content <- fromJSON(savedContent)

        if (save){
          write(savedContent, file = file.path(path, file_name))
        }
      }

      # Fetch PNG image from PubChem
      if (!is.null(output) && output == "PNG"){
        content <- readPNG(getURLContent(apiurl))
        print(image_read(content), info = FALSE)

        if (save){
          writePNG(content, target = file.path(path, file_name), dpi = dpi)
        }
      }

      if (output == "TXT"){
        content <- read.table(response$url)

        if (save){
          write.table(content, file = file.path(path, file_name),
                      sep = "\t", quote = FALSE, row.names = FALSE, fileEncoding = "UTF-8")
        }
      }

      PugREST_List <- createPugRestObject(success = TRUE, request_args = call_args,
                          result = content)

      if (save){
        PugREST_List$fileDetails <- file_details
      }
    } else {
      error_message <- fromJSON(rawToChar(response$content))[["Fault"]]
      PugREST_List <- createPugRestObject(success = FALSE, error = lapply(error_message, "[", 1),
                                          request_args = call_args)
    }

    return(PugREST_List)
  }
}

