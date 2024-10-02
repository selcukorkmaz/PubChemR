#' Retrieve PUG View Data from PubChem
#'
#' This function sends a request to the PubChem PUG View API to retrieve various types of data
#' for a given identifier. It supports fetching annotations, QR codes, and more, with options
#' for different output formats including JSON and SVG.
#'
#' @param annotation A character string specifying the type of annotation to retrieve.
#'
#'                   Valid values are:
#'
#'                    \code{annotations} (default): Retrieve annotations across all of PubChem’s primary databases.
#'
#'                    \code{categories}: List all PubChem depositors and their SIDs for a given compound, including categorization.
#'
#'                    \code{literature}: Retrieve URLs into PubMed for literature associated with a compound, organized by subheading.
#'
#'                    \code{image}: Display biologic images associated with compounds.
#'
#'                    \code{qr}: Generate QR codes that link to the LCSS page for a compound.
#'
#'                    \code{linkout}: Retrieve all the NCBI LinkOut records present for a substance, compound, or assay.
#'
#'                    \code{structure}: Retrieve 3D protein structures associated with a compound.
#'
#' @param identifier A single identifier for the query, either numeric or character.
#'                   **Note:** Only one identifier is allowed per request for certain annotations.
#'                   For some annotations like \code{annotations} with a \code{heading}, the \code{identifier} can be \code{NULL}.
#' @param domain A character string specifying the domain for the request.
#'
#'               Possible values include:
#'
#'
#'               \code{compound} (default)
#'
#'               \code{substance}
#'
#'               - Other domains as specified in the API documentation.
#' @param output A character string specifying the output format.
#'
#'               Possible values include:
#'
#'               \code{JSON} (default)
#'
#'               \code{XML}
#'
#'               \code{CSV}
#'
#'               \code{TXT}
#'
#'               \code{PNG}
#'
#'               \code{SVG}
#'
#' @param heading An optional character string specifying a heading to filter the data.
#'                Used with \code{annotations} when \code{identifier} is \code{NULL}.
#' @param headingType An optional character string specifying a heading type to filter the data.
#'                     Possible values include \code{Compound}, \code{Substance}, etc.
#' @param page An optional integer specifying a page number for pagination.
#' @param qrSize A character string specifying the size of the QR code.
#'                Possible values are \code{short} (default) and \code{long}. Used when \code{annotation} is \code{qr}.
#' @param save A logical value indicating whether to save the output to a file. Default is \code{FALSE}.
#'
#' @details
#' The PubChem PUG View API allows users to retrieve detailed information about compounds, substances, and assays.
#' This function constructs the appropriate API call based on the provided parameters.
#' For more detailed information, please refer to the
#' \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-view}{PubChem PUG View API documentation}.
#'
#' @return Depending on the output format, this function returns different types of content:
#'         JSON or JSONP format returns parsed JSON content.
#'         SVG format returns an image object.
#'         For QR codes, it returns an image object or saves a PNG file.
#'
#' @examples
#' \donttest{
#' result <- get_pug_view(identifier = "2244", annotation = "linkout", domain = "compound")
#' retrieve(result, .slot = "ObjUrl", .to.data.frame = FALSE)
#' }
#'
#' @importFrom RJSONIO fromJSON
#' @importFrom httr GET status_code
#' @importFrom magick image_read
#' @importFrom rsvg rsvg_png
#' @importFrom RCurl getURLContent
#' @importFrom png readPNG
#' @importFrom httr content
#'
#' @export
get_pug_view <- function(annotation = NULL, identifier = NULL, domain = 'compound',
                         output = 'JSON', heading = NULL, headingType = NULL, page = NULL,
                         qrSize = "short", save = FALSE) {

  # Create a helper function to construct a Pug View object for errors and successful responses
  createPugViewObject <- function(error = FALSE, result = list(), request_args = list(), subclass = NULL, ...) {
    dots <- list(...)
    tmp <- list(
      result = result,
      request_args = request_args,
      success = !error,
      error = NULL
    )
    if (length(dots) > 0) {
      tmp <- c(tmp, dots)
    }
    structure(
      tmp,
      class = c("PugViewInstance", subclass)
    )
  }

  # Check for missing or invalid arguments and handle them gracefully
  if (is.null(annotation)) {
    warning("Annotation cannot be NULL. Setting 'annotation' to 'data' by default.")
    annotation <- "data"
  }

  if (is.null(identifier)) {
    results <- createPugViewObject(error = TRUE)
    results$error <- list(Message = "'identifier' cannot be NULL.")
    return(results)
  }

  # Convert numeric identifiers to character
  if (is.numeric(identifier)) {
    identifier <- as.character(identifier)
  }

  # Handle multiple identifiers by only using the first one
  if (length(identifier) > 1) {
    warning(paste0("One identifier is allowed per request. Using only the first element: ", identifier[1]))
    identifier <- identifier[1]
  }

  # URL encode the identifier to make it URL-safe
  if (!is.null(identifier)) {
    identifier <- URLencode(identifier)
  }

  # Define the base API URL
  api_base <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug_view"

  # Construct the API URL based on the input parameters
  comps <- Filter(Negate(is.null), list(api_base, annotation, domain, identifier, output))
  apiurl <- paste(comps, collapse = '/')

  # Additional query parameters if needed
  if (!is.null(heading)) {
    apiurl <- paste0(apiurl, "?heading=", URLencode(sub(" ", "+", heading)))
  } else if (!is.null(headingType)) {
    apiurl <- paste0(apiurl, "?heading_type=", URLencode(sub(" ", "+", headingType)))
  } else if (!is.null(page)) {
    apiurl <- paste0(apiurl, "?page=", URLencode(sub(" ", "+", page)))
  } else if (annotation == "qr") {
    qr_segment <- if (qrSize == "short") "short" else "long"
    apiurl <- paste(c(api_base, annotation, qr_segment, domain, identifier, output), collapse = '/')
  }

  # Implement simple rate limiting (5 requests per second)
  Sys.sleep(0.2)

  # Perform the GET request and handle potential errors
  response <- tryCatch({
    GET(apiurl)
  }, error = function(e) {
    results <- createPugViewObject(error = TRUE)
    results$error <- list(Message = "Failed to make API request", Code = conditionMessage(e))
    return(results)
  })

  # Check if the request was successful
  if (inherits(response, "PugViewInstance")) return(response)  # Return early if the response is an error

  # Handle HTTP errors
  if (status_code(response) != 200) {
    results <- createPugViewObject(error = TRUE)
    results$error <- list(
      Message = "Error in API request",
      Code = paste0("HTTP Error ", status_code(response))
    )
    return(results)
  }

  # Parse the response content based on the output format
  content <- tryCatch({
    if (!is.null(output) && output == 'JSON') {
      savedContent <- content(response, "text", encoding = "UTF-8")
      parsed_content <- fromJSON(savedContent)
      if (save) {
        write(savedContent, file = paste0(domain, "_", identifier, ".", output))
      }
      parsed_content
    } else if (!is.null(output) && output == "SVG" && domain != "key") {
      svg_content <- charToRaw(content(response, as = "text", encoding = "UTF-8"))
      img_content <- image_read(svg_content)
      if (save) {
        rsvg_png(svg = svg_content, file = paste0(identifier, ".png"))
      }
      img_content
    } else if (domain == "key") {
      img_content <- image_read(readPNG(getURLContent(apiurl)))
      img_content
    } else {
      content(response, "text", encoding = "UTF-8")
    }
  }, error = function(e) {
    results <- createPugViewObject(error = TRUE)
    results$error <- list(Message = "Failed to parse API response", Code = conditionMessage(e))
    return(results)
  })

  # If content parsing failed, return the error object
  if (inherits(content, "PugViewInstance")) return(content)

  # Return the successfully parsed content as a PugViewInstance object
  createPugViewObject(
    result = content,
    request_args = list(
      annotation = annotation,
      identifier = identifier,
      domain = domain,
      output = output,
      heading = heading,
      headingType = headingType,
      page = page,
      qrSize = qrSize,
      save = save
    )
  )
}


