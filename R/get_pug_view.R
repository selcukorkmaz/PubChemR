#' Retrieve PUG View Data from PubChem
#'
#' This function sends a request to the PubChem PUG View API to retrieve various types of data
#' for a given identifier. It supports fetching annotations, QR codes, and more, with options
#' for different output formats including JSON and SVG.
#'
#' @param annotation A character string specifying the type of annotation to retrieve.
#' @param identifier A single identifier for the query, either numeric or character.
#' @param domain A character string specifying the domain for the request. Default is 'compound'.
#' @param output A character string specifying the output format. Possible values are 'JSON', 'XML', and 'SVG'. Default is 'JSON'.
#' @param heading An optional character string for specifying a heading in the request.
#' @param headingType An optional character string for specifying a heading type in the request.
#' @param page An optional character string for specifying a page number in the request.
#' @param qrSize A character string specifying the size of the QR code. Possible values are 'short' and 'long'. Default is 'short'.
#' @param save A logical value indicating whether to save the output. Default is FALSE.
#'
#' @return Depending on the output format, this function returns different types of content:
#'         JSON or JSONP format returns parsed JSON content.
#'         SVG format returns an image object.
#'         For QR codes, it returns an image object or saves a PNG file.
#'
#' @examples
#'   get_pug_view(identifier = "2244", annotation = "linkout", domain = "compound")
#'
#' @importFrom RJSONIO fromJSON
#' @importFrom httr GET status_code
#' @importFrom magick image_read
#' @importFrom rsvg rsvg_png
#' @importFrom RCurl getURLContent
#' @importFrom png readPNG
#' @importFrom httr content
#' @importFrom xml2 as_list read_xml

#' @export

get_pug_view <- function(annotation = NULL, identifier = NULL, domain = 'compound',
                         output = 'JSON', heading = NULL, headingType = NULL, page = NULL,
                         qrSize = "short", save = FALSE) {

  # Check for missing annotation
  if (is.null(annotation)) {
    stop("annotation cannot be NULL")
  }

  if (is.numeric(identifier)) {
    identifier <- as.character(identifier)
  }

  # PUG-View does not support multiple identifiers in a single request
  if (length(identifier) > 1) {
    stop("Only one identifier is allowed per request")
  }

  if(domain == "key"){output = NULL}

  if(!is.null(identifier)){
    identifier <- URLencode(identifier)
  }

  # Build API URL
  api_base <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug_view"

  # Ensure the identifier is URL encoded
  # urlid <- URLencode(identifier)

  # Building the URL components
  comps <- Filter(Negate(is.null), list(api_base, annotation, domain, identifier, output))

  if (!is.null(heading)) {
    apiurl <- paste0(paste(comps, collapse = '/'), "?heading=", URLencode(sub(" ", "+", heading)))
  }

  else if (!is.null(headingType)) {
    apiurl <- paste0(paste(comps, collapse = '/'), "?heading_type=", URLencode(sub(" ", "+", headingType)))
  }

  else if (!is.null(page)) {
    apiurl <- paste0(paste(comps, collapse = '/'), "?page=", URLencode(sub(" ", "+", page)))
  }

  else if (annotation == "qr") {
    if(qrSize == "short"){
    comps <- Filter(Negate(is.null), list(api_base, annotation, "short", domain, identifier, output))
    }

    else if(qrSize == "long"){
      comps <- Filter(Negate(is.null), list(api_base, annotation, "long", domain, identifier, output))
    }

    apiurl <- paste(comps, collapse = '/')
  }

  else{
  apiurl <- paste(comps, collapse = '/')
  }

  # Simple Rate Limiting (5 requests per second)
  Sys.sleep(0.2)

  # Perform the GET request
  response <- GET(apiurl)

  # Check for successful response
  if (status_code(response) != 200) {
    stop("Error in API request: ", status_code(response))
  }

  # Handling response based on output format

  if (!is.null(output) && output %in% c('JSON')) {

    savedContent <- content(response, "text", encoding = "UTF-8")

    content <- fromJSON(savedContent)

    if(save){

    write(savedContent, file = paste0(domain, "_", identifier, ".", output))

    }

  }

  else if(!is.null(output) && output == "SVG" && domain != "key"){

    str <- charToRaw(content(response, as = "text", encoding = "UTF-8"))
    content <- image_read(str)

    if(save){

      rsvg_png(svg = str, file = paste0(identifier, ".png"))
    }

  }

  else if(domain == "key"){

    str <- readPNG(getURLContent(apiurl))
    content <- image_read(str)
  }

  else if(output == "XML"){
    responseContent <- rawToChar(response$content)
    xml_file <- read_xml(responseContent)
    content <-  xml2::as_list(xml_file)

    if(save){

      write_xml(xml_file, file = paste0(domain, "_", identifier, ".", output))

    }
  }

  else {
    content <- content(response, "text", encoding = "UTF-8")
  }

  return(content)
}

