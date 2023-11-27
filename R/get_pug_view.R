library(rsvg)
library(magick)
library(png)
library(RCurl)

get_pug_view <- function(annotation = NULL, identifier = NULL, domain = 'compound',
                         output = 'JSON', heading = NULL, headingType = NULL, page = NULL,
                         qrSize = "short", savePNG = FALSE) {

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
  identifier <- sub(" ", "%20", identifier)

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
  if (!is.null(output) && output %in% c('JSON', 'JSONP')) {
    content <- fromJSON(content(response, "text"))
  }

  else if(!is.null(output) && output == "SVG" && domain != "key"){

    str <- charToRaw(content(response, as = "text", encoding = "UTF-8"))
    content <- image_read(str)

    if(savePNG){

      rsvg_png(svg = str, file = paste0(identifier, ".png"))
    }

  }

  else if(domain == "key"){

    str <- readPNG(getURLContent(apiurl))
    content <- image_read(str)
  }

  else {
    content <- content(response, "text")
  }

  return(content)
}

# Note: Users should be aware of rate limiting and usage policies of PUG-View.
# Documentation: [Add documentation here with examples and parameter descriptions]

get_pug_view(identifier = "2244", annotation = "linkout",
             domain = "compound")



