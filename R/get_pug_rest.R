#' Retrieve Data from PubChem PUG REST API
#'
#' This function sends a request to the PubChem PUG REST API to retrieve various types of data
#' for a given identifier. It supports fetching data in different formats and allows saving the output.
#'
#' @param identifier A single identifier for the query, either numeric or character.
#' @param namespace A character string specifying the namespace for the request. Default is 'cid'.
#' @param domain A character string specifying the domain for the request. Default is 'compound'.
#' @param operation An optional character string specifying the operation for the request.
#' @param output A character string specifying the output format. Possible values are 'JSON', 'JSONP', 'XML', 'CSV', 'TXT', and 'PNG'. Default is 'JSON'.
#' @param searchtype An optional character string specifying the search type.
#' @param property An optional character string specifying the property for the request.
#' @param options A list of additional options for the request.
#' @param saveFile A logical value indicating whether to save the output as a file. Default is FALSE.
#' @param saveImage A logical value indicating whether to save the output as an image. Default is FALSE.
#' @param dpi An integer specifying the DPI for image output. Default is 300.
#'
#' @return Depending on the output format, this function returns different types of content:
#'         JSON or JSONP format returns parsed JSON content.
#'         XML format returns an XML object.
#'         CSV format returns a data frame.
#'         TXT format returns a table.
#'         PNG format returns an image object or saves an image file.
#'
#' @examples
#'   get_pug_rest(identifier = "2244", namespace = "cid", domain = "compound", output = "JSON")
#'
#' @importFrom httr GET
#' @importFrom jsonlite prettify
#' @importFrom RJSONIO fromJSON
#' @importFrom xml2 read_xml
#' @importFrom magick image_read
#' @importFrom png readPNG writePNG
#' @importFrom RCurl getURLContent curlEscape
#' @importFrom utils read.csv write.csv read.table write.table
#' @importFrom XML xmlParse
#' @importFrom xml2 write_xml
#' @export

get_pug_rest <- function(identifier = NULL, namespace = 'cid', domain = 'compound',
                         operation = NULL, output = 'JSON', searchtype = NULL, property = NULL, options = NULL,
                         saveFile = FALSE, saveImage = FALSE, dpi = 300) {

  if(!is.null(output)){

    output = toupper(output)

  }else{

    stop("output argument cannot be NULL.")

  }

  # Construct the base URL for PUG REST
  base_url <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug"

  # Build the URL with the given parameters
  url <- paste0(base_url, "/", domain, "/", namespace, "/")

  # Add searchtype to the URL if provided
  if (!is.null(searchtype)) {
    searchtype <- paste0(searchtype, collapse = "/")
    url <- paste0(base_url, "/", domain, "/", searchtype, "/", namespace, "/")
  }

  # Add identifier to the URL if provided
  if (!is.null(identifier)) {

    identifier <- toupper(identifier)

    if(length(identifier)>1){

      identifier = paste0(identifier, collapse = ",")
    }

    url <- paste0(url, identifier, "/")
  }

  # Add operation to the URL if provided
  if (!is.null(operation)) {
    operation <- paste0(operation, collapse = "/")
    url <- paste0(url, operation, "/")
  }

  # Add searchtype to the URL if provided
  if (!is.null(property)) {

    if(length(property)>1){

      property = paste0(property, collapse = ",")
    }
    url <- paste0(url, "property/", property, "/")
  }

  # Finalize URL with output format
  url <- paste0(url, output)

  # Add options to the URL if provided
  if (!is.null(options)) {
    if(namespace == "inchi"){
      options <- paste0("?", paste0("inchi=", curlEscape(unlist(options)), collapse = "&"))
      options <- gsub(" ", "", options)
    }
    else{
      options <- paste0("?", paste0( names(options), unlist(options), collapse = "&"))
      options <- gsub(" ", "", options)
    }
    url <- paste0(url, options)
  }

 if(output == "SDF"){

   if (url.exists(url)) {
     # Write the content to a file in SDF format in the current working directory
     path = paste0(domain, "_", identifier, ".", output)
     download.file(url, path)
     message("  SDF file to save --> '", path, "'", sep = "", "\n")
   } else {
     message("Received no content to write to the SDF file.")
   }
 }else{

  # Make the HTTP GET request and return the response
  response <- GET(URLencode(url))

  if(response$status_code != 400){

  if(output == "CSV"){
    content <- read.csv(response$url)

    if(saveFile){

      write.csv(content, file = paste0(domain, "_", identifier, ".", output), row.names = FALSE)
    }
  }

  if(output == "XML"){
    responseContent <- rawToChar(response$content)
    xml_file <- read_xml(responseContent)
    content <-  xmlParse(xml_file)
  }

  # Check if the response is asking to wait and has a ListKey
  if ('Waiting' %in% names(content) && !is.null(content$Waiting[["ListKey"]])) {
    identifier <- content$Waiting[["ListKey"]]
    namespace <- 'listkey'

    while ('Waiting' %in% names(content) && !is.null(content$Waiting[["ListKey"]])) {
      # Delay before making the next request
      Sys.sleep(2)  # delay for 2 seconds
      # Make the next request
      response <- GET(request(identifier, namespace, domain, operation, output, options))
      responseContent <- rawToChar(response$content)

      if(output == "XML"){
        xml_file <- read_xml(responseContent)
        content <-  xmlParse(xml_file)
      }else{
        content <- fromJSON(responseContent)
      }
    }

  }

  # Handling response based on output format
  if (!is.null(output) && output %in% c('JSON', 'JSONP')) {
    savedContent <- content(response, "text", encoding = "UTF-8")

    if(output == "JSONP"){
      savedContent <- sub('[^;\\{]*', '', savedContent)  # remove function name and opening parenthesis
      savedContent <- sub('\\)$', '', savedContent) # remove closing parenthesis
    }

    content <- prettify(savedContent)
  }

  if(!is.null(output) && output == "PNG"){

    str = readPNG(getURLContent(url))


    if(saveImage){

      writePNG(str, target = paste0(identifier, ".", output), dpi = dpi)
    }

    print(image_read(str))
  }


  if(output == "TXT"){

      content <- read.table(response$url)
  }

  if(saveFile){

    if(output == "TXT"){

      write.table(content, file = paste0(domain, "_", identifier, ".", output), quote = F, row.names = F)

    }

    else if(output == "JSON"){

      write(savedContent, file = paste0(domain, "_", identifier, ".", output))

    }

    else if(output == "XML"){

      write_xml(xml_file, file = paste0(domain, "_", identifier, ".", output))

    }
  }

  if(output == "PNG"){

    message('File has been saved as ', paste0(identifier, ".png"))

  }else{

  invisible(content)

  }
    }else{

    stop(content(response, "text", encoding = "UTF-8"))
  }

 }
}


