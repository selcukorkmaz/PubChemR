get_pug_rest <- function(identifier = NULL, namespace = 'cid', domain = 'compound',
                         operation = NULL, output = 'JSON', searchtype = NULL, property = NULL, options = NULL,
                         saveFile = FALSE, saveImage = FALSE) {

  if(!is.null(output)){

    output = toupper(output)

  }else{

    stop("output argument cannot be NULL.")

  }

  # Construct the base URL for PUG REST
  base_url <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug"

  if(!is.null(options)){

    options <- paste0("?", paste0( names(options), "=", curlEscape(unlist(options)), collapse = "&"))
    # options <- gsub(" ","", options, fixed = TRUE)
  }

  # Build the URL with the given parameters
  url <- paste0(base_url, "/", domain, "/", namespace, "/")

  # Add identifier to the URL if provided
  if (!is.null(identifier)) {
    if(length(identifier)>1){

      identifier = paste0(identifier, collapse = ",")
    }

    url <- paste0(url, identifier, "/")
  }

  # Add operation to the URL if provided
  if (!is.null(operation)) {
    url <- paste0(url, operation, "/")
  }

  # Add searchtype to the URL if provided
  if (!is.null(searchtype)) {
    url <- paste0(url, searchtype, "/")
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
    url <- paste0(url, options)
  }

  # Make the HTTP GET request and return the response
  response <- GET(url)

  # Handling response based on output format
  if (!is.null(output) && output %in% c('JSON', 'JSONP')) {
    content <- fromJSON(content(response, "text", encoding = "UTF-8"))
  }

  else if(!is.null(output) && output == "SVG" && domain != "key"){

    str <- charToRaw(content(response, as = "text", encoding = "UTF-8"))
    content <- image_read(str)

    if(savePNG){

      rsvg_png(svg = str, file = paste0(identifier, ".png"))
    }

  }

  else if(output == "png"){

    str <- readPNG(getURLContent(url))
    content <- image_read(str)

    if(saveImage){

      writePNG(str, target = paste0(identifier, ".png"))
    }
  }

  else {
    content <- content(response, "text", encoding = "UTF-8")
  }

  if(saveFile){

    write.table(content, file = paste0(domain, "_", identifier, ".", output))
  }

  return(content)
}

identifier = c(1,2,3,4,5); namespace = "cid"; domain = 'compound'
operation = NULL; output = 'CSV'; searchtype = NULL; property = c("MolecularFormula","MolecularWeight","CanonicalSMILES");
options = NULL; saveFile = TRUE; saveImage = FALSE

get_pug_rest(identifier, namespace, domain,
                         operation, output, searchtype, property,
                         options, saveFile, saveImage)

