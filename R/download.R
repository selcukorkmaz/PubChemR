#' Download Content from PubChem and Save to a File
#'
#' This function sends a request to PubChem to retrieve content in the specified format for a given identifier.
#' It then writes the content to a specified file path.
#'
#' @param filename a character string specifying the file name to be saved. If not specified, a default file name "file" is used.
#' @param outformat A character string specifying the desired output format (e.g., "sdf", "json").
#' @param path A character string specifying the path where the content should be saved.
#' @param identifier A vector of positive integers (e.g. cid, sid, aid) or identifier strings (source, inchikey, formula). In some cases, only a single
#' identifier string (name, smiles, xref; inchi, sdf by POST only).
#' @param namespace Specifies the namespace for the query. For the 'compound' domain, possible values include 'cid', 'name', 'smiles', 'inchi', 'sdf', 'inchikey',
#' 'formula', 'substructure', 'superstructure', 'similarity', 'identity', 'xref', 'listkey', 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d',
#' 'fastsubstructure', 'fastsuperstructure', and 'fastformula'. For other domains, the possible namespaces are domain-specific.
#' @param domain Specifies the domain of the query. Possible values are 'substance', 'compound', 'assay', 'gene', 'protein', 'pathway', 'taxonomy',
#' 'cell', 'sources', 'sourcetable', 'conformers', 'annotations', 'classification', and 'standardize'.
#' @param operation Specifies the operation to be performed on the input records. For the 'compound' domain, possible operations include 'record',
#' 'property', 'synonyms', 'sids', 'cids', 'aids', 'assaysummary', 'classification', 'xrefs', and 'description'. The available operations are domain-specific.
#' @param searchtype Specifies the type of search to be performed. For structure searches, possible values are combinations of 'substructure',
#' 'superstructure', 'similarity', 'identity' with 'smiles', 'inchi', 'sdf', 'cid'. For fast searches, possible values are combinations of 'fastidentity',
#' 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure' with 'smiles', 'smarts', 'inchi', 'sdf', 'cid', or 'fastformula'.
#' @param overwrite A logical value indicating whether to overwrite the file if it already exists. Default is FALSE.
#' @param options Additional arguments.
#'
#' @return No return value. The function writes the content to the specified file path and prints a message indicating the save location.
#'
#' @export
#'
#' @examples
#' # Download JSON file for the compound "aspirin" into "Aspirin.JSON"
#' # A folder named "Compound" will be created under current directory"
#' download(
#'   filename = "Aspirin",
#'   outformat = "json",
#'   path = "./Compound",
#'   identifier = "aspirin",
#'   namespace = "name",
#'   domain = "compound",
#'   overwrite = TRUE
#' )
#'
#' # Remove downloaded files and folders.
#' file.remove("./Compound/Aspirin.json")
#' file.remove("./Compound/")
download <- function(filename = NULL, outformat, path, identifier, namespace = 'cid', domain = 'compound', operation = NULL,
                     searchtype = NULL, overwrite = FALSE, options = NULL) {

  if (is.null(path)){
    stop("path can not be NULL")
  } else {
    if (!file.exists(path)){
      dir.create(path, recursive = TRUE)
    }
  }

  if (is.null(filename)){
    filename <- identifier
  }

  # Use the get function to retrieve the content
  response_content <- get_pubchem(identifier, namespace, domain, operation, outformat, searchtype, options)

  # Full path, including the name and file extension
  full_path <- paste0(path, "/", filename, ".", outformat)

  # Check if the file already exists and if overwrite is FALSE
  if (!overwrite && file.exists(full_path)) {
    stop(paste(full_path, "already exists. Use 'overwrite=TRUE' to overwrite it."))
  }

  # Write the content to the specified path
  writeBin(charToRaw(response_content), full_path)
  message(paste0("The file has been saved to ", "'", full_path, "'"))

  # Return the full path invisibly
  return(invisible(full_path))
}

