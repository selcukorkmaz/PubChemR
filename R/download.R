#' Download Content from PubChem and Save to a File
#'
#' This function sends a request to PubChem to retrieve content in the specified format for a given identifier.
#' It then writes the content to a specified file path.
#'
#' @param outformat A character string specifying the desired output format (e.g., "sdf", "json").
#' @param path A character string specifying the path where the content should be saved.
#' @param identifier A vector of positive integers (e.g. cid, sid, aid) or identifier strings (source, inchikey, formula). In some cases, only a single identifier string (name, smiles, xref; inchi, sdf by POST only).
#' @param namespace Specifies the namespace for the query. For the 'compound' domain, possible values include 'cid', 'name', 'smiles', 'inchi', 'sdf', 'inchikey', 'formula', 'substructure', 'superstructure', 'similarity', 'identity', 'xref', 'listkey', 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure', and 'fastformula'. For other domains, the possible namespaces are domain-specific.
#' @param domain Specifies the domain of the query. Possible values are 'substance', 'compound', 'assay', 'gene', 'protein', 'pathway', 'taxonomy', 'cell', 'sources', 'sourcetable', 'conformers', 'annotations', 'classification', and 'standardize'.
#' @param operation Specifies the operation to be performed on the input records. For the 'compound' domain, possible operations include 'record', 'property', 'synonyms', 'sids', 'cids', 'aids', 'assaysummary', 'classification', 'xrefs', and 'description'. The available operations are domain-specific.
#' @param searchtype Specifies the type of search to be performed. For structure searches, possible values are combinations of 'substructure', 'superstructure', 'similarity', 'identity' with 'smiles', 'inchi', 'sdf', 'cid'. For fast searches, possible values are combinations of 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure' with 'smiles', 'smarts', 'inchi', 'sdf', 'cid', or 'fastformula'.
#' @param overwrite A logical value indicating whether to overwrite the file if it already exists. Default is FALSE.
#' @param ... Additional arguments.
#'
#' @return No return value. The function writes the content to the specified file path and prints a message indicating the save location.
#'
#'
#' @export
download <- function(outformat, path, identifier, namespace = 'cid', domain = 'compound', operation = NULL,
                     searchtype = NULL, overwrite = FALSE, ...) {
  # Use the get function to retrieve the content
  response_content <- get(identifier, namespace, domain, operation, outformat, searchtype, ...)

  # Check if the file already exists and if overwrite is FALSE
  if (!overwrite && file.exists(path)) {
    stop(paste(path, "already exists. Use 'overwrite=TRUE' to overwrite it."))
  }

  # Write the content to the specified path
  writeBin(charToRaw(response_content), path)
  message(paste("The file has been saved to", paste0(getwd(),"/",path)))
}

