#' Retrieve Assays from PubChem
#'
#' This function sends a request to PubChem to retrieve assay data based on the specified parameters.
#' It returns a list of assays corresponding to the provided identifiers.
#'
#' @param identifier A vector of positive integers (e.g. cid, sid, aid) or identifier strings (source, inchikey, formula). In some cases, only a single identifier string (name, smiles, xref; inchi, sdf by POST only).
#' @param namespace Specifies the namespace for the query. For the 'compound' domain, possible values include 'cid', 'name', 'smiles', 'inchi', 'sdf', 'inchikey', 'formula', 'substructure', 'superstructure', 'similarity', 'identity', 'xref', 'listkey', 'fastidentity', 'fastsimilarity_2d', 'fastsimilarity_3d', 'fastsubstructure', 'fastsuperstructure', and 'fastformula'. For other domains, the possible namespaces are domain-specific.
#' @param operation The operation to be performed (default: NULL).
#' @param searchtype The type of search to be performed (default: NULL).
#' @param options Additional parameters. Currently has no effect on the results.
#' @param to_dataframe Convert list to dataframe.
#'
#' @return A named list where each element corresponds to an assay retrieved from PubChem.
#'         The names of the list elements are based on the provided identifiers.
#'         If no assay is found for a given identifier, the corresponding list element will contain the string "No assay".
#'
#' @importFrom purrr keep map
#' @importFrom tibble as_tibble enframe
#' @importFrom tidyr unnest_wider
#' @export
#'
#' @examples
#' get_assays(
#'   identifier = c(1234, 7815),
#'   namespace = 'aid',
#'   to_dataframe = TRUE
#' )
get_assays <- function(identifier, namespace = 'aid', operation = NULL, searchtype = NULL, options = NULL, to_dataframe = FALSE) {

  assays <- list()
  assays_df = list()
  data <- list()

  for (i in 1:length(identifier)) {
    # Retrieve the JSON data
    results <- get_json(identifier[i], namespace, 'assay', 'description', searchtype, options)

      # Check if results are not empty
      if (!is.null(results)) {
        # Create a list of assays (here, you might want to define what an 'Assay' contains)
        if (!is.null(results$PC_AssayContainer)) {
          assays[[i]] <- results$PC_AssayContainer
        } else {
          assays[[i]] <- "No assay"
        }


      }

    if(to_dataframe){

      # Separate elements into vectors and lists
      vectors <- assays[[i]][[1]][[1]]$descr %>% keep(~ is.atomic(.x) && !is.list(.x))
      lists <- assays[[i]][[1]][[1]]$descr %>% keep(~ is.list(.x))
      lists_result = list()

      # Convert multi-element vectors into single strings
      vectors <- vectors %>%
        map(~ if (length(.x) > 1) paste(.x, collapse = " ") else .x)

      # Convert vectors to dataframe
      vector_df <- as_tibble(vectors)


      for(j in 1:length(lists)){

        if(length(lists[[j]]) == 1){

          lists_result[[j]] = as_tibble(lists[[j]][[1]])

        }else{

          lists_result[[j]] <- tryCatch({
            # Perform the transformations
            lists[[j]] %>%
              enframe(name = NULL, value = names(lists)[j]) %>%
              unnest_wider(col = names(lists)[j]) %>%
              unnest_wider(col = names(lists)[j])
          }, error = function(e) {
            # Handle the error
            return("Error")  # Return NULL in case of error
          })

          if(!is.data.frame(lists_result[[j]]) && lists_result[[j]] == "Error"){

            lists_result[[j]] <- tryCatch({
              # Perform the transformations
              lists[[j]] %>%
                enframe(name = NULL, value = names(lists)[j]) %>%
                unnest_wider(col = names(lists)[j])
            }, error = function(e) {
              # Handle the error
              return("Error")  # Return NULL in case of error
            })
          }


        }

      }

      names(lists_result) = names(lists)
      lists_result$descr = vector_df
      lists_result <- lists_result[c("descr", names(lists))]

      assays_df[[i]] = lists_result

    }


  }

    if(to_dataframe){assay_res = assays_df} else{assay_res = assays}
    names(assay_res) <- paste0("'", identifier, "'")
    results <- assay_res


    return(results)
  }
