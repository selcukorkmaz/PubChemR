# BURADA KALDIM --------
assays <- list()
assays_df = list()
data <- list()

for (i in 1:length(identifier)) {
  # Retrieve the JSON data
  results <- get_json(identifier[i], namespace, 'assay', 'description', searchtype, options)

  # Check if not failed.
  if (results$success) {
    # Create a list of assays (here, you might want to define what an 'Assay' contains)
    if (!is.null(results$result$PC_AssayContainer)) {
      assays[[i]] <- results$result$PC_AssayContainer
    } else {
      assays[[i]] <- "No assay"
    }
  }

  if (to_dataframe){
    # Separate elements into vectors and lists
    vectors <- assays[[i]][[1]][[1]]$descr %>% keep(~ is.atomic(.x) && !is.list(.x))
    lists <- assays[[i]][[1]][[1]]$descr %>% keep(~ is.list(.x))
    lists_result = list()

    # Convert multi-element vectors into single strings
    vectors <- vectors %>%
      map(~ if (length(.x) > 1) paste(.x, collapse = " ") else .x)

    # Convert vectors to dataframe
    vector_df <- as_tibble(vectors)

    for (j in 1:length(lists)){
      if (length(lists[[j]]) == 1){
        lists_result[[j]] = as_tibble(lists[[j]][[1]])

      } else {
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

        if (!is.data.frame(lists_result[[j]]) && lists_result[[j]] == "Error"){

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

if (to_dataframe){
  assay_res = assays_df
} else {
  assay_res = assays
}

names(assay_res) <- paste0("'", identifier, "'")
results <- assay_res

return(results)
