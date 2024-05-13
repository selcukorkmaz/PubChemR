.compound_name <- function(x, ...){
  UseMethod(".compound_name")
}

.compound_name.PubChemInstance <- function(x, ...){
  return(x[["Compound_Name"]])
}




meta_data <- function(x, ...){
  UseMethod("meta_data")
}

meta_data.PubChemInstance <- function(x, ...){
  x[[1]][[1]][["meta_data"]]
}
