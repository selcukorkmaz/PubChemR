.compound_name <- function(x, ...){
  UseMethod(".compound_name")
}

.compound_name.PubChemInstance <- function(x, ...){
  return(x[["Compound_Name"]])
}



