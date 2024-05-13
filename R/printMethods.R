

asp <- get_json(identifier = "aspirin", namespace = "name")
caf <- get_json(identifier = "caffein", namespace = "name")
asp_caf <- get_json(identifier = c(2222, 2223), namespace = "cid")

class(asp_caf) <- class(caf) <- class(asp) <- "PubChemInstance"

# Define a print method for "PubChemInstanceList". this list includes the PubChem data for multiple instances.
printt.PubChemInstanceList <- function(x, ...) {
  cat(" An object of '", class(x), "' class", sep = "", "\n\n")

  cat(" Number of compounds ")
}

# Define a print method for "PubChemInstance"
printt.PubChemInstance <- function(x, ...) {

  if (class(x) != "PubChemInstanceList"){
    printt(x)
  }

  cat(" An object of '", class(x), "' class", sep = "", "\n\n")

  cat(" Number of Compound(s): ", length(x[[1]]), sep = "")
}

# Define a generic print method
printt <- function(x, ...) {
  UseMethod("printt")
}

printt(asp)

