

asp <- get_json(identifier = "aspirin", namespace = "name")
caf <- get_json(identifier = "caffein", namespace = "name")
asp_caf <- get_json(identifier = c(2222, 2223), namespace = "cid")

class(asp_caf) <- class(caf) <- class(asp) <- "PubChemInstance"

# Define a print method for "PubChemInstanceList". this list includes the PubChem data for multiple instances.
print.PubChemInstanceList <- function(x, ...) {
  cat(" An object of '", class(x), "' class", sep = "", "\n\n")
  cat(" Number of compounds: ", length(x[[1]]), "\n")
}

# Define a print method for "PubChemInstance"
print.PubChemInstance <- function(x, ...) {

  cat(" An object of '", class(x), "' class", sep = "", "\n\n")
  cat(" Number of Compound(s): ", length(x[[1]]), sep = "", "\n")

  meta_data <- x[[1]][[1]][["meta_data"]]
  cat("   - ", )
}

# Define a generic print method
print <- function(x, ...) {
  UseMethod("print")
}

print(asp)

