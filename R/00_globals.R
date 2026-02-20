# This script includes objects and helper functions.

#' Import testthat Package
#' @noRd
NULL

# The base URL for the PubChem API.
api_base <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug"

# List of compound ID types.
CompoundIdType <- list(
  DEPOSITED = 0,
  STANDARDIZED = 1,
  COMPONENT = 2,
  NEUTRALIZED = 3,
  MIXTURE = 4,
  TAUTOMER = 5,
  IONIZED = 6,
  UNKNOWN = 255
)

# List of bond types.
BondType <- list(
  SINGLE = 1,
  DOUBLE = 2,
  TRIPLE = 3,
  QUADRUPLE = 4,
  DATIVE = 5,
  COMPLEX = 6,
  IONIC = 7,
  UNKNOWN = 255
)

# List of coordinate types.
CoordinateType <- list(
  TWO_D = 1,
  THREE_D = 2,
  SUBMITTED = 3,
  EXPERIMENTAL = 4,
  COMPUTED = 5,
  STANDARDIZED = 6,
  AUGMENTED = 7,
  ALIGNED = 8,
  COMPACT = 9,
  UNITS_ANGSTROMS = 10,
  UNITS_NANOMETERS = 11,
  UNITS_PIXEL = 12,
  UNITS_POINTS = 13,
  UNITS_STDBONDS = 14,
  UNITS_UNKNOWN = 255
)

# List of project categories.
ProjectCategory <- list(
  MLSCN = 1,
  MPLCN = 2,
  MLSCN_AP = 3,
  MPLCN_AP = 4,
  JOURNAL_ARTICLE = 5,
  ASSAY_VENDOR = 6,
  LITERATURE_EXTRACTED = 7,
  LITERATURE_AUTHOR = 8,
  LITERATURE_PUBLISHER = 9,
  RNAIGI = 10,
  OTHER = 255
)

# Vector of chemical elements.
ELEMENTS <- c(
  'H', 'He', 'Li', 'Be', 'B', 'C', 'N', 'O', 'F', 'Ne',
  'Na', 'Mg', 'Al', 'Si', 'P', 'S', 'Cl', 'Ar', 'K', 'Ca',
  'Sc', 'Ti', 'V', 'Cr', 'Mn', 'Fe', 'Co', 'Ni', 'Cu', 'Zn',
  'Ga', 'Ge', 'As', 'Se', 'Br', 'Kr', 'Rb', 'Sr', 'Y', 'Zr',
  'Nb', 'Mo', 'Tc', 'Ru', 'Rh', 'Pd', 'Ag', 'Cd', 'In', 'Sn',
  'Sb', 'Te', 'I', 'Xe', 'Cs', 'Ba', 'La', 'Ce', 'Pr', 'Nd',
  'Pm', 'Sm', 'Eu', 'Gd', 'Tb', 'Dy', 'Ho', 'Er', 'Tm', 'Yb',
  'Lu', 'Hf', 'Ta', 'W', 'Re', 'Os', 'Ir', 'Pt', 'Au', 'Hg',
  'Tl', 'Pb', 'Bi', 'Po', 'At', 'Rn', 'Fr', 'Ra', 'Ac', 'Th',
  'Pa', 'U', 'Np', 'Pu', 'Am', 'Cm', 'Bk', 'Cf', 'Es', 'Fm',
  'Md', 'No', 'Lr', 'Rf', 'Db', 'Sg', 'Bh', 'Hs', 'Mt', 'Ds',
  'Rg', 'Cp', 'ut', 'uq', 'up', 'uh', 'us', 'uo'
)

# Set the names of the vector elements based on their atomic numbers
names(ELEMENTS) <- 1:118


#' @param x A character vector of compound properties. The \link{property_map} function will search for each property provided here within the available properties. The search can be customized using the \code{type} argument. This argument is ignored if \code{type = "all"}.
#' @param type Defines how to search within the available properties. The default is "match". See Notes for details.
#' @param .ignore.case A logical value. If TRUE, the pattern match ignores case letters. This argument is ignored if \code{type = "all"}. The default is TRUE.
#' @param ... Other arguments. Currently, these have no effect on the function's return.
#'
#' @note
#' \subsection{Property Map:}{
#'   \code{property_map()} is not used to request properties directly from the PubChem database. This function is intended to list the available compound properties that can be requested from PubChem. It has flexible options to search properties from the available property list of the PubChem database. The output of \link{property_map} is used as the \code{property} input in the \link{get_properties} function. This function may be practically used to request specific properties across a range of compounds. See examples for usage.
#' }
#'
#' @importFrom tidyr ends_with starts_with contains
#' @rdname get_properties
#' @order 2
#'
#' @examples
#' \donttest{
#' #### EXAMPLES FOR property_map() ####
#' # List all available properties:
#' property_map(type = "all")
#'
#' # Exact match:
#' property_map("InChI", type = "match")
#' property_map("InChi", type = "match",
#'   .ignore.case = TRUE) # Returns no match. Ignores '.ignore.case'
#'
#' # Match at the start/end:
#' property_map("molecular", type = "start", .ignore.case = TRUE)
#' property_map("mass", type = "end", .ignore.case = TRUE)
#'
#' # Partial match with multiple search patterns:
#' property_map(c("molecular", "mass", "inchi"),
#'   type = "contain", .ignore.case = TRUE)
#'}
#' @export
property_map <- function(x, type = c("match", "contain", "start", "end", "all"), .ignore.case = TRUE, ...){
  properties_list <- c(
    'MolecularFormula', 'MolecularWeight', 'CanonicalSMILES', 'IsomericSMILES', 'InChI', 'InChIKey', 'IUPACName', 'XLogP',
    'ExactMass', 'MonoisotopicMass', 'TPSA', 'Complexity', 'Charge', 'HBondDonorCount', 'HBondAcceptorCount', 'RotatableBondCount',
    'HeavyAtomCount', 'IsotopeAtomCount', 'AtomStereoCount', 'DefinedAtomStereoCount', 'UndefinedAtomStereoCount', 'BondStereoCount',
    'DefinedBondStereoCount', 'UndefinedBondStereoCount', 'CovalentUnitCount', 'Volume3D', 'ConformerModelRMSD3D', 'ConformerModelRMSD3D',
    'XStericQuadrupole3D', 'YStericQuadrupole3D', 'ZStericQuadrupole3D', 'FeatureCount3D', 'FeatureAcceptorCount3D', 'FeatureDonorCount3D',
    'FeatureAnionCount3D', 'FeatureCationCount3D', 'FeatureRingCount3D', 'FeatureHydrophobeCount3D', 'EffectiveRotorCount3D', 'ConformerCount3D'
  )

  type = match.arg(type)

  if (type == "match"){
    .ignore.case <- FALSE
  }

  if (type != "all"){
    res <- sapply(x, function(xx){
      idx <- if (type == "start"){
        starts_with(match = xx, vars = properties_list, ignore.case = .ignore.case)
      } else if (type == "end"){
        ends_with(match = xx, vars = properties_list, ignore.case = .ignore.case)
      } else if (type == "match"){
        if (.ignore.case){
          x <- tolower(xx)
          which(xx == tolower(properties_list))
        } else {
          which(xx == properties_list)
        }
      } else {
        contains(match = xx, vars = properties_list, ignore.case = .ignore.case)
      }

      if (length(idx) == 0){
        return(NULL)
      }

      return(idx)
    }, simplify = FALSE)

    res <- unlist(res)
  } else {
    res <- 1:length(properties_list)
  }

  if (length(res) == 0){
    return(NULL)
  }

  return(properties_list[res])
}

#' @importFrom dplyr case_when
namespace_text <- function(x){
  txt <- case_when(
    .default = "DomainSpecific",
    x == "name" ~ "Name",
    x == "aid" ~ "AID",
    x == "sid" ~ "SID",
    x == "cid" ~ "CID",
    x == "smiles" ~ "SMILES",
    x == "inchi" ~ "INCHI",
    x == "inchikey" ~ "INCHI_Key",
    x == "sdf" ~ "SDF",
    x == "formula" ~ "Formula",
    x == "substructure" ~ "Substructure",
    x == "superstructure" ~ "Superstructure",
    x == "similarity" ~ "Similarity",
    x == "identity" ~ "Identity",
    x == "xref" ~ "CrossReference",
    x == "listkey" ~ "ListKey",
    x == "fastidentity" ~ "FastIdentity",
    x == "fastsimilarity_2d" ~ "2D_FastSimilarity",
    x == "fastsimilarity_3d" ~ "3D_FastSimilarity",
    x == "fastsubstructure" ~ "Fast_Substructure",
    x == "fastsuperstructure" ~ "Fast_Superstructure",
    x == "fastformula" ~ "FastFormula"
  )

  return(txt)
}

#' @importFrom dplyr case_when
domain_text <- function(x){
  txt <- case_when(
    .default = paste0("DomainSpecific (", x, ")"),
    x == "substance" ~ "Substance",
    x == "compound" ~ "Compound",
    x == "assay" ~ "Assay",
    x == "gene" ~ "Gene",
    x == "protein" ~ "Protein",
    x == "taxonomy" ~ "Taxonomy",
    x == "cell" ~ "Cell",
    x == "sources" ~ "Sources",
    x == "sourcetable" ~ "SourceTable",
    x == "conformers" ~ "Conformers",
    x == "annotations" ~ "Annotations",
    x == "classification" ~ "Classification",
    x == "standardize" ~ "Standardize"
  )

  return(txt)
}

primaryClass <- function(x){
  class(x)[1]
}

find_last_layer <- function(x, ...) {
  # Base case: if the current list has multiple elements, return it
  if (length(x) > 1) {
    return(x)
  }

  # Recursive case: go deeper into the single element
  if (is.list(x[[1]])) {
    return(find_last_layer(x[[1]]))
  }

  # If not a list or single element list, return NULL
  return(x)
}


printSlotDetails <- function(x, ...){
  dots <- list(...)
  instanceNames <- names(x)

  for (item in instanceNames){
    itemClass <- class(x[[item]])[1]
    itemNames <- names(x[[item]])

    if (!is.null(itemNames) & length(itemNames) > 4){
      itemNames <- c(itemNames[1:4], "...")
      named_unnamed <- "named"
    }

    itemNamesText <- paste0(itemNames, collapse = ", ")
    named_unnamed <- ifelse(is.null(itemNames), "unnamed", "named")

    if (item == "Section"){
      if (length(dots$pugViewSection) == 0){
        dots$pugViewSection <- FALSE
      }
      if (dots$pugViewSection){
        sectionHeadings <- unlist(lapply(x[[item]], "[[", "TOCHeading"))
        itemNamesText <- if (length(sectionHeadings) >= 2){
          paste0(paste0(sectionHeadings[1:2], collapse = ", "), ", ...", " and ", length(sectionHeadings) - 2, " more.", sep = "")
        } else {
          paste0(sectionHeadings, collapse = ", ")
        }
      }
    }

    cat("  - ", item, " (", length(x[[item]]), ")", ": ", "[<", named_unnamed, " ", itemClass, ">] ",
        itemNamesText, sep = "", "\n")
  }
}

printSectionDetails <- function(x, ...){
  itemNames <- names(x)

  cat("\n")
  cat(" Details on Pug View Section (", x[["TOCHeading"]], ")", sep = "", "\n\n")

  for (item in itemNames){
    itemContent <- find_last_layer(x[[item]])

    if (item != "Section"){
      if (length(itemContent) == 1){
        cat(" > ", item, ": ", itemContent, sep = "", "\n\n")
      } else {
        cat(" > ", item, ": ", "[<", class(x[[item]]), ">]; include nested elements (", length(itemContent), ").", sep = "", "\n\n")
      }
    } else {
      cat(" > ", item, ": ", "[<a ", class(x[[item]]), ">]; includes nested subsections (", length(itemContent), "). ", sep = "")
      cat("Run 'sectionList()' to see available subsections and 'section()' to go through available subsections. See ?section and ?sectionList for help.", sep = "", "\n\n")
    }
  }
}

# Calculate the size of a given file or object
#' @importFrom utils object.size
#' @importFrom dplyr case_when
calculateObjectSize <- function(f = NULL, digits = 2, ...){
  tmp <- as.numeric(file.size(f))

  unit <- case_when(
    .default = "Bytes",
    (tmp >= 1024 & tmp < 1024 ** 2) ~ "KB",
    (tmp >= 1024 ** 2 & tmp < 1024 ** 3) ~ "MB",
    (tmp >= 1024 ** 3 & tmp < 1024 ** 4) ~ "GB",
    tmp >= 1024 ** 4 ~ "PB",
  )

  size <- case_when(
    .default = tmp,
    unit == "KB" ~ tmp / 1024,
    unit == "MB" ~ tmp / (1024 ** 2),
    unit == "GB" ~ tmp / (1024 ** 3),
    unit == "PB" ~ tmp / (1024 ** 4)
  )

  if (is.na(size)){
    size <- 0
  }
  return(list(size = round(size, digits), unit = unit))
}

# Functions used globally in package tests (testthat) ----
# allSuccess <- function(object){
#   all(unlist(lapply(object$result, "[[", "success")))
# }
#
# testRequest <- function(object, ...){
#   test_that(paste0("pulling via '", request_args(object, "namespace"), "' is succesfull"), {
#     expect_true(allSuccess(object))
#   })
#
#   test_that("prints output to the R Console", {
#     expect_output(print(object))
#   })
# }
