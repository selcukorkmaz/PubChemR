# PubChemInstanceList ----
#' \code{PubChemInstanceList} and \code{PubChemInstance} objects
#'
#' @description \code{PubChemInstanceList} object is the super-class returned by a request of compound(s) from
#' PubChem Database, e.g., the output from \link{get_compounds}, \link{get_assays}, etc.
#'
#' @section Slots:
#' \describe{
#'   \item{\code{results}:}{a list with elements of each of requested compounds, assays, substances, etc.}
#'   \item{\code{request_args}:}{a list with function inputs of a PubChem request.}
#'   \item{\code{success}:}{a logical. Returns TRUE if a request is successfully completed.}
#'   \item{\code{error}:}{a list. If a request encountered an error, details are given within \code{error} slot.}
#' }
#'
#' @docType class
#' @name PubChemInstance-class
#' @rdname PubChemInstance-class
#' @order 1
#'
#' @note
#' There is no constructor function for \code{'PubChemInstanceList'} or \code{'PubChemInstance'} classes. These object are
#' constructed within related functions and returned as output of PubChem requests.
NULL

# PubChemInstance ----
#' @describe
#' \code{PubChemInstance} object is another super-class for a PubChem instance, e.g., assay, compound, substance, etc.
#' These instances are nested within \code{results} slot of \code{PubChemInstanceList} object. Likewise, \code{PubChemInstaneList},
#' \code{PubChemInstance} also has the same slots as described below. For more details, see \link{instance}.
#'
#' @name PubChemInstance-class
#' @rdname PubChemInstance-class
NULL
