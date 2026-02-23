# PubChemInstanceList ----
#' @title \code{PubChemInstanceList} and \code{PubChemInstance} Classes
#'
#' @description The \code{PubChemInstanceList} object is a superclass returned by a request for compound(s) from
#' the PubChem Database, such as the output from \link{get_compounds}, \link{get_assays}, etc.
#'
#' @section Slots:
#' \describe{
#'   \item{\code{results}:}{A list containing elements of each of the requested compounds, assays, substances, etc.}
#'   \item{\code{request_args}:}{A list containing the input arguments of a PubChem request.}
#'   \item{\code{success}:}{A logical value indicating whether the request was successfully completed (TRUE) or not (FALSE).}
#'   \item{\code{error}:}{A list detailing any errors encountered during the request, if applicable.}
#' }
#'
#' @docType class
#' @name PubChemR-classes
#' @rdname PubChemR-classes
#' @order 1
#'
#' @note
#' There is no constructor function for the \code{PubChemInstanceList} or \code{PubChemInstance} classes. These objects are
#' constructed within related functions and returned as the output of PubChem requests.
NULL

# PubChemInstance ----
#' @description
#' The \code{PubChemInstance} object is another superclass for a PubChem instance, such as an assay, compound, substance, etc.
#' These instances are nested within the \code{results} slot of a \code{PubChemInstanceList} object. Similar to \code{PubChemInstanceList},
#' the \code{PubChemInstance} also contains the same slots as described below. For more details, see \link{instance}.
#'
#' @name PubChemR-classes
#' @rdname PubChemR-classes
#' @order 2
#'
#' @note
#' There are several subclasses defined under the \code{PubChemInstanceList} and \code{PubChemInstance} superclasses. The PubChem API
#' returns request results in a list; however, each request may have a different list structure and/or items within the returned list.
#' Therefore, we have defined subclasses to make generic functions compatible with any PubChem request, such as assays, instances,
#' substances, etc. These subclasses may include \code{PC_Compounds}, \code{PC_Substance}, \code{PC_Properties}, \code{PubChemInstance_AIDs},
#' \code{PubChemInstance_SIDs}, \code{PubChemInstance_CIDs}, \code{PubChemInstance_Synonyms}, and \code{PubChemInstance_Substances}.
#'
#' Most of the defined subclasses have similar slots as described above. However, some classes may have additional slots not described here.
#' Please refer to the contents of the returned object for more details.
NULL


# PugViewInstance ----
#' @title Classes for Pug View Request
#'
#' @description The Pug View API of PubChem database returns more detailed information about a PubChem request, such as assays, compounds,
#' substances, etc. A super-class \code{PugViewInstance} is defined, which is returned from \link{get_pug_view} function. This class has
#' slots detailed below.
#'
#' @section Slots:
#' \describe{
#'   \item{\code{results}:}{A list containing elements of each of the requested compounds, assays, substances, etc.}
#'   \item{\code{request_args}:}{A list containing the input arguments of a PubChem request.}
#'   \item{\code{success}:}{A logical value indicating whether the request was successfully completed (TRUE) or not (FALSE).}
#'   \item{\code{error}:}{A list detailing any errors encountered during the request, if applicable.}
#' }
#'
#' @docType class
#' @name PugView-classes
#' @rdname PugView-classes
#'
#' @note
#' Pug View API returns many section about the requested instance, which includes detailed information from PubChem database.
#' There may be many nested sections, where each contains details about different features of the instance requested. These sections
#' can be listed via \link{sectionList} function.
#'
#' Other classes, called \code{PugViewSectionList} and \code{PugViewSection}, are defined to control the outputs of available sections
#' and sub-sections returned from \link{get_pug_view}. See related functions for details.
#'
#' @seealso \link{section}, \link{sectionList}
NULL

