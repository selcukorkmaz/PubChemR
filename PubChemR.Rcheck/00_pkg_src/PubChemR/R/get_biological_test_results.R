#' Retrieve "Biological Test Results" Section from PubChem Contents
#'
#' This helper fetches a PUG View record and extracts section(s) with heading
#' `"Biological Test Results"` (or a custom heading) from the PubChem
#' `CONTENTS` structure.
#'
#' @param identifier A single identifier for a PUG View request.
#' @param domain A domain value accepted by \code{\link{get_pug_view}}.
#' @param heading Section heading to search for. Defaults to
#'   \code{"Biological Test Results"}.
#' @param .match_type Matching strategy for \code{heading}; one of
#'   \code{"match"} (exact, case-insensitive) or \code{"contain"}.
#' @param .all Logical. If \code{TRUE}, returns all matching sections as a
#'   \code{PugViewSectionList} object. If \code{FALSE}, returns the first
#'   matching section as a \code{PugViewSection} object.
#' @param .verbose Logical. If \code{TRUE}, prints the returned section object
#'   and returns it invisibly.
#' @param ... Additional arguments passed to \code{\link{get_pug_view}}.
#'
#' @return A \code{PugViewSection} object (default) or \code{PugViewSectionList}
#'   when \code{.all = TRUE}. If no section is found, a failed
#'   \code{PugViewSection} object is returned with error details.
#'
#' @examples
#' \donttest{
#' bio <- get_biological_test_results(identifier = "2244", domain = "compound")
#' bio
#'
#' # Return all matching sections (if multiple are present)
#' bio_all <- get_biological_test_results(
#'   identifier = "2244",
#'   domain = "compound",
#'   .all = TRUE
#' )
#' bio_all
#' }
#'
#' @export
get_biological_test_results <- function(identifier,
                                        domain = "compound",
                                        heading = "Biological Test Results",
                                        .match_type = c("match", "contain"),
                                        .all = FALSE,
                                        .verbose = FALSE,
                                        ...) {
  .match_type <- match.arg(.match_type)

  if (is.null(identifier) || length(identifier) == 0) {
    stop("'identifier' cannot be NULL or empty.")
  }

  if (length(identifier) > 1) {
    warning(
      "Only one identifier is allowed for get_biological_test_results(). ",
      "Using the first element: ", identifier[[1]]
    )
    identifier <- identifier[[1]]
  }

  if (!is.character(heading) || length(heading) != 1 || !nzchar(heading)) {
    stop("'heading' must be a non-empty character string.")
  }

  pview <- get_pug_view(
    annotation = "data",
    identifier = identifier,
    domain = domain,
    output = "JSON",
    ...
  )

  req <- list(
    annotation = "data",
    identifier = as.character(identifier),
    domain = domain,
    heading = heading
  )

  if (!isTRUE(pview$success)) {
    msg <- if (!is.null(pview$error$Message)) pview$error$Message else "Unknown PUG View error."
    err <- .pv_make_failed_section(
      message = paste0("PUG View retrieval failed: ", msg),
      request_args = req
    )
    if (isTRUE(.verbose)) {
      print(err)
      return(invisible(err))
    }
    return(err)
  }

  section_container <- retrieve(pview, .slot = "Section")
  if (is.null(section_container) || !isTRUE(section_container$success)) {
    err <- .pv_make_failed_section(
      message = "No section data available in PUG View response.",
      request_args = req
    )
    if (isTRUE(.verbose)) {
      print(err)
      return(invisible(err))
    }
    return(err)
  }

  matches <- .pv_collect_heading_sections(
    sections = section_container$result,
    heading = heading,
    .match_type = .match_type
  )

  if (length(matches) == 0) {
    err <- .pv_make_failed_section(
      message = paste0("No section found for heading: '", heading, "'."),
      request_args = req,
      record_information = section_container$recordInformation
    )
    if (isTRUE(.verbose)) {
      print(err)
      return(invisible(err))
    }
    return(err)
  }

  out <- if (isTRUE(.all)) {
    structure(
      list(
        result = matches,
        recordInformation = section_container$recordInformation,
        request_args = req,
        success = TRUE,
        error = NULL
      ),
      class = "PugViewSectionList"
    )
  } else {
    structure(
      list(
        result = matches[[1]],
        recordInformation = section_container$recordInformation,
        request_args = req,
        success = TRUE,
        error = NULL
      ),
      class = "PugViewSection"
    )
  }

  if (isTRUE(.verbose)) {
    print(out)
    invisible(out)
  } else {
    out
  }
}

.pv_make_failed_section <- function(message,
                                    request_args = NULL,
                                    record_information = NULL,
                                    code = "SectionNotFound") {
  structure(
    list(
      result = list(),
      recordInformation = record_information,
      request_args = request_args,
      success = FALSE,
      error = list(
        Message = as.character(message),
        Code = as.character(code)
      )
    ),
    class = "PugViewSection"
  )
}

.pv_heading_match <- function(x, heading, .match_type = c("match", "contain")) {
  .match_type <- match.arg(.match_type)
  if (is.null(x) || !nzchar(as.character(x))) {
    return(FALSE)
  }

  x <- as.character(x)
  heading <- as.character(heading)

  if (.match_type == "match") {
    tolower(x) == tolower(heading)
  } else {
    grepl(tolower(heading), tolower(x), fixed = TRUE)
  }
}

.pv_collect_heading_sections <- function(sections, heading, .match_type = c("match", "contain")) {
  .match_type <- match.arg(.match_type)
  out <- list()

  walk <- function(node) {
    if (!is.list(node)) {
      return(invisible(NULL))
    }

    # Section node
    if (!is.null(node$TOCHeading)) {
      if (.pv_heading_match(node$TOCHeading, heading = heading, .match_type = .match_type)) {
        out[[length(out) + 1]] <<- node
      }

      if (!is.null(node$Section) && is.list(node$Section)) {
        for (child in node$Section) {
          walk(child)
        }
      }
      return(invisible(NULL))
    }

    # Section container
    for (child in node) {
      if (is.list(child) && (!is.null(child$TOCHeading) || !is.null(child$Section))) {
        walk(child)
      }
    }
    invisible(NULL)
  }

  walk(sections)
  out
}
