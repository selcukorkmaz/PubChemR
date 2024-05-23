#' @importFrom dplyr bind_cols bind_rows full_join
#' @importFrom tidyr as_tibble
#'
#' @export
extract.PubChemInstance <- function(object, .slot = NULL, .to.data.frame = TRUE, ...){

  dots <- list(...)

  if (!object$success){
    return(stop("'object' encountered an error. Nothing to return. \n See error details in 'object'."))
  }

  # Gather all the elements from selected slot. ----
  if ("PC_Compounds" %in% class(object)){
    slotContents <- object$result[[1]][[1]][[.slot]]
  }

  if (is.null(slotContents)){
    return(NULL)
  }

  # Convert into data.frame if possible. ----
  if (.to.data.frame){
    successDF <- try({
      if (.slot == "props" & ("PC_Compounds" %in% class(object))){
        slotContents <- lapply(slotContents, function(x){
          as.data.frame(as.matrix(bind_cols(x)))
        })

        res <- slotContents[[1]]
        for (i in 2:length(slotContents)){
          res <- suppressMessages(full_join(res, slotContents[[i]])) %>%
            as_tibble(.)
        }
      } else {
        res <- bind_cols(slotContents)
      }

      TRUE
    })

    if (!inherits(successDF, "try-error")){
      return(res)
    }
  }

  return(slotContents)
}


#' @export
extract.PubChemInstanceList <- function(object, .which = NULL, .slot = NULL,
                                        .to.data.frame = TRUE, .combine.all = FALSE, ...){
  if (is.null(.which)){
    idx <- 1
  } else {
    if (!(.which %in% request_args(object, "identifier"))){
      stop("Unknown instance identifier. Run 'request_args(object, \"identifier\")' to see all the requested instance identifiers.")
    }
    idx <- which(request_args(object, "identifier") == .which)
  }

  dots <- list(...)
  args <- c(list(object = object$result[[idx]], .slot = .slot,
                 .to.data.frame = .to.data.frame,
                 .combine.all = .combine.all), dots)

  do.call("extract", args)
}

#' @export
extract <- function(object, ...){
  UseMethod("extract")
}



