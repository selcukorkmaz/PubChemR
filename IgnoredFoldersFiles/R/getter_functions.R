# Getter Functions ----
## PubChemRequest class ----
call_params.PubChemRequest <- function(object){
  return(object[["call_parameters"]])
}

call_params <- function(object, ...){
  UseMethod("call_params")
}

instance.PubChemRequest <- function(object, .which = "aspirin", ....){
  return(object[[1]][[.which]])
}

instance <- function(object, ...){
  UseMethod("instance")
}









## PubChemInstance class ----
atoms.PubChemInstance <- function(object, ...){
  object[["atoms"]]
}

atoms.PubChemRequest <- function(object){
  object <- object[[1]][[1]]
  object[["atoms"]]
}

atoms <- function(object, ...){
  UseMethod("atoms")
}


bonds.PubChemInstance <- function(object, ...){
  object[["atoms"]]
}

bonds <- function(object, ...){
  UseMethod("bonds")
}

coords.PubChemInstance <- function(object, ...){

}

coords <- function(object, ...){
  UseMethod("coords")
}

charge.PubChemInstance <- function(object, ...){

}

charge <- function(object, ...){
  UseMethod("charge")
}

props.PubChemInstance <- function(object, ...){

}

props <- function(object, ...){
  UseMethod("props")
}

count.PubChemInstance <- function(object, ...){

}

count <- function(object, ...){
  UseMethod("count")
}
