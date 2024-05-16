## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = TRUE,
  class.output="scroll-100",
  cache.path = "cached/"
)
library(PubChemR)

## -----------------------------------------------------------------------------
result <- get_pug_view(annotation = "data", identifier = "1234", domain = "compound", output = "JSON")

## -----------------------------------------------------------------------------
result

## -----------------------------------------------------------------------------
result <- get_pug_view(annotation = "index", identifier = "1234", domain = "compound", output = "XML")

## -----------------------------------------------------------------------------
result

## -----------------------------------------------------------------------------
result <- get_pug_view(annotation = "data", identifier = "2244", domain = "compound", output = "JSON", heading = "Experimental Properties")

## -----------------------------------------------------------------------------
result

## -----------------------------------------------------------------------------
result <- get_pug_view(annotation = "data", identifier = "1", domain = "assay", output = "JSON")

## -----------------------------------------------------------------------------
result

## -----------------------------------------------------------------------------
result <- get_pug_view(annotation = "data", identifier = "1", domain = "gene", output = "JSON")

## -----------------------------------------------------------------------------
result

## -----------------------------------------------------------------------------
result <- get_pug_view(annotation = "annotations", identifier = "Viscosity",   domain = "heading", output = "JSON")

