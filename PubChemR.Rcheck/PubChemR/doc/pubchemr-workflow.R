## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(PubChemR)
library(dplyr)

## ----eval=FALSE---------------------------------------------------------------
# ids <- get_cids(c("aspirin", "ibuprofen", "caffeine"), namespace = "name")
# CIDs(ids)

## ----eval=FALSE---------------------------------------------------------------
# props <- get_properties(
#   properties = c("MolecularWeight", "MolecularFormula", "XLogP", "TPSA"),
#   identifier = c("aspirin", "ibuprofen", "caffeine"),
#   namespace = "name"
# )
# 
# prop_tbl <- retrieve(props, .combine.all = TRUE, .to.data.frame = TRUE)
# prop_tbl

## ----eval=FALSE---------------------------------------------------------------
# model_tbl <- prop_tbl %>%
#   mutate(
#     MolecularWeight = as.numeric(MolecularWeight),
#     XLogP = as.numeric(XLogP),
#     TPSA = as.numeric(TPSA)
#   )
# 
# model_tbl

## ----eval=FALSE---------------------------------------------------------------
# mm <- pc_model_matrix(
#   model_tbl,
#   id_cols = c("CID"),
#   na_fill = 0
# )
# 
# mm

