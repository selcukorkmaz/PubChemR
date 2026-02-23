## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(PubChemR)
library(dplyr)
library(tibble)

## ----eval=FALSE---------------------------------------------------------------
# cid_map <- pc_identifier_map(
#   identifier = c("aspirin", "ibuprofen", "caffeine"),
#   namespace = "name",
#   to = "cids",
#   domain = "compound",
#   cache = TRUE
# )
# 
# as_tibble(cid_map)

## ----eval=FALSE---------------------------------------------------------------
# aid_map <- pc_identifier_map(
#   identifier = c(2244, 3672, 2519),
#   namespace = "cid",
#   to = "aids",
#   domain = "compound",
#   cache = TRUE
# )
# 
# aid_tbl <- as_tibble(aid_map)
# aid_tbl

## ----eval=FALSE---------------------------------------------------------------
# # Select a subset of AIDs for demonstration
# sel_aids <- unique(na.omit(unlist(aid_tbl$AID)))
# sel_aids <- head(sel_aids, 10)
# 
# assay_res <- pc_assay(
#   identifier = sel_aids,
#   namespace = "aid",
#   operation = "summary",
#   cache = TRUE
# )
# 
# as_tibble(assay_res)

