## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(PubChemR)
library(tibble)
library(dplyr)

## -----------------------------------------------------------------------------
cfg <- pc_config(
  rate_limit = 5,
  retries = 3,
  timeout = 60
)
cfg

## -----------------------------------------------------------------------------
pc_cache_clear()

## ----eval=FALSE---------------------------------------------------------------
# first <- pc_property(
#   identifier = c(2244, 3672, 2519),
#   properties = c("MolecularWeight", "XLogP", "TPSA"),
#   namespace = "cid",
#   cache = TRUE
# )
# 
# second <- pc_property(
#   identifier = c(2244, 3672, 2519),
#   properties = c("MolecularWeight", "XLogP", "TPSA"),
#   namespace = "cid",
#   cache = TRUE
# )
# 
# # second$from_cache is expected to be TRUE
# as_tibble(second)

## ----eval=FALSE---------------------------------------------------------------
# ids <- c("aspirin", "ibuprofen", "caffeine", "acetaminophen", "naproxen")
# 
# batch <- pc_batch(
#   ids = ids,
#   fn = function(chunk_ids) {
#     pc_identifier_map(
#       identifier = chunk_ids,
#       namespace = "name",
#       to = "cids",
#       domain = "compound",
#       cache = TRUE
#     )
#   },
#   chunk_size = 2
# )
# 
# batch
# as_tibble(batch)

