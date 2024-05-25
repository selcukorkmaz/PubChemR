# Examples ----
comp <- get_compounds(identifier = c("1234", "3452"), namespace = "cid")
comp

asp <-instance(comp, "1234")
instance(comp, "dncr")  # Results with error.

asp <- instance(comp, "aspirin")
atoms(asp)
count(asp)
charge(asp)

ass <- get_assays(identifier = c(1234, 7815, 3642, 6214), namespace = "aid")
ass

ass1234 <- instance(ass, 1234)
ass1234

retrieve(ass, .slot = "aid", .to.data.frame = TRUE, .combine.all = TRUE)
retrieve(ass1234, .slot = "aid", .verbose = FALSE, .to.data.frame = TRUE)
retrieve(ass1234, .slot = "aid", .verbose = TRUE, .to.data.frame = FALSE)

retrieve(ass, .which = "1234", .slot = "aid_source")
retrieve(ass, .slot = "aid_source", .combine.all = TRUE, .to.data.frame = FALSE)
retrieve(ass, .slot = "aid_source", .combine.all = TRUE, .to.data.frame = TRUE)
retrieve(ass1234, .slot = "aid_source", .to.data.frame = TRUE)
retrieve(ass1234, .slot = "aid_source", .to.data.frame = FALSE)

retrieve(ass, .slot = "name", .combine.all = TRUE, .to.data.frame = TRUE)
retrieve(ass, .slot = "name", .combine.all = TRUE, .to.data.frame = FALSE)
retrieve(ass1234, .slot = "name")
retrieve(ass1234, .slot = "name", .verbose = TRUE, .to.data.frame = FALSE)
retrieve(ass1234, .slot = "name", .verbose = FALSE, .to.data.frame = TRUE)


retrieve(ass, .slot = "description", .combine.all = TRUE, .to.data.frame = FALSE)
retrieve(ass, .slot = "description", .combine.all = TRUE, .to.data.frame = FALSE)
retrieve(ass1234, .slot = "description", .verbose = TRUE, .to.data.frame = FALSE)
retrieve(ass1234, .slot = "description", .verbose = FALSE, .to.data.frame = TRUE)

retrieve(ass1234, .slot = "protocol", .verbose = TRUE, .to.data.frame = FALSE)
retrieve(ass1234, .slot = "protocol", .verbose = FALSE, .to.data.frame = TRUE)

retrieve(ass1234, .slot = "comment", .verbose = TRUE, .to.data.frame = FALSE)
retrieve(ass1234, .slot = "comment", .verbose = FALSE, .to.data.frame = TRUE)

retrieve(ass1234, .slot = "xref", .verbose = TRUE, .to.data.frame = FALSE)
retrieve(ass1234, .slot = "xref", .verbose = FALSE, .to.data.frame = TRUE)

retrieve(ass1234, .slot = "results", .verbose = TRUE, .to.data.frame = FALSE)
retrieve(ass1234, .slot = "results", .verbose = FALSE, .to.data.frame = TRUE)

retrieve(ass1234, .slot = "revision", .verbose = TRUE, .to.data.frame = FALSE)
retrieve(ass1234, .slot = "revision", .verbose = FALSE, .to.data.frame = TRUE)

retrieve(ass1234, .slot = "activity_outcome_method", .verbose = TRUE, .to.data.frame = FALSE)
retrieve(ass1234, .slot = "activity_outcome_method", .verbose = FALSE, .to.data.frame = TRUE)

retrieve(ass1234, .slot = "project_category", .verbose = TRUE, .to.data.frame = FALSE)
retrieve(ass1234, .slot = "project_category", .verbose = FALSE, .to.data.frame = TRUE)



(cids <- get_cids(identifier = c("aspirin", "caffein", "dncr", "ibuprofen"), namespace = "name"))
(aids <- get_aids(identifier = c("aspirin", "caffein", "dncr", "ibuprofen"), namespace = "name"))
(sids <- get_sids(identifier = c("aspirin", "caffein", "dncr", "ibuprofen"), namespace = "name"))

CIDs(cids)
AIDs(aids)
SIDs(sids)

AIDs(get_aids(identifier = "CC(=O)OC1=CC=CC=C1C(=O)O", namespace = "smiles"))


# Getter için genel bir fonksiyon -----
comp <- get_compounds(identifier = c("1234", "3452", "dncr"), namespace = "cid")
comp1234 <- instance(comp, "1234")
comp1234

retrieve(comp1234, .slot = "props", .to.data.frame = TRUE)
retrieve(comp, .slot = "props", .which = "1234", .to.data.frame = TRUE, .combine.all = TRUE)


props <- get_properties(
  properties = c("smile"),
  identifier = c("aspirin", "ibuprofen", "dncr"),
  namespace = "name",
  propertyMatch = list(
    .ignore.case = TRUE,
    type = "contain"
  )
)

retrieve(props, .which = c("aspirin", "ibuprofen"), .to.data.frame = TRUE, .combine.all = FALSE)
retrieve(props, .which = c("aspirin"), .to.data.frame = TRUE)
retrieve(props, .which = "aspirin", .to.data.frame = FALSE)
retrieve(props, .which = "dncr", .to.data.frame = FALSE)
retrieve(props, .to.data.frame = TRUE, .combine.all = TRUE)
retrieve(props, .to.data.frame = FALSE, .combine.all = TRUE)

