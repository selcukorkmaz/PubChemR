# Examples ----
comp <- get_compounds(identifier = c("1234", "3452", "dncr"), namespace = "cid")
comp

asp <-instance(comp, "1234")
instance(comp, "dncr")  # Results with error.

asp <- instance(comp, "aspirin")
atoms(asp)
count(asp)
charge(asp)

ass <- get_assays(identifier = c(1234, 7815, 3642, 2856, 6214), namespace = "aid")
ass

ass1234 <- instance(ass, 1234)
ass1234

retrieve(ass, .which = "1234", .slot = "aid", .to.data.frame = TRUE, .combine.all = TRUE)
retrieve(ass1234, .slot = "aid", .verbose = FALSE, .to.data.frame = TRUE)
retrieve(ass1234, .slot = "aid", .verbose = TRUE, .to.data.frame = FALSE)

aid_source(ass1234, .verbose = TRUE)
retrieve(ass1234, .slot = "aid_source", .verbose = FALSE, .to.data.frame = TRUE)
retrieve(ass1234, .slot = "aid_source", .verbose = TRUE, .to.data.frame = FALSE)

name(ass1234, .verbose = TRUE)
retrieve(ass1234, .slot = "name", .verbose = TRUE, .to.data.frame = FALSE)
retrieve(ass1234, .slot = "name", .verbose = FALSE, .to.data.frame = TRUE)

description(ass1234, .verbose = TRUE)
retrieve(ass1234, .slot = "description", .verbose = TRUE, .to.data.frame = FALSE)
retrieve(ass1234, .slot = "description", .verbose = FALSE, .to.data.frame = TRUE)

protocol(ass1234, .verbose = TRUE)
retrieve(ass1234, .slot = "protocol", .verbose = TRUE, .to.data.frame = FALSE)
retrieve(ass1234, .slot = "protocol", .verbose = FALSE, .to.data.frame = TRUE)

comment(ass1234, .verbose = TRUE)
retrieve(ass1234, .slot = "comment", .verbose = TRUE, .to.data.frame = FALSE)
retrieve(ass1234, .slot = "comment", .verbose = FALSE, .to.data.frame = TRUE)

xref(ass1234, .verbose = TRUE)
retrieve(ass1234, .slot = "xref", .verbose = TRUE, .to.data.frame = FALSE)
retrieve(ass1234, .slot = "xref", .verbose = FALSE, .to.data.frame = TRUE)

results(ass1234, .verbose = TRUE)
retrieve(ass1234, .slot = "results", .verbose = TRUE, .to.data.frame = FALSE)
retrieve(ass1234, .slot = "results", .verbose = FALSE, .to.data.frame = TRUE)

revision(ass1234, .verbose = TRUE)
retrieve(ass1234, .slot = "revision", .verbose = TRUE, .to.data.frame = FALSE)
retrieve(ass1234, .slot = "revision", .verbose = FALSE, .to.data.frame = TRUE)

activity_outcome_method(ass1234, .verbose = TRUE)
retrieve(ass1234, .slot = "activity_outcome_method", .verbose = TRUE, .to.data.frame = FALSE)
retrieve(ass1234, .slot = "activity_outcome_method", .verbose = FALSE, .to.data.frame = TRUE)

project_category(ass1234, .verbose = TRUE)
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
  identifier = c("aspirin", "ibuprofen"),
  namespace = "name",
  propertyMatch = list(
    .ignore.case = TRUE,
    type = "contain"
  )
)

instanceProperties(props, .which = "aspirin", .to.data.frame = TRUE)
instanceProperties(props, .which = "aspirin", .to.data.frame = FALSE)
instanceProperties(props, .which = "dncr", .to.data.frame = FALSE)
instanceProperties(props, .which = "selcuk", .to.data.frame = FALSE)
instanceProperties(props, .to.data.frame = TRUE, .combine.all = TRUE)
instanceProperties(props, .to.data.frame = FALSE, .combine.all = TRUE)

