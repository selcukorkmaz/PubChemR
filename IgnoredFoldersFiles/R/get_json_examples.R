# Examples ----
comp <- get_compounds(identifier = c("2244", "1234"), namespace = "cid")
comp

asp <- instance(comp, "2244")
instance(comp, "dncr")  # Results with error.
retrieve(comp, .slot = "id", .which = 2244)
retrieve(asp, "id")

ass <- get_assays(identifier = c(1234, 7815, 3642, 6214), namespace = "aid")
ass

ass1234 <- instance(ass, 1234)
ass1234

retrieve(ass, .slot = "aid", .to.data.frame = TRUE, .combine.all = FALSE)
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
(res2 <- get_compounds(identifier = "aspirin", namespace = "name"))
res1 <- get_pug_rest(identifier = "aspirin", namespace = "name", domain = "compound", operation = "props")
(aids <- get_aids(identifier = c("aspirin", "caffein", "dncr", "ibuprofen"), namespace = "name"))
(sids <- get_sids(identifier = c("aspirin", "caffein", "dncr", "ibuprofen"), namespace = "name"))
(syns <- get_synonyms(identifier = c("aspirin", "caffein", "dncr", "ibuprofen"), namespace = "name"))

CIDs(cids)
AIDs(aids)
SIDs(sids)
synonyms(syns)

AIDs(get_aids(identifier = "CC(=O)OC1=CC=CC=C1C(=O)O", namespace = "smiles"))


# Getter için genel bir fonksiyon -----
comp <- get_compounds(identifier = c("1234", "3452", "dncr"), namespace = "cid")
comp1234 <- instance(comp, "1234")
comp1234

retrieve(comp1234, .slot = "props", .to.data.frame = TRUE)
retrieve(comp, .slot = "props", .which = "1234", .to.data.frame = TRUE, .combine.all = TRUE)


props <- get_properties(
  properties = c("MolecularWeight", "MolecularFormula", "HBondDonorCount", "HBondAcceptorCount", "InChIKey", "InChI"),
  identifier = c(1:5),
  namespace = "cid",
  propertyMatch = list(
    .ignore.case = TRUE,
    type = "contain"
  )
)

result <- get_pug_rest(identifier = "1,2,3,4,5", namespace = "cid", domain = "compound",
                       property = c("MolecularWeight", "MolecularFormula", "HBondDonorCount", "HBondAcceptorCount", "InChIKey", "InChI"),
                       output = "JSON")

retrieve(props, .which = c("aspirin", "ibuprofen"), .to.data.frame = TRUE, .combine.all = FALSE)
retrieve(props, .which = c("aspirin"), .to.data.frame = TRUE)
retrieve(props, .which = "aspirin", .to.data.frame = FALSE)
retrieve(props, .which = "dncr", .to.data.frame = FALSE)
retrieve(props, .to.data.frame = TRUE, .combine.all = TRUE)
retrieve(props, .to.data.frame = FALSE, .combine.all = TRUE)


# get_substances ----
subs <- get_substances(identifier = c("aspirin", "ibuprofen"), namespace = "name")
instance(subs, "aspirin")

retrieve(instance(subs, "aspirin"), .slot = "comment", .to.data.frame = TRUE, .verbose = TRUE)
retrieve(instance(subs, "aspirin"), .slot = "comment", .to.data.frame = FALSE)
retrieve(instance(subs, "aspirin"), .slot = "comment", .to.data.frame = TRUE, .verbose = TRUE)
retrieve(instance(subs, "aspirin"), .slot = "sid", .to.data.frame = TRUE, .idx = 132)  # Substance 132
retrieve(instance(subs, "aspirin"), .slot = "comment", .to.data.frame = TRUE, .idx = 132)  # There is no comment slot for Substance 132
retrieve(instance(subs, "aspirin"), .slot = "comment", .to.data.frame = TRUE, .idx = 38)
retrieve(instance(subs, "aspirin"), .slot = "xref", .to.data.frame = TRUE, .idx = 19, .verbose = TRUE)
retrieve(instance(subs, "aspirin"), .slot = "compound", .to.data.frame = FALSE, .idx = 132)  # Compound slot is complex and returned as a list.

retrieve(subs, .which = "aspirin", "sid")
retrieve(subs, .which = "ibuprofen", .slot = "comment", .idx = 2)  # comment info for substance 2 in ibuprofen list.
retrieve(subs, .which = "ibuprofen", .slot = "sid", .idx = 1, .combine.all = TRUE)  # combine sid info for substance 1 for aspirin and ibuprofen.

subs <- get_substances(identifier = "10000", namespace = "sid")

# get_pug_view ----
pview <- get_pug_view(identifier = "2244", annotation = "linkout", domain = "compound")
pview <- get_pug_view(identifier = "2244", annotation = "data", domain = "compound")
pview2 <- get_pug_view(identifier = "2244", annotation = "linkout", domain = "compoundsad")

pview <- get_pug_view(identifier = c("1234", "2244"), annotation = "data", domain = "compound")
pview2 <- get_pug_view(identifier = "2244", annotation = "data", domain = "compoundasd")

# PugViewSectionList class
sect <- retrieve(pview, .slot = "Section", .to.data.frame = FALSE)
class(sect)
sectionList(sect)
sectionList(sect, .pattern = c("chemical", "safety"), .match_type = "contain")
sectionList(sect, .pattern = "dncr", .match_type = "start")
sectionList(sect, .pattern = 2, .match_type = "match")

# PugViewSection class
sect2 <- section(sect, .id = "S4", .verbose = FALSE)
section(sect, .id = "S4", .verbose = FALSE)
section(sect, .id = "S4", .verbose = TRUE)
retrieve(sect2, .slot = "Description")
sect2

sectionList(sect2)
sectionList(sect2, .pattern = "properties", .match_type = "contain")
section(sect2, .id = "S4", .verbose = FALSE)
retrieve(section(sect2, .id = "S3", .verbose = FALSE), .slot = "Description")
retrieve(section(sect2, .id = "S3", .verbose = FALSE), .slot = "URL")
sectionList(section(sect2, .id = "S1", .verbose = FALSE))

# How to use ".verbose" in Pug View Request.
# Print section and subsection details at once using nested functions.
pview <- get_pug_view(identifier = "2244", annotation = "data", domain = "compound")
section(section(section(pview, "S13", .verbose = TRUE), "S1", .verbose = TRUE), "S5", .verbose = TRUE)



result1 <- get_pug_rest(identifier = "10000", namespace = "sid", domain = "substance", operation = "synonyms")
result2 <- get_pug_rest(identifier = as.character(1:5), namespace = "cid", domain = "compound", property = c("MolecularFormula","MolecularWeight","CanonicalSMILES"), output = "CSV", save = TRUE)
