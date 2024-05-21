# Examples ----
comp <- get_compounds(identifier = c("1234", "3452", "dncr"), namespace = "cid")
comp

instance(comp, "aspirin")
instance(comp, "dncr")  # Results with error.

asp <- instance(comp, "aspirin")
atoms(asp)
count(asp)
charge(asp)

ass <- get_assays(identifier = c(1234, 7815, 3642, 2856, 6214), namespace = "aid")
ass

instance(ass, 7815)
aid_source(ass, .which = "7815")
aid_source(ass, .which = "dncr")
aid_source(ass, .which = "2222")

description(ass, .which = "7815")

results(ass, .which = "1234", .to.data.frame = TRUE)
xref(instance(ass, 7815), .verbose = FALSE)
xref(instance(ass, 7815), .verbose = TRUE)

comment(ass, .which = "7815")
comment(ass, .which = "1234")

(cids <- get_cids(identifier = c("aspirin", "caffein", "dncr", "ibuprofen"), namespace = "name"))
(aids <- get_aids(identifier = c("aspirin", "caffein", "dncr", "ibuprofen"), namespace = "name"))
(sids <- get_sids(identifier = c("aspirin", "caffein", "dncr", "ibuprofen"), namespace = "name"))

CIDs(cids)
AIDs(aids)
SIDs(sids)

props <- get_properties(
  properties = c("mass"),
  identifier = c(1234:1350),
  namespace = "cid",
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
