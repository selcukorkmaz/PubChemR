# Examples ----
comp <- get_compounds(identifier = c("aspirin", "caffein", "dncr"), namespace = "name")
comp

instance(comp, "aspirin")
instance(comp, "dncr")  # Results with error.

ass <- get_assays(identifier = c(1234, 7815, "dncr"), namespace = "aid")
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
