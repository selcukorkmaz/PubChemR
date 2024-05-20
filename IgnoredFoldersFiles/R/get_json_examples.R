# 1. get_json examples -----
asp_name <- get_json(identifier ="aspirin", namespace = "name")
asp_cid <- get_json(identifier ="2244", namespace = "cid")
asp_smiles <- get_json(identifier ="CC(=O)OC1=CC=CC=C1C(=O)O", namespace = "smiles")
asp_inchikey <- get_json(identifier ="BSYNRYMUTXBXSQ-UHFFFAOYSA-N", namespace = "inchikey")
asp_formula <- get_json(identifier ="CH3COOC6H4COOH", namespace = "formula")

names(asp_name$PC_Compounds[[1]])
names(asp_cid$PC_Compounds[[1]])
names(asp_smiles$PC_Compounds[[1]])
names(asp_inchikey$PC_Compounds[[1]])
names(asp_formula$PC_Compounds[[1]])


caf_name <- get_json(identifier ="caffein", namespace = "name")
caf_cid <- get_json(identifier ="2519", namespace = "cid")
caf_smiles <- get_json(identifier ="CN1C=NC2=C1C(=O)N(C(=O)N2C)C", namespace = "smiles")
caf_inchikey <- get_json(identifier ="RYYVLZVUVIJVGH-UHFFFAOYSA-N", namespace = "inchikey")
caf_formula <- get_json(identifier ="	C8H10N4O2", namespace = "formula")


names(caf_name$PC_Compounds[[1]])
names(caf_cid$PC_Compounds[[1]])
names(caf_smiles$PC_Compounds[[1]])
names(caf_inchikey$PC_Compounds[[1]])
names(caf_formula$PC_Compounds[[1]])


asp_caf_name <- get_json(identifier = c("aspirin","caffein"), namespace = "name")
names(asp_caf_name$PC_Compounds[[1]]) ## for aspirin
names(asp_caf_name$PC_Compounds[[2]]) ## for caffein

asp_caf_cid <- get_json(identifier = c("2244","2519"), namespace = "cid")
names(asp_caf_cid$PC_Compounds[[1]]) ## for aspirin
names(asp_caf_cid$PC_Compounds[[2]]) ## for caffein

asp_caf_smiles <- get_json(identifier = c("CC(=O)OC1=CC=CC=C1C(=O)O", "CN1C=NC2=C1C(=O)N(C(=O)N2C)C"), namespace = "smiles")
names(asp_caf_smiles$PC_Compounds[[1]]) ## for aspirin
names(asp_caf_smiles$PC_Compounds[[2]]) ## for caffein

asp_caf_inchikey <- get_json(identifier = c("BSYNRYMUTXBXSQ-UHFFFAOYSA-N", "RYYVLZVUVIJVGH-UHFFFAOYSA-N"), namespace = "inchikey")
names(asp_caf_inchikey$PC_Compounds[[1]]) ## for aspirin
names(asp_caf_inchikey$PC_Compounds[[2]]) ## for caffein

asp_caf_formula <- get_json(identifier = c("CH3COOC6H4COOH", "C8H10N4O2"), namespace = "formula")
names(asp_caf_formula$PC_Compounds[[1]]) ## for aspirin
names(asp_caf_formula$PC_Compounds[[2]]) ## for caffein



# 2. Class and Getter examples ----
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
