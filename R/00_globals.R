#' PubChem API Base URL
#'
#' @description The base URL for the PubChem API.

api_base <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug"

#' Compound ID Types
#'
#' @description List of compound ID types.

CompoundIdType <- list(
  DEPOSITED = 0,
  STANDARDIZED = 1,
  COMPONENT = 2,
  NEUTRALIZED = 3,
  MIXTURE = 4,
  TAUTOMER = 5,
  IONIZED = 6,
  UNKNOWN = 255
)

#' Bond Types
#'
#' @description List of bond types.

BondType <- list(
  SINGLE = 1,
  DOUBLE = 2,
  TRIPLE = 3,
  QUADRUPLE = 4,
  DATIVE = 5,
  COMPLEX = 6,
  IONIC = 7,
  UNKNOWN = 255
)

#' Coordinate Types
#'
#' @description List of coordinate types.

CoordinateType <- list(
  TWO_D = 1,
  THREE_D = 2,
  SUBMITTED = 3,
  EXPERIMENTAL = 4,
  COMPUTED = 5,
  STANDARDIZED = 6,
  AUGMENTED = 7,
  ALIGNED = 8,
  COMPACT = 9,
  UNITS_ANGSTROMS = 10,
  UNITS_NANOMETERS = 11,
  UNITS_PIXEL = 12,
  UNITS_POINTS = 13,
  UNITS_STDBONDS = 14,
  UNITS_UNKNOWN = 255
)

#' Project Categories
#'
#' @description List of project categories.

ProjectCategory <- list(
  MLSCN = 1,
  MPLCN = 2,
  MLSCN_AP = 3,
  MPLCN_AP = 4,
  JOURNAL_ARTICLE = 5,
  ASSAY_VENDOR = 6,
  LITERATURE_EXTRACTED = 7,
  LITERATURE_AUTHOR = 8,
  LITERATURE_PUBLISHER = 9,
  RNAIGI = 10,
  OTHER = 255
)

#' Elements
#'
#' @description Vector of chemical elements.

ELEMENTS <- c(
  'H', 'He', 'Li', 'Be', 'B', 'C', 'N', 'O', 'F', 'Ne',
  'Na', 'Mg', 'Al', 'Si', 'P', 'S', 'Cl', 'Ar', 'K', 'Ca',
  'Sc', 'Ti', 'V', 'Cr', 'Mn', 'Fe', 'Co', 'Ni', 'Cu', 'Zn',
  'Ga', 'Ge', 'As', 'Se', 'Br', 'Kr', 'Rb', 'Sr', 'Y', 'Zr',
  'Nb', 'Mo', 'Tc', 'Ru', 'Rh', 'Pd', 'Ag', 'Cd', 'In', 'Sn',
  'Sb', 'Te', 'I', 'Xe', 'Cs', 'Ba', 'La', 'Ce', 'Pr', 'Nd',
  'Pm', 'Sm', 'Eu', 'Gd', 'Tb', 'Dy', 'Ho', 'Er', 'Tm', 'Yb',
  'Lu', 'Hf', 'Ta', 'W', 'Re', 'Os', 'Ir', 'Pt', 'Au', 'Hg',
  'Tl', 'Pb', 'Bi', 'Po', 'At', 'Rn', 'Fr', 'Ra', 'Ac', 'Th',
  'Pa', 'U', 'Np', 'Pu', 'Am', 'Cm', 'Bk', 'Cf', 'Es', 'Fm',
  'Md', 'No', 'Lr', 'Rf', 'Db', 'Sg', 'Bh', 'Hs', 'Mt', 'Ds',
  'Rg', 'Cp', 'ut', 'uq', 'up', 'uh', 'us', 'uo'
)

# Set the names of the vector elements based on their atomic numbers
names(ELEMENTS) <- 1:118

#' Property Map
#'
#' @description Map of properties to their respective names.

property_map <- list(
  molecular_formula = 'MolecularFormula',
  molecular_weight = 'MolecularWeight',
  canonical_smiles = 'CanonicalSMILES',
  isomeric_smiles = 'IsomericSMILES',
  inchi = 'InChI',
  inchikey = 'InChIKey',
  iupac_name = 'IUPACName',
  xlogp = 'XLogP',
  exact_mass = 'ExactMass',
  monoisotopic_mass = 'MonoisotopicMass',
  tpsa = 'TPSA',
  complexity = 'Complexity',
  charge = 'Charge',
  h_bond_donor_count = 'HBondDonorCount',
  h_bond_acceptor_count = 'HBondAcceptorCount',
  rotatable_bond_count = 'RotatableBondCount',
  heavy_atom_count = 'HeavyAtomCount',
  isotope_atom_count = 'IsotopeAtomCount',
  atom_stereo_count = 'AtomStereoCount',
  defined_atom_stereo_count = 'DefinedAtomStereoCount',
  undefined_atom_stereo_count = 'UndefinedAtomStereoCount',
  bond_stereo_count = 'BondStereoCount',
  defined_bond_stereo_count = 'DefinedBondStereoCount',
  undefined_bond_stereo_count = 'UndefinedBondStereoCount',
  covalent_unit_count = 'CovalentUnitCount',
  volume_3d = 'Volume3D',
  conformer_rmsd_3d = 'ConformerModelRMSD3D',
  conformer_model_rmsd_3d = 'ConformerModelRMSD3D',
  x_steric_quadrupole_3d = 'XStericQuadrupole3D',
  y_steric_quadrupole_3d = 'YStericQuadrupole3D',
  z_steric_quadrupole_3d = 'ZStericQuadrupole3D',
  feature_count_3d = 'FeatureCount3D',
  feature_acceptor_count_3d = 'FeatureAcceptorCount3D',
  feature_donor_count_3d = 'FeatureDonorCount3D',
  feature_anion_count_3d = 'FeatureAnionCount3D',
  feature_cation_count_3d = 'FeatureCationCount3D',
  feature_ring_count_3d = 'FeatureRingCount3D',
  feature_hydrophobe_count_3d = 'FeatureHydrophobeCount3D',
  effective_rotor_count_3d = 'EffectiveRotorCount3D',
  conformer_count_3d = 'ConformerCount3D'
)
