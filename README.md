[![](https://www.r-pkg.org/badges/version/PubChemR)](https://cran.r-project.org/package=PubChemR)
[![](https://www.r-pkg.org/badges/last-release/PubChemR?color=orange)](https://cran.r-project.org/package=PubChemR)
[![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![BUILD & CHECK](https://github.com/selcukorkmaz/PubChemR/actions/workflows/R-BUILD-CHECK.yml/badge.svg)](https://github.com/selcukorkmaz/PubChemR/actions/workflows/R-BUILD-CHECK.yml)
<!-- [![](https://cranlogs.r-pkg.org/badges/PubChemR)](https://cran.r-project.org/package=PubChemR) -->
<!-- [![License: GPL (>= 2)](https://img.shields.io/badge/license-GPL%20(%3E=%202)-blue.svg)](https://cran.r-project.org/web/licenses/GPL-2) -->

# PubChemR: An Interface to the PubChem Collection of Chemical Data PubChemR <img src="man/figures/logo.png" align="right" height="32" />

`PubChemR` is an R package that provides an interface to the 'PubChem' database via the [PUG REST](https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest) and 
[PUG View](https://pubchem.ncbi.nlm.nih.gov/docs/pug-view) services. This package allows users to programmatically 
access chemical and biological data from 'PubChem', including compounds, substances, assays, and various other data types. 
Functions are available to retrieve data in different formats, perform searches, and access detailed annotations.

## Installation

You can install the development version of `PubChemR` from [GitHub](https://github.com/) with:

```r
# install.packages("devtools")
devtools::install_github("selcukorkmaz/PubChemR")
```

**Usage**

The PubChemR package provides functions to query PubChem's database using their RESTful web service. Below are some examples of how to use the package:

Getting Compound Information
Retrieve compound information by CID:

library(PubChemR)


**Retrieving Compound Information**

Get compound information by CID

```r
compound_info <- get_compounds(c(2244, 305))
```

**Searching for Compounds**

Search for compounds by name:

```r
compounds_by_name <- get_cids(c("Aspirin", "Paracetamol"))
```

**Retrieving Assay Information**

Get assay information by AID:

```r
assay_info <- get_aids(c(2551, 1000))
```

**Downloading Chemical Structures**

Download chemical structures in different formats:

```r
download("SDF", "aspirin.sdf", 2244)
```

**Functions**

The package includes the following main functions:

`get_compounds()`: Retrieve information about compounds.
`get_cids()`: Get compound identifiers for given names or other identifiers.
`get_aids()`: Obtain assay information for given assay identifiers.
`get_sids()`: Get substance identifiers related to compounds.
`get_properties()`: Retrieve specific properties of compounds.
`get_json()`: General function to retrieve data in JSON format.
`download()`: Download chemical structures and other data.
Each function is documented with details on its parameters and return values. Use ?function_name in R to access the help page for a specific function.

**Output Formats**

The PubChemR package supports various output formats including JSON, SDF, CSV, PNG, and TXT.

**HTTP Interface**

PubChemR interacts with the PubChem API through HTTP requests, handling the construction of query URLs and parsing the responses.

**Contributing**

Contributions to PubChemR are welcome! Please refer to the CONTRIBUTING.md file for guidelines.

**License**

PubChemR is released under the MIT License. See the LICENSE file for more details.

**Contact**

For questions and feedback, please open an issue in the GitHub repository issue tracker.

**Citation**

If you use PubChemR in your research, please cite it as follows:

Korkmaz S, Goksuluk D (2023). _PubChemR: Interface to the 'PubChem' Database for Chemical Data Retrieval_. R package version 0.99-1, <https://CRAN.R-project.org/package=PubChemR>.

A BibTeX entry for LaTeX users is:

```r
@Manual{,
  title = {PubChemR: Interface to the 'PubChem' Database for Chemical Data Retrieval},
  author = {Selcuk Korkmaz and Dincer Goksuluk},
  year = {2023},
  note = {R package version 0.99-1},
  url = {https://CRAN.R-project.org/package=PubChemR},
}
```
















