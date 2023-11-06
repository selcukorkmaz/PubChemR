# PubChemR: An Interface to the PubChem Collection of Chemical Data PubChemR <img src="man/figures/logo.png" align="right" height="32" />

`PubChemR` is an R package that provides a user-friendly interface to the PubChem database, which is a rich resource for chemical information. This package allows users to programmatically access chemical data, including compound identifiers (CIDs), substance identifiers (SIDs), assay information (AIDs), and more.

## Installation

You can install the development version of `PubChemR` from [GitHub](https://github.com/) with:

```r
# install.packages("devtools")
devtools::install_github("your_github_username/PubChemR")
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

Your Name (Year). PubChemR: An Interface to the PubChem Collection of Chemical Data. R package version x.x.x. URL: https://github.com/your_github_username/PubChemR
















