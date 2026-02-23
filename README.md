
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![](https://www.r-pkg.org/badges/version/PubChemR)](https://cran.r-project.org/package=PubChemR)
[![](https://www.r-pkg.org/badges/last-release/PubChemR?color=orange)](https://cran.r-project.org/package=PubChemR)
[![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Codecov test
coverage](https://codecov.io/gh/selcukorkmaz/PubChemR/branch/devel/graph/badge.svg)](https://app.codecov.io/gh/selcukorkmaz/PubChemR?branch=devel)
[![BUILD &
CHECK](https://github.com/selcukorkmaz/PubChemR/actions/workflows/R-BUILD-CHECK.yml/badge.svg)](https://github.com/selcukorkmaz/PubChemR/actions/workflows/R-BUILD-CHECK.yml)
<!-- [![](https://cranlogs.r-pkg.org/badges/PubChemR)](https://cran.r-project.org/package=PubChemR) -->
<!-- [![License: GPL (>= 2)](https://img.shields.io/badge/license-GPL%20(%3E=%202)-blue.svg)](https://cran.r-project.org/web/licenses/GPL-2) -->
<!-- badges: end -->

# PubChemR: An Interface to the PubChem Collection of Chemical Data PubChemR <img src="docs/figures/logo.png" align="right" height="32" />

`PubChemR` is an R package that provides an interface to the ‘PubChem’
database via the [PUG
REST](https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest) and [PUG
View](https://pubchem.ncbi.nlm.nih.gov/docs/pug-view) services. This
package allows users to programmatically access chemical and biological
data from ‘PubChem’, including compounds, substances, assays, and
various other data types. Functions are available to retrieve data in
different formats, perform searches, and access detailed annotations.

## Installation

You can install the development version of `PubChemR` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("selcukorkmaz/PubChemR")
```

**Usage**

The PubChemR package provides functions to query PubChem’s database
using their RESTful web service. Below are some examples of how to use
the package:

Getting Compound Information Retrieve compound information by CID:

library(PubChemR)

**Retrieving Compound Information**

Get compound information by CID

``` r
compound_info <- get_compounds(c(2244, 305))
```

**Searching for Compounds**

Search for compounds by name:

``` r
compounds_by_name <- get_cids(c("Aspirin", "Paracetamol"))
```

**Retrieving Assay Information**

Get assay information by AID:

``` r
assay_info <- get_aids(c(2551, 1000))
```

**Downloading Chemical Structures**

Download chemical structures in different formats:

``` r
download(
  filename = "aspirin",
  outformat = "sdf",
  path = tempdir(),
  identifier = 2244,
  namespace = "cid",
  domain = "compound",
  overwrite = TRUE
)
```

**Functions**

The package includes the following main functions:

`get_compounds()`: Retrieve information about compounds. `get_cids()`:
Get compound identifiers for given names or other identifiers.
`get_aids()`: Obtain assay information for given assay identifiers.
`get_sids()`: Get substance identifiers related to compounds.
`get_properties()`: Retrieve specific properties of compounds.
`get_biological_test_results()`: Retrieve the `CONTENTS -> Biological Test Results` section from PUG View records.
`get_json()`: General function to retrieve data in JSON format.
`download()`: Download chemical structures and other data. Each function
is documented with details on its parameters and return values. Use
?function_name in R to access the help page for a specific function.

**Next-Generation API (`pc_*`)**

Phase 2 introduces typed, workflow-oriented functions:

`pc_request()`, `pc_response()`, `pc_compound()`, `pc_substance()`,
`pc_assay()`, `pc_property()`, `pc_identifier_map()`, `pc_batch()`,
`pc_resume_batch()`, `pc_submit()`, `pc_poll()`, `pc_collect()`, `pc_benchmark()`,
`pc_cache_info()`, `pc_feature_table()`, `pc_profile()`,
`pc_similarity_search()`, `pc_activity_matrix()`, `pc_activity_outcome_map()`,
`pc_cross_domain_join()`, `pc_model_matrix()`,
`pc_assay_activity_long()`, `pc_export_model_data()`,
`pc_to_rcdk()`, `pc_to_chemminer()`, and `pc_lifecycle_policy()`.

**Benchmark Harness (10/1k/100k + Threshold Gates)**

Use `pc_benchmark_harness()` to run scale scenarios with explicit pass/fail thresholds and optional report output.

```r
thresholds <- list(
  elapsed_sec = c(`10` = 30, `1000` = 300, `100000` = 3600),
  failed_chunk_ratio = c(`10` = 0, `1000` = 0.01, `100000` = 0.05)
)

probe <- function(ids) {
  pc_request(
    domain = "compound",
    namespace = "cid",
    identifier = 2244,
    operation = "property/MolecularWeight",
    output = "JSON",
    cache = FALSE
  )
}

bench <- pc_benchmark_harness(
  fn = probe,
  ids = rep(2244, 100000),
  scenario_sizes = c(10, 1000, 100000),
  chunk_sizes = 1000,
  thresholds = thresholds,
  report_path = file.path(tempdir(), "pubchemr-benchmark.md"),
  report_format = "markdown"
)

bench$summary
```

CI integration is available in `.github/workflows/live-pubchem-smoke.yml`, which:
1. Runs the live harness nightly.
2. Fails on threshold breaches.
3. Uploads benchmark reports/history artifacts.
4. Re-calibrates rolling thresholds from accumulated live history.

**Output Formats**

The PubChemR package supports various output formats including JSON,
SDF, CSV, PNG, and TXT.

**HTTP Interface**

PubChemR interacts with the PubChem API through HTTP requests, handling
the construction of query URLs and parsing the responses.

**Contributing**

Contributions to PubChemR are welcome! Please refer to the
CONTRIBUTING.md file for guidelines.

**License**

PubChemR is released under the GPL (>= 2) License. See the package metadata for details.

**Contact**

For questions and feedback, please open an issue in the GitHub
repository issue tracker.

**Citation**

If you use PubChemR in your research, please cite it as follows:

Korkmaz S, Yamasan BE, Goksuluk D (2024). *PubChemR: An R Package for
Accessing Chemical Data from PubChem*. The R Journal, 16(3), 150-185.
<https://journal.r-project.org/articles/RJ-2024-031/RJ-2024-031.pdf>.

A BibTeX entry for LaTeX users is:

``` r
@Manual{,
  title = {PubChemR: An R Package for Accessing Chemical Data from PubChem},
  author = {Selcuk Korkmaz and Bilge Eren Yamasan and Dincer Goksuluk},
  year = {2024},
  note = {R Journal, 16(3), 150-185},
  url = {https://journal.r-project.org/articles/RJ-2024-031/RJ-2024-031.pdf},
}
```


