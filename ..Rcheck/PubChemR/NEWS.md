# PubChemR 2.1.9 (development)

- Refactored internal request/result collection logic for all identifier-based wrappers
  to enforce consistent `result` / `success` / `error` contracts.
- Added a new `pc_*` API layer including:
  `pc_request()`, `pc_response()`, `pc_compound()`, `pc_substance()`,
  `pc_assay()`, `pc_property()`, `pc_identifier_map()`, `pc_batch()`,
  `pc_submit()`, `pc_poll()`, and `pc_collect()`.
- Added centralized request configuration via `pc_config()` and deterministic
  memory/disk cache controls via `pc_cache_clear()`.
- Added typed result classes (`PubChemResult`, `PubChemBatchResult`, `PubChemError`)
  with `as_tibble()` adapters and print methods.
- Added Phase 3 capabilities:
  cache diagnostics (`pc_cache_info()`), offline cache-only replay mode in
  `pc_request(offline = TRUE)`, benchmarking helper (`pc_benchmark()`),
  and modeling-focused feature extraction (`pc_feature_table()`).
- Added extended Phase 3 analytics/integration APIs:
  execution profiles (`pc_profile()`), similarity workflow helper
  (`pc_similarity_search()`), assay activity matrix assembly
  (`pc_activity_matrix()`), cross-domain joins (`pc_cross_domain_join()`),
  model matrix adapter (`pc_model_matrix()`), optional conversion bridges
  (`pc_to_rcdk()`, `pc_to_chemminer()`), and lifecycle policy helper
  (`pc_lifecycle_policy()`).
- Removed unsafe fault parsing in JSON retrieval and replaced with structured error objects.
- Fixed malformed query-string option construction in URL builder.
- Fixed argument forwarding bug in asynchronous listkey polling path.
- Updated `get_assays()` to respect the user-provided `operation` argument.
- Added `get_biological_test_results()` to directly extract
  `CONTENTS -> Biological Test Results` from PubChem PUG View records.
- Added initial `testthat` suite and multiple workflow vignettes.
- Updated README examples, license text, and citation metadata for consistency.

# PubChemR 2.0.0 (Major Release)

-   R version 3.6.0 or higher is now explicitly required.

-   New superclasses have been created to store data retrieved from the PubChem Database, such as `PubChemInstance`, `PubViewInstance`, and `PugRestInstance`.

-   New subclasses have been created, including `PC_Assay`, `PC_Compound`, and `PC_Properties`.

-   A generic getter function, `retrieve()`, has been added. You can retrieve specific information, slots, or elements from a PubChem object using the `retrieve()` function.

-   The functions `get_pug_view()` and `get_pug_rest()` now return outputs in the specified superclasses, which are more organized and have their own print methods for the R console.

-   The `get_pug_view()` function can now return highly detailed data from the PubChem Database. This major release includes handy getter functions, primarily developed for the `PubViewInstance` object. You can retrieve data using the `retrieve()` function or extract and print section information from a given object using the `section()` and `sectionList()` functions.

-   This major release features two main functions, `get_pug_view()` and `get_pug_rest()`, which can be used for various purposes. These functions mainly fetch information on single compounds, elements, assays, etc.

-   Several functions, such as `get_compounds()`, `get_sids()`, `get_assays()`, etc., have been included to request multiple elements, compounds, assays, etc., in a loop. These functions are simplified and targeted variants of the `get_pug_view()` and `get_pug_rest()` functions.
