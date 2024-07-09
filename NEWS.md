# PubChemR 2.0.0 (Major Release)

-   R version 3.6.0 or higher is now explicitly required.

-   New superclasses have been created to store data retrieved from the PubChem Database, such as `PubChemInstance`, `PubViewInstance`, and `PugRestInstance`.

-   New subclasses have been created, including `PC_Assay`, `PC_Compound`, and `PC_Properties`.

-   A generic getter function, `retrieve()`, has been added. You can retrieve specific information, slots, or elements from a PubChem object using the `retrieve()` function.

-   The functions `get_pug_view()` and `get_pug_rest()` now return outputs in the specified superclasses, which are more organized and have their own print methods for the R console.

-   The `get_pug_view()` function can now return highly detailed data from the PubChem Database. This major release includes handy getter functions, primarily developed for the `PubViewInstance` object. You can retrieve data using the `retrieve()` function or extract and print section information from a given object using the `section()` and `sectionList()` functions.

-   This major release features two main functions, `get_pug_view()` and `get_pug_rest()`, which can be used for various purposes. These functions mainly fetch information on single compounds, elements, assays, etc.

-   Several functions, such as `get_compounds()`, `get_sids()`, `get_assays()`, etc., have been included to request multiple elements, compounds, assays, etc., in a loop. These functions are simplified and targeted variants of the `get_pug_view()` and `get_pug_rest()` functions.
