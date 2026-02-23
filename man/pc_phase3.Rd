% Manual documentation for Phase 3 analytical and integration APIs
\name{pc_phase3}
\alias{pc_phase3}
\alias{pc_profile}
\alias{pc_similarity_search}
\alias{pc_activity_matrix}
\alias{pc_cross_domain_join}
\alias{pc_model_matrix}
\alias{pc_to_rcdk}
\alias{pc_to_chemminer}
\alias{pc_lifecycle_policy}
\alias{print.PubChemModelMatrix}
\title{Phase 3 Analytical and Integration API}
\description{
High-level workflow helpers for similarity search, assay activity matrix assembly,
cross-domain joins, model-matrix preparation, interoperability bridges, and
execution policy configuration.
}
\usage{
pc_profile(profile = c("default", "cloud", "high_throughput"), ...)

pc_similarity_search(
  identifier,
  namespace = c("smiles", "cid", "inchi", "sdf"),
  domain = "compound",
  to = c("cids", "sids", "aids"),
  searchtype = c("similarity", "fastsimilarity_2d", "fastsimilarity_3d"),
  threshold = 95,
  max_records = NULL,
  options = NULL,
  ...
)

pc_activity_matrix(
  x,
  cid_col = "CID",
  aid_col = "AID",
  outcome_col = "ActivityOutcome",
  value_map = c(Active = 1, Inactive = 0, Inconclusive = NA_real_),
  strict_outcome = FALSE,
  unknown_outcome = NA_real_,
  fill = NA_real_,
  prefix = "AID_",
  aggregate = c("max", "mean", "first"),
  output = c("tibble", "sparse")
)

pc_cross_domain_join(
  compounds,
  substances = NULL,
  assays = NULL,
  targets = NULL,
  by = list(
    compound_substance = "CID",
    compound_assay = "CID",
    assay_target = "AID"
  ),
  join = c("left", "inner", "full")
)

pc_model_matrix(
  x,
  outcome = NULL,
  id_cols = c("CID", "SID", "AID", "Identifier"),
  na_fill = NULL,
  scale = FALSE
)

pc_to_rcdk(x, smiles_col = "CanonicalSMILES", id_col = "CID")

pc_to_chemminer(x, smiles_col = "CanonicalSMILES")

pc_lifecycle_policy()
}
\arguments{
\item{...}{Additional named overrides or request arguments passed to lower-level functions.}

\item{aggregate}{Aggregation strategy used for repeated CID/AID pairs.}

\item{aid_col}{Column containing assay identifiers.}

\item{assays}{Optional assay table for cross-domain joins.}

\item{by}{Named list of join-key definitions for each join edge.}

\item{cid_col}{Column containing compound identifiers.}

\item{compounds}{Base compound table (required).}

\item{domain}{PubChem domain for similarity search.}

\item{fill}{Fill value for missing matrix cells in \code{pc_activity_matrix}.}

\item{strict_outcome}{Logical. If \code{TRUE}, unknown outcome labels raise an error.}

\item{unknown_outcome}{Numeric fallback for unknown labels when \code{strict_outcome = FALSE}.}

\item{id_col}{Identifier column used to name converted molecule objects.}

\item{id_cols}{Identifier columns excluded from predictor matrix construction.}

\item{identifier}{Query identifier(s) for similarity search.}

\item{join}{Join type for \code{pc_cross_domain_join}.}

\item{max_records}{Optional maximum records returned by PubChem.}

\item{na_fill}{Optional numeric value used to fill \code{NA} predictor values.}

\item{namespace}{Namespace for similarity-search identifiers.}

\item{options}{Additional named query options for similarity search.}

\item{outcome}{Optional outcome column name for supervised model datasets.}

\item{outcome_col}{Column containing activity outcome values.}

\item{output}{Output type for \code{pc_activity_matrix}: \code{"tibble"} or \code{"sparse"}.}

\item{prefix}{Prefix applied to assay columns in wide activity output.}

\item{profile}{Named transport profile.}

\item{scale}{Logical; center and scale predictor matrix.}

\item{searchtype}{Similarity search mode.}

\item{smiles_col}{Column containing SMILES strings.}

\item{substances}{Optional substance table for cross-domain joins.}

\item{targets}{Optional target table for cross-domain joins.}

\item{threshold}{Similarity threshold from 0 to 100.}

\item{to}{Target mapping output (`cids`, `sids`, or `aids`).}

\item{value_map}{Named numeric map used when activity outcomes are character values.}

\item{x}{Input table/tibble or a \code{PubChemResult} object.}
}
\value{
\code{pc_profile()} returns active configuration list.
\code{pc_similarity_search()} returns a typed object inheriting from \code{PubChemIdMap}.
\code{pc_activity_matrix()} and \code{pc_cross_domain_join()} return tibbles with class \code{PubChemTable}.
\code{pc_model_matrix()} returns an object of class \code{PubChemModelMatrix}.
\code{pc_to_rcdk()} and \code{pc_to_chemminer()} return objects from their respective ecosystems.
\code{pc_lifecycle_policy()} returns a policy tibble.
}
\examples{
\donttest{
pc_profile("cloud")

sim <- pc_similarity_search(
  identifier = "CC(=O)OC1=CC=CC=C1C(=O)O",
  namespace = "smiles",
  threshold = 90
)

activity_long <- tibble::tibble(
  CID = c(2244, 2244, 3672),
  AID = c(100, 101, 100),
  ActivityOutcome = c("Active", "Inactive", "Active")
)
pc_activity_matrix(activity_long)

pc_lifecycle_policy()
}
}
