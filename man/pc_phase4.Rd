% Manual documentation for Phase 4 analysis-layer helpers
\name{pc_phase4}
\alias{pc_phase4}
\alias{pc_assay_activity_long}
\alias{pc_export_model_data}
\title{Phase 4 Analysis-Layer Helpers}
\description{
Utilities for transforming assay summary payloads into activity-ready long tables
and exporting model-ready data artifacts.
}
\usage{
pc_assay_activity_long(
  identifier = NULL,
  namespace = "cid",
  x = NULL,
  chunk_size = NULL,
  unique_rows = TRUE,
  add_outcome_value = TRUE,
  outcome_map = NULL,
  strict_outcome = FALSE,
  unknown_outcome = NA_real_,
  ...
)

pc_export_model_data(
  x,
  path,
  format = c("csv", "rds"),
  include_ids = TRUE,
  include_outcome = TRUE
)
}
\arguments{
\item{identifier}{Identifier vector used when \code{x} is \code{NULL}.}

\item{namespace}{Namespace for \code{identifier} when requesting from PubChem.}

\item{x}{Optional source object. Supported values are a \code{PubChemResult}
or a parsed payload list containing \code{Table/Columns/Row}.}

\item{chunk_size}{Optional chunk size for large identifier vectors.}

\item{unique_rows}{Logical; remove duplicate rows after normalization.}

\item{add_outcome_value}{Logical. If \code{TRUE}, adds a numeric
\code{ActivityOutcomeValue} column when \code{ActivityOutcome} exists.}

\item{outcome_map}{Optional named mapping passed to
\code{pc_activity_outcome_map()}.}

\item{strict_outcome}{Logical. If \code{TRUE}, unknown outcome labels error.}

\item{unknown_outcome}{Numeric fallback for unknown labels when
\code{strict_outcome = FALSE}.}

\item{...}{Additional arguments forwarded to \code{pc_request()} when \code{x} is \code{NULL}.}

\item{path}{Output path for exported data.}

\item{format}{Export format, one of \code{"csv"} or \code{"rds"}.}

\item{include_ids}{Logical; include identifier columns for \code{PubChemModelMatrix} inputs.}

\item{include_outcome}{Logical; include outcome column for \code{PubChemModelMatrix} inputs.}
}
\value{
\code{pc_assay_activity_long()} returns a tibble (classed as \code{PubChemTable})
with normalized fields such as \code{CID}, \code{AID}, and \code{ActivityOutcome}
when available.

\code{pc_export_model_data()} invisibly returns a list containing output metadata:
\code{path}, \code{format}, \code{rows}, and \code{cols}.
}
\examples{
\donttest{
res <- pc_request(
  domain = "compound",
  namespace = "cid",
  identifier = 2244,
  operation = "assaysummary",
  output = "JSON"
)

activity_long <- pc_assay_activity_long(x = res)
activity_long

mm <- pc_model_matrix(
  data.frame(CID = c("1", "2"), f1 = c(0.2, 1.1), f2 = c(1.3, 2.4))
)

pc_export_model_data(
  mm,
  path = file.path(tempdir(), "model_features.csv"),
  format = "csv"
)
}
}
