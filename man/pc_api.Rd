% Manual documentation for Phase 2 next-generation PubChemR API
\name{pc_api}
\alias{pc_api}
\alias{pc_config}
\alias{pc_cache_clear}
\alias{pc_cache_info}
\alias{pc_request}
\alias{pc_response}
\alias{pc_compound}
\alias{pc_substance}
\alias{pc_assay}
\alias{pc_property}
\alias{pc_feature_table}
\alias{pc_identifier_map}
\alias{pc_batch}
\alias{pc_resume_batch}
\alias{pc_benchmark}
\alias{pc_submit}
\alias{pc_poll}
\alias{pc_collect}
\alias{print.PubChemResult}
\alias{print.PubChemBatchResult}
\alias{as_tibble.PubChemResult}
\alias{as_tibble.PubChemRecord}
\alias{as_tibble.PubChemIdMap}
\alias{as_tibble.PubChemBatchResult}
\title{Next-Generation PubChemR API}
\description{
Typed, fault-tolerant transport and workflow APIs for PubChem retrieval,
batch orchestration, caching, and asynchronous listkey workflows.
}
\usage{
pc_config(...)

pc_cache_clear(cache_dir = NULL, memory = TRUE, disk = TRUE)

pc_cache_info(cache_dir = NULL)

pc_request(
  domain = "compound",
  namespace = "cid",
  identifier = NULL,
  operation = NULL,
  searchtype = NULL,
  output = "JSON",
  options = NULL,
  method = c("GET", "POST"),
  body = NULL,
  rate_limit = TRUE,
  timeout = NULL,
  retries = NULL,
  pause_base = NULL,
  pause_cap = NULL,
  user_agent = NULL,
  cache = FALSE,
  cache_dir = NULL,
  cache_ttl = NULL,
  force_refresh = FALSE,
  offline = NULL,
  ...
)

pc_response(response, request = list())

pc_compound(
  identifier,
  namespace = "cid",
  operation = NULL,
  searchtype = NULL,
  output = "JSON",
  options = NULL,
  ...
)

pc_substance(
  identifier,
  namespace = "sid",
  operation = "record",
  output = "JSON",
  options = NULL,
  ...
)

pc_assay(
  identifier,
  namespace = "aid",
  operation = "description",
  output = "JSON",
  options = NULL,
  ...
)

pc_property(
  identifier,
  properties,
  namespace = "cid",
  searchtype = NULL,
  options = NULL,
  ...
)

pc_feature_table(
  identifier,
  properties = c(
    "MolecularWeight",
    "XLogP",
    "TPSA",
    "HBondDonorCount",
    "HBondAcceptorCount"
  ),
  namespace = "cid",
  numeric_only = TRUE,
  ...
)

pc_identifier_map(
  identifier,
  namespace = "name",
  to = c("cids", "sids", "aids"),
  domain = "compound",
  searchtype = NULL,
  options = NULL,
  ...
)

pc_batch(
  ids,
  fn,
  chunk_size = 100,
  parallel = FALSE,
  workers = NULL,
  checkpoint_dir = NULL,
  checkpoint_id = NULL,
  resume = FALSE,
  rerun_failed = TRUE,
  ...
)

pc_resume_batch(
  fn,
  checkpoint_dir,
  checkpoint_id,
  parallel = FALSE,
  workers = NULL,
  rerun_failed = TRUE,
  ...
)

pc_benchmark(
  ids,
  fn,
  chunk_sizes = c(25, 50, 100),
  parallel_options = c(FALSE),
  workers = NULL,
  ...
)

pc_submit(
  domain = "compound",
  namespace = "cid",
  identifier = NULL,
  operation = NULL,
  searchtype = NULL,
  output = "JSON",
  options = NULL,
  ...
)

pc_poll(
  x,
  domain = "compound",
  operation = NULL,
  output = "JSON",
  options = NULL,
  interval = 1.5,
  max_attempts = 20,
  ...
)

pc_collect(x, ...)
}
\arguments{
\item{...}{Additional arguments passed through to lower-level request functions.}

\item{cache}{Logical; enable deterministic memory + disk caching.}

\item{cache_dir}{Cache directory path.}

\item{cache_ttl}{Cache TTL in seconds.}

\item{body}{Optional POST body payload for \code{pc_request(method = "POST")}.}

\item{checkpoint_dir}{Optional directory to persist per-chunk checkpoint files.}

\item{checkpoint_id}{Optional checkpoint run id for \code{pc_batch}; required for \code{pc_resume_batch}.}

\item{disk}{Logical; clear disk cache in \code{pc_cache_clear}.}

\item{domain}{PubChem domain such as \code{compound}, \code{substance}, or \code{assay}.}

\item{fn}{Function to apply to each identifier chunk in \code{pc_batch}.}

\item{force_refresh}{Logical; bypass cache when \code{TRUE}.}

\item{identifier}{Identifier(s) for a query.}

\item{ids}{Identifier vector for batching.}

\item{chunk_size}{Chunk size used by \code{pc_batch}.}

\item{interval}{Polling interval in seconds for async listkey workflows.}

\item{max_attempts}{Maximum polling attempts in \code{pc_poll}.}

\item{memory}{Logical; clear in-memory cache in \code{pc_cache_clear}.}

\item{method}{HTTP method for \code{pc_request}; one of \code{"GET"} or \code{"POST"}.}

\item{namespace}{Namespace for identifiers.}

\item{operation}{PUG operation segment.}

\item{options}{Named list of query options.}

\item{output}{Output format, default \code{"JSON"}.}

\item{offline}{\code{TRUE} to enable cache-only replay mode in \code{pc_request}.}

\item{parallel}{Logical; enable parallel chunk processing in \code{pc_batch}.}

\item{parallel_options}{Logical vector of parallel settings tested by \code{pc_benchmark}.}

\item{resume}{Logical; resume \code{pc_batch} from an existing checkpoint manifest.}

\item{rerun_failed}{Logical; when resuming, rerun chunks previously marked as failed.}

\item{pause_base}{Retry pause base for \code{httr::RETRY}.}

\item{pause_cap}{Retry pause cap for \code{httr::RETRY}.}

\item{properties}{Property names for \code{pc_property}.}

\item{numeric_only}{If \code{TRUE}, \code{pc_feature_table} attempts numeric coercion for feature columns.}

\item{rate_limit}{\code{TRUE} to use configured rate limit, \code{FALSE} to disable, or numeric req/sec.}

\item{request}{Request metadata list for \code{pc_response}.}

\item{response}{A \code{httr} response object or raw payload text.}

\item{retries}{Retry count for \code{pc_request}.}

\item{searchtype}{Optional PubChem search type.}

\item{timeout}{Request timeout in seconds.}

\item{to}{Target mapping operation in \code{pc_identifier_map}.}

\item{user_agent}{User-agent string for HTTP requests.}

\item{workers}{Number of workers for parallel batching.}

\item{chunk_sizes}{Integer vector of chunk sizes evaluated by \code{pc_benchmark}.}

\item{x}{A \code{PubChemAsyncQuery} object or listkey string for \code{pc_poll}; a
\code{PubChemAsyncQuery} object for \code{pc_collect}.}
}
\value{
Most query functions return a typed object inheriting from \code{PubChemResult}.
\code{pc_batch} returns \code{PubChemBatchResult}; \code{pc_submit} returns
\code{PubChemAsyncQuery}.
}
\examples{
\donttest{
res <- pc_compound(2244, namespace = "cid")
tibble::as_tibble(res)

id_map <- pc_identifier_map(c("aspirin", "ibuprofen"), namespace = "name", to = "cids")
tibble::as_tibble(id_map)

batch <- pc_batch(
  ids = c("aspirin", "caffeine", "ibuprofen"),
  fn = function(x) pc_identifier_map(x, namespace = "name", to = "cids"),
  chunk_size = 2
)
batch

pc_feature_table(
  identifier = c(2244, 3672),
  properties = c("MolecularWeight", "XLogP"),
  namespace = "cid"
)
}
}
