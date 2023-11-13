# get_aids ----
#' \dontrun{
#'. get_aids(
#'   identifier = "aspirin",
#'   namespace = "name",
#'  )
#' }

# download ----
#' \dontrun{
#'  download(
#'   outformat = "json",
#'   path = "/Users/selcukkorkmaz/Documents/aspirin.json",
#'   identifier = "aspirin",
#'   namespace = "name",
#'   domain = "compound",
#'   operation = NULL,
#'   searchtype = NULL,
#'   overwrite = FALSE
#'  )
#' }

# get_all_sources ----
#' \dontrun{
#'  get_all_sources(
#'   domain = 'substance'
#'. )
#' }

# get_assays ----
#' \dontrun{
#'  get_assays(
#'   identifier = 1234,
#'   namespace = "aid",
#'  )
#' }

# get_cids ----
#' \dontrun{
#'  get_cids(
#'   identifier = "aspirin",
#'   namespace = "name",
#'  )
#' }

# get_compounds ----
#' \dontrun{
#'  get_compounds(
#'   identifier = "aspirin",
#'   namespace = "name",
#'  )
#' }

# get_json ----
#'  \dontrun{
#'   get_json(
#'   identifier = "aspirin",
#'   namespace = "name",
#'  )
#' }

# get_properties ----
#' \dontrun{
#'  get_properties(
#'   properties = "IsomericSMILES",
#'   identifier = "aspirin",
#'   namespace = "name",
#'  )
#' }

# get_sdf ----
#' \dontrun{
#'  get_sdf(
#'   identifier = "aspirin",
#'   namespace = "name",
#'  )
#' }

# get_sids ----
#' \dontrun{
#' get_sids(
#'   identifier = "aspirin",
#'   namespace = "name",
#'  )
#' }

# get_substances ----
#' \dontrun{
#'  get_substances(
#'   identifier = "aspirin",
#'   namespace = "name",
#'  )
#' }

# get_synonyms ----
#' \dontrun{
#'. get_synonyms(
#'   identifier = "aspirin",
#'   namespace = "name",
#'  )
#' }

# get ----
#' \dontrun{
#'  get(
#'   identifier = "aspirin",
#'   namespace = "name",
#'  )
#' }
#'
#' \donttest{
#' if(!curl::has_internet()) {
#'   message("Internet connection required for this example. Skipping.")
#' } else {
#'   result <- tryCatch({
#'     get(
#'       identifier = "aspirin",
#'       namespace = "name",
#'     )
#'   }, error = function(e) {
#'     message(paste("An error occurred or the API is not reachable:", e$message))
#'     # Fallback example or message
#'     message("Showing example with pre-loaded data...")
#'     # [Insert code with pre-loaded data here]
#'     return(list())  # Return an empty list in case of an error
#'   })
#' }
#'}

# request ----
#' \dontrun{
#'  request(
#'   identifier = "aspirin",
#'   namespace = "name",
#'  )
#' }
