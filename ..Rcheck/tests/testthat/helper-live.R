is_live_smoke_enabled <- function() {
  flag <- tolower(Sys.getenv("PUBCHEMR_LIVE_SMOKE", "false"))
  flag %in% c("1", "true", "yes", "on")
}

skip_if_not_live_smoke <- function() {
  testthat::skip_if(
    !is_live_smoke_enabled(),
    "Live PubChem smoke tests are disabled. Set PUBCHEMR_LIVE_SMOKE=true to run."
  )
}
