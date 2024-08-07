% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getter_functions.R
\name{request_args}
\alias{request_args}
\title{Retrieve Function Inputs}
\usage{
request_args(object, .which = NULL, ...)
}
\arguments{
\item{object}{An object returned from related request functions of the PubChem database.}

\item{.which}{A string specifying which argument's content to retrieve from \code{object}. If \code{NULL}, all
function inputs will be returned.}

\item{...}{Additional arguments. These have no effect on the returned outputs and are included for
compatibility with S3 methods in the PubChemR package.}
}
\value{
A list or string vector containing the options used in the function call.
}
\description{
This function retrieves the input arguments from a specified PubChem database request object.
}
\examples{
request <- get_cids("aspirin", namespace = "name")

request_args(request, "identifier")
request_args(request)

}
