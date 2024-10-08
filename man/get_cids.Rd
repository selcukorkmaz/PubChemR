% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_cids.R
\name{get_cids}
\alias{get_cids}
\title{Retrieve Compound IDs (CIDs) from PubChem}
\usage{
get_cids(
  identifier,
  namespace = "name",
  domain = "compound",
  searchtype = NULL,
  options = NULL
)
}
\arguments{
\item{identifier}{A vector of identifiers, either numeric or character.
The type of identifier depends on the \code{namespace} and \code{domain} parameters.
**Note**: \code{identifier} must be provided; it cannot be \code{NULL}.}

\item{namespace}{A character string specifying the namespace of the identifier.

                 Possible values depend on the \code{domain} parameter and include:

                 - For \code{domain = 'compound'}: \code{cid}, \code{name}, \code{smiles}, \code{inchi}, \code{sdf}, \code{inchikey}, \code{formula}, etc.

                 - For \code{domain = 'substance'}:\code{sid}, \code{sourceid/<source id>}, \code{sourceall/<source name>}, \code{name}, etc.

                 - For \code{domain = 'assay'}: \code{aid}, \code{listkey}, \code{type/<assay type>}, \code{sourceall/<source name>}, etc.

                 For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Input}{Input} section of the PUG REST API.}

\item{domain}{A character string specifying the domain of the query.

              Possible values are:

              - \code{compound} (default)

              - \code{substance}

              - \code{assay}

              - Other domains as specified in the API documentation.}

\item{searchtype}{An optional character string specifying the search type.

                  Possible values depend on the \code{namespace} and \code{domain}.

                  Examples include:

                  - \code{substructure}, \code{superstructure}, \code{similarity}, \code{identity} for structure searches.

                  - \code{fastidentity}, \code{fastsimilarity_2d}, \code{fastsimilarity_3d}, etc. for fast searches.

                  If \code{NULL} (default), no search type is specified.}

\item{options}{A list of additional options for the request.

               Available options depend on the specific request and the API.

               Examples include:

               - For similarity searches: \code{list(Threshold = 95)}

               - For substructure searches: \code{list(MaxRecords = 100)}

               If \code{NULL} (default), no additional options are included.

               For more details, see the \href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Structure-Search-Operations}{Structure Search Operations} section of the PUG REST API.}
}
\value{
An object of class 'PubChemInstance_CIDs', which is a list containing information retrieved from the PubChem database. Compound IDs can be extracted from the returned object using the \link{CIDs} function.
}
\description{
This function sends a request to PubChem to retrieve Compound IDs (CIDs) for given identifier(s).
}
\details{
For more detailed information, please refer to the
\href{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest}{PubChem PUG REST API documentation}.
}
\note{
To extract compoud IDs from returned object, one may use \link{CIDs} function. See examples.
}
\examples{
compound <- get_cids(
  identifier = "aspirin",
  namespace = "name"
)

compound

# Extract compound IDs.
CIDs(compound)

}
\seealso{
\link{CIDs}, \link{get_pug_rest}
}
