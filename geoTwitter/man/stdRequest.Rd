\name{stdRequest}
\alias{stdRequest}

\title{
Standard Request to Twitter API
}
\description{
Returns parsed results from a query to the Twitter API
}
\usage{
stdRequest(uri, params, cainfo = NULL)
}

\arguments{
  \item{uri}{
  \code{character}, URI to query
}
  \item{params}{
  \code{list}, parameters to URI
}
  \item{cainfo}{
  certificat file
}
}

\value{
\code{list} received after parsing the JSON file returned by Twitter
}
\references{
https://dev.twitter.com/docs/api/1
}
\author{
Robert MacNguyen <rmacngu@stanford.edu>
}
\note{
See Warnings in \code{\link{getUsersFromIDs}}
}

\seealso{
\code{\link{oauthRequest}}
}
\examples{
\dontrun{
require(RCurl)
url <- 'https://api.twitter.com/1/users/lookup.json'
params <- list('user_id' = 405210383)
cainfo <- cainfo <- system.file('CurlSSL', 'cacert.pem', package = 'RCurl')
result <- stdRequest(url, params, cainfo)
}
}
