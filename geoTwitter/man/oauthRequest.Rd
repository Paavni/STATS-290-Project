\name{oauthRequest}
\alias{oauthRequest}

\title{
OAuth Request to Twitter API
}
\description{
Returns parsed results from an authenticated query to the Twitter API
}
\usage{
oauthRequest(uri, params, oauth, cainfo = NULL)
}

\arguments{
  \item{uri}{
  \code{character}, URI to query
}
  \item{params}{
  \code{list}, parameters to URI
}
  \item{oauth}{
  \code{OAuth} object with completed handshake to Twitter
}
  \item{cainfo}{
  certificat file
}
}

\value{
\code{list} received after parsing the JSON file returned by Twitter
}
\references{
https://dev.twitter.com/docs/api/1.1
}
\author{
Robert MacNguyen <rmacngu@stanford.edu>
}

\note{
See Warnings in \code{\link{getUsersFromIDs}}
}

\seealso{
\code{\link{stdRequest}}
}
\examples{
\dontrun{
# need OAuth object
require(RCurl)
url <- 'https://api.twitter.com/1.1/users/lookup.json'
params <- list('user_id' = 405210383)
cainfo <- cainfo <- system.file('CurlSSL', 'cacert.pem', package = 'RCurl')
result <- oauthRequest(url, params, oauth, cainfo)
}
}
