\name{getUsersFromIDs}
\alias{getUsersFromIDs}

\title{
Scrape Twitter User Info
}
\description{
Scrape user information from Twitter given a list of user IDs
}
\usage{
getUsersFromIDs(userids, timeout = 3600, file.name = NULL, oauth = NULL, append = FALSE, cainfo = NULL, start = 1, include_entities = FALSE, suppressOAuthPrompt = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{userids}{
  file or vector containing user IDs to lookup
}
  \item{timeout}{
  \code{numeric}, duration for which to scrape IDs
}
  \item{file.name}{
\code{character}, filename of file to write results to
}
  \item{oauth}{
\code{OAuth} object with completed handshake to Twitter
}
  \item{append}{
\code{logical}, whether to append to \code{file.name}
}
  \item{cainfo}{
	certificate file
}
  \item{start}{
	index of userids to start lookup up from
}
  \item{include_entities}{
	include_entities paramter. See Twitter API
}
  \item{suppressOAuthPrompt}{
	\code{logical}, whether to supress the prompt regarding OAuth
}
}
\details{
While there exist packages to scrape Twitter data, they do not provide the facilities required for scraping large amounts of data from the Twitter REST and Search APIs. The \code{twitteR} package allows access to the REST and Search APIs, while the \code{streamR} package allows access to the Stream API. In order to scrape user information, the REST API must be used. However, API access via \code{twitteR} results in errors due to rate limiting when trying to get a large number of follower IDs. This functions takes into account current rate limits as outlined by Twitter's API documentation, and writes results to a file intermittently so that one can resume scraping at a later time (e.g. when not rate limited). 
}
\value{
Writes results to file. Additionally returns:
\item{cursor }{next cursor to resume scraping at a later date}
\item{errors }{errors received while scraping, usually rate limiting errors}
}
\references{
https://dev.twitter.com/docs/api/1.1/get/users/lookup
}
\author{
Robert MacNguyen <rmacngu@stanford.edu>
}

\section{Warning }{
The function \code{fromJSON} from the \code{rjson} library sometimes returns warnings due to unexpected escape characters in user locations. This is unavoidable. However, this will not mess up the data. A bigger problem is that sometimes \code{rjson} errors out do to an unexpected character. The only solution at this time is to skip the chunk of IDs that are currently being queried when the error occurs.
}

\seealso{
\code{\link{getFollowerIDs}}
}
\examples{
# OAuth authentication using \code{ROAuth}:
# requestURL     <- 'https://api.twitter.com/oauth/request_token'
# accessURL      <- 'http://api.twitter.com/oauth/access_token'
# authURL        <- 'http://api.twitter.com/oauth/authorize'
# consumerKey    <- '<< YOUR KEY >>'
# consumerSecret <- '<< YOUR SECRET >>'
# oauth <- OAuthFactory$new(consumerKey    = consumerKey,
#                           consumerSecret = consumerSecret, 
#                           requestURL     = requestURL,
#                           accessURL      = accessURL, 
#                           authURL        = authURL)
# cainfo <- system.file("CurlSSL", "cacert.pem", package = "RCurl")
# oauth$handshake(cainfo = cainfo)
# save(oauth, file = 'oauth.RData')
\dontrun{
require{RCurl}
biebs <- getFollowerIDs('justinbieber', timeout = 7300, 
                        file.name = 'biebsFollowerIDs.txt')
obama <- getFollowerIDs('barackobama', timeout = 7300,
                        file.name = 'obamaFollowerIDs.txt')
beliebers <- getUsersFromIDs('biebsFollowerIDs.txt', timeout = 7300, 
                             file.name = 'beliebers.txt')
teamobama <- getUsersFromIDs('obamaFollowerIDs.txt', timeout = 7300, 
                             file.name = 'teamobama.txt')
}
}
