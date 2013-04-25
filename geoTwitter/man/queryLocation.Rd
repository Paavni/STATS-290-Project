\name{queryLocation}
\alias{queryLocation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Query locations using GeoNames
}
\description{
queryLocation queries locations of given types for a list of users. 
}
\usage{
queryLocation(user = NULL, location = NULL, type = NULL, countryBias = "", fuzzy = 1, orderby = "relevance", username, password = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{user}{
a vector of user 
}
  \item{location}{
a vector of cleaned locations
}
  \item{type}{
a vector of location types
}
  \item{countryBias}{
character vector indicating the preferred country code of the result from the query , for e.g. US
}
  \item{fuzzy}{
numeric value between 0 and 1, defining the fuzziness of the query
}
  \item{orderby}{
character vector defining the criteria to sort the results from GeoNames query, for e.g. relevance, population
}
  \item{username}{
character vector indicating valid GeoNames username
}
  \item{password}{
character vector indicating valid password for the GeoNames username 
}
}
\details{
Queries GeoNames and returns a data frame of resulting name of the location, latitude, longitude and country code of the user.
}
\value{
a data frame with a column for user, name of the location, latitude, longitude and country code of the location. 
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Paavni Rattan <rattan.paavni@gmail.com>
}
\note{
It is advisable to pass the cleaned location from cleanLocation method to queryLocation to avoid missing and irrelevant results. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\section{Warning }{
Issues a warning to check the latest rate limit of GeoNames if querying for more than 10,000 GeoNames credits.
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
\dontrun{
user <- c("1111a", "2222b", "3333c", "4444d", "5555d")
location <- c("College Park, MD", "21093", "34.479984,-93.004616", "The sky is the LIMIT", "")
type <- c("Name", "Postal", "Coordinates", "Name", "")
result <- queryLocation(user,location,type,username = "geotwitter")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ location }
\keyword{ query }% __ONLY ONE__ keyword per line
