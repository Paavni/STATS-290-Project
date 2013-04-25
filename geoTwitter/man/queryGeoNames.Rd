\name{queryGeoNames}
\alias{queryGeoNames}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Query GeoNames
}
\description{
queryGeoNames queries a location of given type using the geographical database Geo Names.
}
\usage{
queryGeoNames(location = "", type = "", countryBias = "US", fuzzy = 1, orderby = "relevance", username, password = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{location}{
a character vector for location
}
  \item{type}{
a character vector for type, supported types are Coordinates, Names, Postal. Returns missing values for invalid types. 
}
  \item{countryBias}{
character vector indicating the preferred country code of the result from the query, for e.g. US
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
Queries GeoNames and returns a list of resulting name of the location, latitude, longitude and country code of the location. It returns missing values for invalid location or type. 
}
\value{
a list of name of the location, latitude, longitude and country code of the location. 
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
Issues a warning if the type of the location is invalid.
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
\dontrun{
location <- c("College Park, MD")
type <- c("Name")
result <- queryGeoNames(location,type,username = "geotwitter")
location <- c("")
type <- c("")
result <- queryGeoNames(location,type,username = "geotwitter")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ query }
\keyword{ location }% __ONLY ONE__ keyword per line
