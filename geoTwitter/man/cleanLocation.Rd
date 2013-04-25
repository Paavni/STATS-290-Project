\name{cleanLocation}
\alias{cleanLocation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Clean location strings
}
\description{
cleanLocation cleans locations of given types for a list of users. 
}
\usage{
cleanLocation(user, location, stopWords = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{user}{
a vector of user 
}
  \item{location}{
a vector of locations
}
  \item{stopWords}{
a character vector of stop words
}
}
\details{
Cleans locations and returns a data frame of user, location, cleaned location and type of the location. 
}
\value{
a data frame with a column for user, location, cleaned location and type of the location. 
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Paavni Rattan <rattan.paavni@gmail.com>
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
user <- c("1111a", "2222b", "3333c", "4444d")
location <- c("College Park, MD", "The SKY is the  LIMIT", "21093", "iPhone: 34.479984,-93.004616")
result <- cleanLocation(user,location)  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ location }
\keyword{ clean }% __ONLY ONE__ keyword per line
