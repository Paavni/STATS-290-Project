\name{cleanString}
\alias{cleanString}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Clean and characterize location string
}
\description{
cleanString cleans unwanted characters from the location string and characterizes it to a type. Currently supported types are Coordinates, Name and Postal. 
}
\usage{
cleanString(location)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{location}{
a character vector 
}
}
\details{
Removes unwanted characters, characterizes the location using regex matching and returns the cleaned location and the type of the location.
}
\value{
a character vector indicating the type, Coordinates, Name, Postal or an empty string for an invalid type if no match is found. 
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
location <- c("Living in C.A.")
result <- cleanString(location) 
location <- c("iPhone: 34.479984,-93.004616")
result <- cleanString(location) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ clean }
\keyword{ location }% __ONLY ONE__ keyword per line
