\name{matchPostalCode}
\alias{matchPostalCode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Matching with US Postal Codes 
}
\description{
matchPostalCode is used to find if the input string matches the pattern of US Postal Codes using regex matching.
}
\usage{
matchPostalCode(str)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{str}{
a character vector
}
}
\details{
Matches the input character vector with the pattern XXXXX-XXXX, where X are digits, 0-9, and returns a logical value indicating match or no match. 
}
\value{
a logical value indicating match or no match. 
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
Currently supports US Postal Codes. 
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
matchPostalCode("94305")
matchPostalCode("94305-1234")
matchPostalCode("943")
matchPostalCode("943051234")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ location }
\keyword{ process }% __ONLY ONE__ keyword per line
