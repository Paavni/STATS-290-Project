\name{removeStopWords}
\alias{removeStopWords}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove stop words
}
\description{
removeStopWords removes a global list of stop words from the location string. 
}
\usage{
removeStopWords(location)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{location}{
a character vector 
}
}
\details{
Removes the stop words and returns the cleaned location.
}
\value{
a character vector
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
It is advisable to use loadStopWords to load the default list of stop words if the user has not created his/her own global list of stop words.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\section{Warning }{
Issues a warning if a list of stop words is not found in the global environment. 
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
loadStopWords()
result <- removeStopWords("I am living a dream in LA")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ clean }
\keyword{ utility }% __ONLY ONE__ keyword per line
