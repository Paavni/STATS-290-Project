\name{updateStopwords}
\alias{updateStopwords}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Update list of stop words
}
\description{
updateStopWords adds the input string or list of strings to the list of default stop words provided by the package. 
}
\usage{
updateStopwords(newWords)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{newWords}{
string or a list of strings. 
}
}
\details{
Adds the given string(s) to the stop words and updates the global list of stop words. 
}
\author{
Paavni Rattan <rattan.paavni@gmail.com>
}
\note{
It is advisable to use loadStopWords to load the default list of stop words if the user has not created his/her own global list of stop words.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\section{Warning }{
Issues a warning if a global list of stop words is not found in the global environment. 
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
\dontrun{
loadStopWords()
updateStopwords(c("sky", "lol"))
updateStopwords("lol")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utility }
\keyword{ clean }% __ONLY ONE__ keyword per line
