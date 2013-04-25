\name{trim}
\alias{trim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Removing unwanted spaces
}
\description{
trim is used to remove any leading, trailing and unwanted multiple spaces in the input string using regex matching.
}
\usage{
trim(str)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{str}{
a character vector
}
}
\details{
Removes any leading, trailing and unwanted multiple spaces from str and returns a cleaned character vector. 
}
\value{
a character vector with leading, trailing and unwanted multiple spaces cleaned. 
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
trim("     I     live    in Stanford      ")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ proces }
\keyword{ clean }% __ONLY ONE__ keyword per line
