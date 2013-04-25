\name{aggregateByState}
\alias{aggregateByState}

\title{
Aggregate Longitude/Latitude Data by US States
}
\description{
Returns the number of users (or tweets) and optionally the areas and the density of users in each state as defined by the /texttt{'state'} dataset in the \code{maps} package.
}
\usage{
aggregateByState(x, y, data = NULL, area = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  \code{numeric} vector containing longitudes for each point. Optionally, if \code{data} is specified, can be a column name of \code{data}.
}
  \item{y}{
  \code{numeric} vector containing latitudes for each point. Optionally, if \code{data} is specified, can be a column name of \code{data}.
}
  \item{data}{
  \code{data.frame} containing longitude and latitude points for each user. If specified, \code{x} and \code{y} must also be specified.
}
  \item{area}{
	\code{logical}. If \code{TRUE} then areas and densities will be computed and returned
}
}

\value{
\code{data.frame} containing the state names, the count of points in the state, and optionally the area and density of points
}

\author{
Robert MacNguyen <rmacngu@stanford.edu>
}

\seealso{
\code{\link{aggregateByRegion}}, \code{\link{aggregateByCountry}}, \code{\link{aggregateByCounty}}	
}
\examples{
data(beliebers)
result <- aggregateByState(lng, lat, beliebers)
}

\keyword{ dplot }
