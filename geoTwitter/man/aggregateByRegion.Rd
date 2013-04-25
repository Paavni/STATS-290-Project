\name{aggregateByRegion}
\alias{aggregateByRegion}

\title{
Aggregate Longitude/Latitude Data by User-Specified Regions
}
\description{
Returns the number of users (or tweets)  and optionally the areas and the density of users in each user-input region.
}
\usage{
aggregateByRegion(x, y, data = NULL, rx, ry, rdata = NULL, group = NULL, area = TRUE)
}

\arguments{
   \item{x}{
  \code{numeric} vector containing longitudes for each user. Optionally, if \code{data} is specified, can be a column name of \code{data}.
}
  \item{y}{
  \code{numeric} vector containing latitudes for each user. Optionally, if \code{data} is specified, can be a column name of \code{data}.
}
  \item{data}{
  \code{data.frame} containing longitude and latitude points for each user. If specified, \code{x} and \code{y} must also be specified.
}
  \item{rx}{
  \code{numeric} vector containing longitudes for polygon verticies to define a region. Optionally, if \code{rdata} is specified, can be a column name of \code{rdata}.
}
  \item{ry}{
  \code{numeric} vector containing latitude for polygon verticies to define a region. Optionally, if \code{rdata} is specified, can be a column name of \code{rdata}.
}
  \item{rdata}{
  \code{data.frame} containing longitude and latitude points for defining polygonal regions, as well as the groupings of each region to determine which points belong to which region. If specified, \code{rx} and \code{ry} must also be specified.
}
  \item{group}{
  vector of unique region identifiers. Optionally, if \code{rdata} is specified, can be a column name of \code{rdata}. See note.
}
  \item{area}{
  \code{logical}. If \code{TRUE} then areas and densities will be computed and returned
}
}

\value{
results in either an integer vector of counts over the region or \code{data.frame} consisting of counts, areas, and densities, depending on whether \code{area} was specified as \code{FALSE} or \code{TRUE}, respectively. 
}

\author{
Robert MacNguyen <rmacngu@stanford.edu>
}
\note{
Care needs to be taken when specifying groups for the regions. Each group must be associated with only one polygon, otherwise overlapping and self-intersection of polygons may occur. Thus, if a region is made of more than one polygon, post processing must be done in order to correctly aggregate points over that region.
}

\seealso{
\code{\link{aggregateByCountry}}, \code{\link{aggregateByState}}, \code{\link{aggregateByCounty}}
}
\examples{
require(ggplot2)
require(maps)
data(beliebers)
rdata  <- map_data('state')
result <- aggregateByRegion(lng, lat, beliebers, long, lat, rdata, group)
}

\keyword{ dplot }
