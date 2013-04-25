\name{getAreaByRegion}
\alias{getAreaByRegion}

\title{
Get Area of Regions
}
\description{
Computes the areas of regions defined by longitude/latitude polygons
}
\usage{
getAreaByRegion(rx, ry, rdata = NULL, group = NULL, units = "miles")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
  \item{units}{
  one of \code{'miles'}, \code{'km'}, or\code{'m'} for computing areas
}
}

\value{
  \code{numeric} or \code{list} of areas, depending on if \code{groups} was specified
}
\references{
From \code{areaPolygon}: Bevis, M. and G. Cambareri, 1987. Computing the area of a spherical polygon of arbitrary shape. Mathematical Geology 19: 335-346
}

\author{
Robert MacNguyen <rmacngu@stanford.edu>
}
\note{
Care needs to be taken when specifying groups for the regions. Each group must be associated with only one polygon, otherwise overlapping and self-intersection of polygons may occur. Thus, if a region is made of more than one polygon, post processing must be done in order to correctly aggregate points over that region.
}


\examples{
require(ggplot2)
require(maps)
region <- map_data('state')
areas  <- getAreaByRegion(long, lat, region, group)
}

\keyword{ dplot }
