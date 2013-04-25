\name{filterByRegion}
\alias{filterByRegion}

\title{
Filter Longitude/Latitude Data By User-Specified Region
}
\description{
Return the indices of the longitude/latitude data corresponding to users (or tweets) in the specified region
}
\usage{
filterByRegion(x, y, data = NULL, rx, ry, rdata = NULL, group = NULL)
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
}

\value{
  either a vector or list, depending on if \code{group} was specified. If specified, then each element of the list is a vector of indices. If not specified, then just a vector of indices
}

\references{
From \code{point.in.polygon}: Uses the C function InPoly(). InPoly is Copyright (c) 1998 by Joseph O'Rourke. It may be freely redistributed in its entirety provided that this copyright notice is not removed.
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
data(beliebers)
idx <- filterByRegion(lng, lat, beliebers, long, lat, region, group)

# cooler example
rx <- c(0, -0.05, -0.15, -0.25, -0.4, -0.5, -0.55, -0.65, 
         -0.75, -0.85, -0.95, -1, -1.4, -1.7, -1.8, -1.8, 
         -1.6, -1.2, -0.77, -0.68, -0.47, -0.27, -0.17, -0.12)
ry <- c(-0.75, -0.58, -0.45, -0.4, -0.45, -0.59, -0.44, 
         -0.36, -0.33, -0.36, -0.45, -0.62, -0.48, -0.25, 
         0, 0, 0.37, 0.74, 0.9, 0.58, 0.43, 0.46, 0.93, 0.73)
rx <- c(rx, rev(-rx))
ry <- c(ry, rev(ry))
x  <- rnorm(300, 0, 1)
y  <- rnorm(300, 1, 1)
xy <- data.frame(x = x, y = y)
regions <- data.frame(x = rx, y = ry, grp = c(rep(1, length(rx))))
idx <- unlist(filterByRegion(x, y, xy, x, y, regions, grp))
# plot results
xlim <- c(min(rx, x), max(rx, x))
ylim <- c(min(ry, y), max(ry, y))
plot(x[-idx], y[-idx], pch = 4, cex = 0.7, 
     xlab = 'x', ylab = 'y', xlim = xlim, ylim = ylim, col = 'blue')
points(x[idx], y[idx], pch = 20, cex = 1, col = 'red')   # region 1
polygon(rx, ry)
}

\keyword{ dplot }
