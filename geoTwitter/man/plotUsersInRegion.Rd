\name{plotUsersInRegion}
\alias{plotUsersInRegion}

\title{
Plot Users By User-Specified Regions
}
\description{
Plots only those users (or tweets) in a data set that are inside a user-specified region
}
\usage{
plotUsersInRegion(x, y, data, rx, ry, rdata, group = NULL, types = c("points"), colors = c("#132B43"))
}

\arguments{
  \item{x}{
  column of \code{data} specifying longitutde points for users
}
  \item{y}{
  column of \code{data} specifying latitude points for users
}
  \item{data}{
  \code{data.frame} containing longitutde and latitude points for users
}
  \item{rx}{
	column of \code{rdata} specifying longitude points for regions
}
  \item{ry}{
	column of \code{rdata} specifying latitude points for regions
}
  \item{rdata}{
	\code{data.frame} containing longitude and latitude points specifying the regions
}
  \item{group}{
  column of \code{rdata} specifying the individual regions (one polygon per region)
}
  \item{types}{
  \code{character} vector containing one or all of \code{'points'}, \code{'cloud'}, or \code{'contour'}. The plot will lbe layered depending on the order in which thes types are specified
}
  \item{colors}{
  \code{character} vector containing the colors of each aesthetic specified in \code{types}
}
}

\value{
\code{ggplot} object
}

\author{
Robert MacNguyen <rmacngu@stanford.edu>
}

\seealso{
\code{\link{plotUsersInState}}, \code{\link{plotUsersInCountry}}
}
\examples{
require(ggplot2)
require(maps)
region <- map_data('world')
data(beliebers)
data(teamobama)
# can plot two data sets on the same map by manipulating ggplot layers
# future implementations could enable this functionality in one function
b <- plotUsersInRegion(lng, lat, beliebers, long, lat, region, group, 
                       colors = 'red')
o <- plotUsersInRegion(lng, lat, teamobama, long, lat, region, group, 
                       colors = 'blue')
p <- b + o$layers[2]
}

\keyword{ dplot }
