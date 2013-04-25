\name{fillRegionsByUsers}
\alias{fillRegionsByUsers}

\title{
Fill (Color) Regions By Users
}
\description{
Fills in regions on a color scale based on the number of users (or tweets) in that region relative to other regions
}
\usage{
fillRegionsByUsers(x, y, data, rx, ry, rdata, group, density = FALSE)
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
  \item{density}{
  \code{logical}. If \code{TRUE} then fills regions based on densities rather than counts
}
}

\value{
\code{ggplot} object
}

\author{
Robert MacNguyen <rmacngu@stanford.edu>
}

\examples{
require(ggplot2)
require(maps)
data(beliebers)
data(teamobama)
# color regions based on count relative to other regions
region <- map_data('state')
p <- fillRegionsByUsers(lng, lat, beliebers, long, lat, region, group, FALSE)
p
# color regions based on density (users/sq mile) relative to other regions
region <- map_data('state')
p <- fillRegionsByUsers(lng, lat, teamobama, long, lat, region, group, TRUE)
p

}

\keyword{ dplot }
