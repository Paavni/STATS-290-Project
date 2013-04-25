\name{compareOverRegions}
\alias{compareOverRegions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compare Longitude/Latitude Datasets Over Regions
}
\description{
For each region specified, plots and colors the region based on which dataset has the most users (or tweets) in the region.
}
\usage{
compareOverRegions(xlist, ylist, datalist, rx, ry, rdata, group)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xlist}{
	list or vector of type \code{character} containing column names specifying longitudes for points belonging to the respective \code{data.frame} in \code{datalist}
}
  \item{ylist}{
	list or vector of type \code{character} containing column names specifying latitudes for points belonging to the respective \code{data.frame} in \code{datalist}
}
  \item{datalist}{
	\code{list} of \code{data.frames} containing longitude and latitude points for users
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
}

\value{
\code{ggplot} object
}

\author{
Robert MacNguyen <rmacngu@stanford.edu>
}


\examples{
require(maps)
require(ggplot2)
region <- map_data('state')
data(beliebers)
data(teamobama)
xlist <- c('lng', 'lng')
ylist <- c('lat', 'lat')
datalist <- list(TheBiebs = beliebers, BarryO = teamobama)
p <- compareOverRegions(xlist, ylist, datalist, long, lat, region, group)
p
}

\keyword{ dplot }
