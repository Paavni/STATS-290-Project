\name{latLong2RegionNum}
\alias{latLong2RegionNum}

\title{
Identifing regions from latitude and longitude
}
\description{
Function that identifies the number associated with each region in a database given by the user by the map argument
}
\usage{
latLong2RegionNum(lon, lat, map)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon}{
 numeric vector of longitudes
}
  \item{lat}{
numeric vector of latitudes}
  \item{map}{
 character string naming a geographical database, or a list of x, y, and names obtained from a previous call to maps::map. The string choices include \"world\", \"usa\", \"state\", \"county\", and more (see package maps).
}
}
\details{
This function is useful in plotting using the maps package to identify regions in a database 
}
\value{
A numeric vector containing the the id number of the region each latitude and longitude pair belongs to. If the latitude and longitude pair is not in any of the regions in the geographical database specified by map, then NA is returned in that spot in the vector.
}
\author{
 Andy Galdi <andy.galdi@gmail.com>
}
\examples{
pointsDF <- data.frame(lon = c(-79.4042, -122.1419, -75.1642),
                       lat = c(43.6481, 37.4419, 39.9522))
rownames(pointsDF) <- c("Toronto, CA", "Palo Alto, CA", "Philadelphia, PA")
(regionIds <- latLong2RegionNum(pointsDF$lon, pointsDF$lat, map = 'state'))
# return names to check accuracy
map('state', fill = TRUE, col = "transparent", plot = FALSE)$names[regionIds]

# Try world database
(regionIds <- latLong2RegionNum(pointsDF$lon, pointsDF$lat, map = 'world'))
map('world', fill = TRUE, col = "transparent", plot = FALSE)$names[regionIds]
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{ maps }
