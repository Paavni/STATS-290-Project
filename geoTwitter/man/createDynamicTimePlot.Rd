\name{createDynamicTimePlot}
\alias{createDynamicTimePlot}
\title{
Function to create dynamic map
}
\description{
Creates an interactive map illustrating tweet density over time. A slider lets the user control the time parameter in the map.
}
\usage{
createDynamicTimePlot(times, regionIds, map, startTime, units, interval, rampColor = c("white", "blue"), labelText)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{times}{
vector of POSXIct times
}
  \item{regionIds}{
vector of ids for each region to map (can be gotten with latLong2RegionNum)
}
  \item{map}{
 character string naming a geographical database, or a list of x, y, and names obtained from a previous call to maps::map. The string choices include \"world\", \"usa\", \"state\", \"county\", and more (see package maps).
}
  \item{startTime}{
time from which the slider starts. Must be POSIXct. If no time is specified then the minimum time times is used.
}
  \item{units}{
character string. Units in which the time from startTime is calculated. Can be abbreviated.
}
  \item{interval}{
real valued length of time interval in slider in units of the argument specified in units
}
  \item{rampColor}{
vector of length 2 containing the beginning and end color of the color gradient.
}
  \item{labelText}{
the text label to appear on top of the slider. If missing then defaults to paste("Time Since Start (in ", interval, substr(units, 1, nchar(units)-1)," segments)")
}
}
\details{
createDynamicTimePlot utilizes the widget capabilities in the tcltk package.
}
\author{
 Andy Galdi <andy.galdi@gmail.com>
}
\examples{
\dontrun{
nDataPoints = 1000
datecol = as.POSIXct(rep(Sys.time(),nDataPoints)+sample(1:3600,nDataPoints,replace=TRUE))
stateids = sample(1:2, nDataPoints, replace=TRUE)

times = datecol
regionIds = stateids
dataset=data.frame(times = datecol, region.ids = stateids)
map = 'state'
units = 'mins'
interval = 5

createDynamicTimePlot(times=times, region.ids=region.ids, dataset=dataset, map=map, units=units, interval=interval)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ interactive }
\keyword{ dynamic }
\keyword{ plotting }
