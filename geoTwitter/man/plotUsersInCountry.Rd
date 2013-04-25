\name{plotUsersInCountry}
\alias{plotUsersInCountry}

\title{
Plot Users By Country
}
\description{
Plots only those users (or tweets) in a data set that are inside a given country
}
\usage{
plotUsersInCountry(x, y, data, country, types = c("points"), colors = c("#132B43"))
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
  \item{country}{
  \code{character} specifying the country to plot
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
\code{\link{plotUsersInRegion}}, \code{\link{plotUsersInCountry}}
}
\examples{
require(ggplot2)
data(beliebers)
country <- 'Mexico'
b <- plotUsersInCountry(lng, lat, beliebers, country, 
                        colors = c('red', 'blue'), 
                        types = c('cloud', 'points'))
}

\keyword{ dplot }
