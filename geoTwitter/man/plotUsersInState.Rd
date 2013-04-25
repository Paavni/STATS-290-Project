\name{plotUsersInState}
\alias{plotUsersInState}

\title{
Plot Users By US State
}
\description{
Plots only those users (or tweets) in a data set that are inside a given state
}
\usage{
plotUsersInState(x, y, data, state, types = c("points"), colors = c("#132B43"))
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
  \item{state}{
  \code{character} specifying the state to plot
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
state <- 'texas'
b <- plotUsersInState(lng, lat, beliebers, state, 
                      colors = c('red', 'blue'), 
                      types = c('cloud', 'contour'))
b + coord_cartesian(xlim = c(-106, -94), ylim = c(26, 36))
}

\keyword{ dplot }
