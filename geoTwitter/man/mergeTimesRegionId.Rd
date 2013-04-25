\name{mergeTimesRegionId}
\alias{mergeTimesRegionId}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to prepare data for plotting with createDynamicTimePlot
}
\description{
Prepares data for createDynamicTimePlot by merging the tweet time column in the .csv file (typically) produced by readTweetsToFile with the regionIds which can produced by latLong2RegionNum. Pairs in which regionId is NA are removed.
}
\usage{
mergeTimesRegionId(file = "./tweets.csv", header = TRUE, quote = "\"",
FUN = 'read.csv', colClasses = c(id_str = "factor", created_at =
"POSIXct", location = "factor", lon = "double", lat = "double"),  ..., regionIds)
}
\arguments{
\item{file}{
    .csv file containing tweet data. Typically the file that is produced by readTweetsToFile
}
\item{header}{
  logical: indicating whether the file contains the names of the
  variables as its first line
}
\item{quote}{
  the set of quoting characters
}
\item{FUN}{
  character: name of a function that is called for reading each chunk.
}
 \item{colClasses}{
A character vector of classes to be assumed for the columns. Recycled as necessary. Note that "character" is not implemented in ffdfs and instead "factor" or "ordered" must be used. 
}
  \item{\dots}{
futher arguments passed to read.csv
}
  \item{regionIds}{
numerical id of the regions associated with the location in the file. Typically produced by latLong2RegionNum
}
}
\details{
mergeTimesRegionId first reads in the .csv file of twitter data into an ffdf so that it remains on disk rather than in RAM then only places the elements of the column of time stamps in the .csv file that have a corresponding non NA regionId in RAM.   
}
\value{
A data.frame of containing two columns: times and regionIds
}
\author{
 Andy Galdi <andy.galdi@gmail.com>
}

\examples{
test <- data.frame(id_str = c("123", "456", "789"),
                   created_at = rep(Sys.time(),3),
                   location = c("Palo Alto, CA", "MaRyLanD!!", ""))
tf <- tempfile()
write.csv(test, tf)

pointsDF <- data.frame(lon = c(-79.4042, -122.1419, -75.1642),
                       lat = c(43.6481, 37.4419, 39.9522))
rownames(pointsDF) <- c("Toronto, CA", "Palo Alto, CA", "Philadelphia, PA")
regionIds <- latLong2RegionNum(pointsDF$lon, pointsDF$lat, map = 'state')

mergeTimesRegionId(file = tf, colClasses = c(id_str = "factor",
                                             created_at = "factor",
                                             location = "factor"), 
                   regionIds = regionIds)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Big data }
