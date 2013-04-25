\name{cleanLocationFromFile}
\alias{cleanLocationFromFile}
\title{
Cleaning location strings in file
}
\description{
Function that cleans tweet location by first reading in a file of twitter data on disk as an ffdf then cleaning the location field in chunks using cleanLocation so as not to occupy RAM. Returns a reduced size data.frame of ids and cleaned locations
}
\usage{
cleanLocationFromFile(file = "./tweets.csv", colClasses, ..., chunkSize = 5000L)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
   file containing twitter data, which must conform to two criteria: (1) the file must be comma separated; (2) the file must have a header row in which the id column is labeled "id_str" and the location column is labeled "location". 
}
  \item{colClasses}{
A character vector of classes to be assumed for the columns. Recycled as necessary. Note that "character" is not implemented in ffdfs and instead "factor" or "ordered" must be used. 

}
  \item{\dots}{
futher arguments passed to read.csv
}
  \item{chunkSize}{
 positive integer: the number of locations to clean at a time
}
}
\details{
cleanTweetLocations allows a user to clean a large amount of location data without having to storing all of that data in RAM. It works by reading a .csv file of cleaned twitter data (provided by readTweetsToFile or getUsersFromIDs) into a ffdf. Chunks of a user specified size are removed from the ffdf, cleaned using cleanLocation, and blank locations are removed. Recent publications including Cheng, Caverlee, and Lee (2010) suggest that user inputted location data is sparse, "only 26\% have listed a user location as granular as a city name (e.g., Los Angeles, CA)". The sparsity in Twitter location data is utilized by throwing out these blank locations to (hopefully) arrive at a dataset size that is small enough to store on RAM as a data.frame. It should be noted however that it cannot be guaranteed that the resulting data.frame will be small enough to store on memory so the user should call this function with caution.
}
\value{
A data.frame of 4 columns the input ids, the uncleaned location, the cleaned location, and the type of the location.
}
\references{
Cheng, Caverlee, and Lee (2010). "You Are Where You Tweet: A Content-Based Approach to Geo-locating Twitter Users." CIKM 2010.
}
\author{
 Andy Galdi <andy.galdi@gmail.com>
}
\note{
Due to the strong restrictions on the inputs to this function it is highly recommended that the file input is generated using either readTweetsToFile or getUsersFromIDs
}

\examples{
test <- data.frame(id_str = c("123", "456", "789"),
                   location = c("Palo Alto, CA", "MaRyLanD!!", ""))
tf <- tempfile()
write.csv(test, tf)

cleanTest = cleanLocationFromFile(file = tf, 
                                  colClasses = c(id_str = "factor", 
                                               location = "factor"),
                                  chunkSize = 1L)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ clean }
\keyword{ location }
\keyword{ big data }
