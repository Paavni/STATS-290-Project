\name{readTweetsToFile}
\alias{readTweetsToFile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simplifing JSON files
}
\description{
Function to convert large JSON file of tweets to a more readable file while avoiding storing the data in memory
}
\usage{
readTweetsToFile(file = "./tweets.csv", jsonFile, fields = c(5, 9, 14, 37, 38), sep = ",", chunkSize = 1000L)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
    either a character string naming a file or a connection open for writing. "" indicates output to the console.
}
  \item{jsonFile}{
the name of a file to read data values from. The file name is interpreted relative to the current working directory (given by getwd()), unless it specifies an absolute path. Typically this is the .json file that is produced when streamR::filterStream is called so that each line is a separate tweet.
}
  \item{fields}{
numeric vector indicating which columns from the output of streamR::parseTweets (which is called by this function) to write to the file. The default is c(5,9,14,37,38) which collects id_str, created_at, location, lat, long. See documentation for streamR for full list of available fields
}
  \item{sep}{
  the field separator string. Values within each row of the output file are separated by this string.
}
  \item{chunkSize}{
  positive integer: the number of lines at a time from the jsonFile to process and write to the output file
}
}
\details{
  readTweetsToFile allows a user to convert the .json file obtained from running streamR's filterStream function and output it into a readable, delimmited file without loading the data on memory. The function reads in the json file chunkSize at a time creating a data.frame of tweet data containing fields specified by the user. The data.frame is then written to the desired file and removed from memory so that the next chunk can be read.

Prior to writing the data.frame to the file, some processing is done to format time and latitude and longitude if those fields are included. 
}
\author{
 Andy Galdi <andy.galdi@gmail.com>
}

\examples{
	\dontrun{
	readTweetsToFile(jsonFile = 'example.json')	
	}
}

