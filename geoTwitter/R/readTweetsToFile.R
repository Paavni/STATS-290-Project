readTweetsToFile <-
function(file="./tweets.csv", jsonFile,
                             fields = c(5,9,14,37,38), sep=",",
                             chunkSize=1000L){
  require(streamR)
  if (!is.integer(chunkSize))
    stop('\'chunkSize\' must be an integer')
  if (chunkSize <= 0)
    stop('\'chunkSize\' must be > 0')
  # Parse Tweets chunkSize at a time and write to file
  skip <- 0
  counter <- 0 
  while(TRUE){
    counter <- counter + 1
    scannedTweets <- scan(jsonFile, what=character(), skip=skip, n=chunkSize,
                          sep="\n", quiet = TRUE)
    if(length(scannedTweets) == 0) break
    TweetDF <- parseTweets(scannedTweets)[,fields]
    # Format Date
    if ('created_at' %in% names(TweetDF))
      tFormat = "%a %b %d %H:%M:%S +0000 %Y"
      TweetDF$created_at <- as.POSIXct(strptime(TweetDF$created_at,
                                                format=tFormat))
    if ('lon' %in% names(TweetDF))
      TweetDF$lon <- as.numeric(TweetDF$lon)
    if ('lat' %in% names(TweetDF))
      TweetDF$lat <- as.numeric(TweetDF$lat)
    if(counter == 1) {
      header = TRUE 
      append = FALSE
    }else {
      header = FALSE
      append = TRUE
    }
    write.table(TweetDF, file, append = append, sep=sep, row.names=FALSE,
                col.names = header)
    rm(TweetDF)
    garbage <- gc(FALSE) #Silently perform garbage collection
    # According to the documentation on gc()
    # it can be useful to call gc after a large object has been removed, 
    # as this may prompt R to return memory to the operating system.
    skip <- skip + chunkSize
  }
}
