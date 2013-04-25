cleanLocationFromFile <-
function(file = "./tweets.csv", colClasses, ...,
                                  chunkSize = 5000L) {
  if (missing(colClasses))
    stop('Must include \'colClasses\' argument')
  if (!is.integer(chunkSize))
    stop('\'chunkSize\' must be of type integer')
  # Check that 'location' is in colClasses
  if (!('location' %in% names(colClasses)))
    stop('No location field specified in \'colClasses\'')
  if (!any(colClasses['location'] %in% c("factor","ordered")))
    stop('\'location\' field must be specified as \'factor\' or \'ordered\'')
  if (!('id_str' %in% names(colClasses)))
    stop('No \'id_str\' field specified in \'colClasses\'')
  if (!any(colClasses['id_str'] %in% c("factor","ordered")))
    stop('\'id_str\' field must be specified as \'factor\' or \'ordered\'')
  # Read in file as an ffdf
  require(ff)
  ffdataframe <- read.csv.ffdf(file=file,...)
  # Extract locations chunkSize at a time and clean
  reqChunks <- ceiling(nrow(ffdataframe)/chunkSize)
  skip <- 0
  counter <- 0
  while(TRUE){
    counter <- counter + 1
    start <- 1 + skip
    end <- min(nrow(ffdataframe), chunkSize*counter)
    locationChunk <- as.character(ffdataframe$location[start:end])
    idChunk <- as.character(ffdataframe$id_str[start:end])
    ### input location and id and return cleaned location in a data.frame
    cleanDF <- cleanLocation(idChunk, locationChunk)
    if (counter == 1){
      cleanedLocation <- subset(cleanDF, clean != "")
    } else {
      cleanedLocation <- rbind(cleanedLocation, subset(cleanDF, clean != "")) 
    }
    rm(locationChunk)
    rm(idChunk)
    rm(cleanDF)
    garbage <- gc(FALSE) #Silently perform garbage collection
    # According to the documentation on gc()
    # it can be useful to call gc after a large object has been removed, 
    # as this may prompt R to return memory to the operating system.
    cat('Chunk ', counter, ' of ', reqChunks, ' complete.\n')
    if(end == nrow(ffdataframe)) break
    skip <- skip + chunkSize
  }
  cleanedLocation
}
