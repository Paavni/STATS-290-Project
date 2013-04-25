mergeTimesRegionId <-
function(file = "./tweets.csv", header = TRUE,
                               quote = "\"", FUN='read.csv',
                               colClasses = c(id_str = "factor", 
                                            created_at = "POSIXct",
                                            location = "factor",
                                            lon = "double",
                                            lat = "double"),
                               ..., regionIds) {
    # Check that 'created_at' is in colClasses                                                                                
    if (!('created_at' %in% names(colClasses)))
      stop('No field named \'created_at\' specified in \'colClasses\'')
    # Read in file as an ffdf                                                                                                 
    require(ff)
    ffdataframe <- read.table.ffdf(file = file, header = header, quote= quote,
                                   colClasses = colClasses,
                                   FUN = FUN, ...)
    data.frame(
      times = as.POSIXct(ffdataframe$created_at[which(!is.na(regionIds))]),
      regionIds = regionIds[which(!is.na(regionIds))])
}
