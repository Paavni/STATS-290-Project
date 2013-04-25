cleanLocation <-
function(user, location, stopWords = NULL){
	if(missing(user) | missing(location)){
		stop("user and location are required parameters")
	}
	user <- as.vector(user)
	location <- as.vector(location)
	if(length(user) != length(location)){
		stop("user and location should be of same length")
	}
	if(!is.null(stopWords)){
		stopWords <<- as.vector(stopWords)
	}
	if(is.null(stopWords) | is.character(stopWords)){
		loadStopWords()
	}
	df <- data.frame(user, location)
	names(df) <- c("user", "location")

	location <- as.character(location)
	cleanLocation <- unlist(lapply(location, removeStopWords))
	locAndType <- lapply(cleanLocation, cleanString)
	locAndType <- sapply(locAndType, "unlist")
	df$clean <- locAndType["loc", ]
	df$type <- locAndType["type",]
	rm(stopWords, envir = .GlobalEnv)
	return(df)
}
