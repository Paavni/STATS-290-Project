updateStopwords <-
function(newWords){
	if(!("stopWords" %in% ls(envir = .GlobalEnv))){
		stop("Global list of stop words not found. Use loadStopWords to use the
		 default list")
	}
	if(length(newWords) == 0)
	{
		stop("length of list of new stop words is zero")
	}
	stopWords <<- c(stopWords, newWords)
}
