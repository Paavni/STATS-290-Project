removeStopWords <-
function(location){
	if(!("stopWords" %in% ls(envir = .GlobalEnv))){
		stop("Global list of stop words not found. Use loadStopWords to use the
		 default list")
	}
	loc = location
	if(loc == "" | is.na(loc)){
		return(loc)
	}
	words  = unlist(strsplit(loc, " "))
	stoplist = words[(tolower(words) %in% stopWords)]
	for(s in stoplist){	
		s = paste("\\b",s,"\\b", sep = "")
		loc = gsub(s, "", loc, ignore.case = TRUE, perl = TRUE)
	}
	return(loc)
}
