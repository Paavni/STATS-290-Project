cleanString <-
function(location){
	if(location == "" | is.na(location)){
		loc <- ""
		type <- ""
		return(list("loc" = loc, "type" = type))
	}
	coordGrep = '\\s*(\\-?\\d+(\\.\\d+)?)\\s*,\\s*(\\-?\\d+(\\.\\d+)?)\\s*'
	coordGsub = '[^(^\\s*(\\-?\\d+(\\.\\d+)?)\\s*,\\s*(\\-?\\d+(\\.\\d+)?)\\s*$)]'
	postalGrep = '(^|\\s+)(\\d{5}([\\-]?\\d{4})?)(\\s+|$)'
	anGsub = '\\.(?=\\.+)|,(?=,+)|([^a-zA-Z0-9\\.,\\s])'
	if(grepl(coordGrep,location, perl = TRUE)){ # check for Coordinates in the string
		loc <- gsub(coordGsub, "",location, perl = TRUE)
		type <- "Coordinates"
	} else if(grepl(postalGrep,location, perl = TRUE)){ # check for Postal Codes in the string
		strs <- unlist(strsplit(split = " ", x = location, perl = TRUE))
		strl <- unlist(lapply(strs, matchPostalCode))
		index <- length(strl[strl==TRUE])	
		loc <- strs[strl==TRUE][index]
		type <- "Postal"
	} else {
		loc <- gsub(anGsub, "",location, perl = TRUE)
		loc <- trim(loc)
		type <- "Name"
	}	
	if(is.null(loc)) loc <- ""
	if(is.null(type)) type <- ""
	return(list("loc" = loc, "type" = type))
}
