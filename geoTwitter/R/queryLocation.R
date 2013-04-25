queryLocation <-
function(user = NULL, location = NULL, type = NULL, 
	countryBias = "", fuzzy = 1, orderby = "relevance", username, password=""){
	require(httr)
	require(rjson)
	if(missing(username)){
		stop("A valid user is required to use GeoNames")
	}

	if(missing(user) | missing(location) | missing(type)){
		stop("user and location are required parameters")
	}
	
	user <- as.vector(user)
	location <- as.vector(location)
	type <-as.vector(type)
	if((length(user) != length(location)) | (length(user) != length(type)) | 
		(length(type) != length(location))){
		stop("user, location and type should be of same length")
	}

	location <- as.character(location)
	type <- as.character(type)
	credits <- (sum(type=="Name"))+(sum(type=="Postal"))+(4*(sum(type=="Coordinates")))
	if(credits >= 10000){
		warning(paste("You are querying for ", credits, " credits. Please check
		 GeoNames for latest rate limits", sep = ""))
	}
	result <- mapply(queryGeoNames, location = location, type = type, 
		MoreArgs=list(countryBias = countryBias, fuzzy = fuzzy, 
			orderby = orderby, username=username, password=password))
	dfResult <- data.frame("user" = user)
	dfResult$name <- as.character(unlist(result["name", ]))
	dfResult$lat <- as.character(unlist(result["lat", ]))
	dfResult$lng <- as.character(unlist(result["lng", ]))
	dfResult$countryCode <- as.character(unlist(result["countryCode", ]))
	return(dfResult)
}
