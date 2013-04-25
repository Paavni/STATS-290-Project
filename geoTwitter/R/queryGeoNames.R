queryGeoNames <-
function(location="", type="", countryBias = "US", fuzzy = 1, 
	orderby = "relevance", username, password=""){
	
	if(missing(username)){
		stop("A valid user is required to use GeoNames")
	}

	name <- NA 
	lat <- NA
	lng <- NA
	countryCode <- NA
	
	if(grepl('^\\s*$',location, perl = TRUE)){
		resultList <- list("name" = name, "lat" = lat, "lng" = lng, 
			"countryCode" = countryCode)
	    return(resultList)
	}

	if(type=="Postal"){
		locJSON  <- tryCatch(GET("http://api.geonames.org/", 
			path = "postalCodeSearchJSON", query = list("postalcode"=location, 
				"maxRows"=1, "countryBias"=countryBias, "fuzzy"=fuzzy, 
				"orderby"=orderby, "username" = username, "password" = password)), 
		error =  function(e) {}, warning = function(w) {})
		if(locJSON$status_code == 200){
			resJSON  <- unlist(fromJSON(as.character(locJSON)))
			if(!is.null(resJSON)){
				name <- as.character(resJSON["postalCodes.placeName"])
				lat <- as.character(resJSON["postalCodes.lat"])
				lng <- as.character(resJSON["postalCodes.lng"])
				countryCode <- as.character(resJSON["postalCodes.countryCode"])
			}	
		}
	} else if(type=="Coordinates"){
		lat <- tryCatch(as.numeric(unlist(strsplit(location, ","))[1]), 
			error =  function(e) {}, warning = function(w) {})
		lng <- tryCatch(as.numeric(unlist(strsplit(location, ","))[2]), 
			error =  function(e) {}, warning = function(w) {})
		locJSON  <- tryCatch(GET("http://api.geonames.org/", 
			path = "findNearbyJSON", query = list("lat"=lat, "lng"=lng, 
				"username" = username, "password" = password)), 
		error =  function(e) {}, warning = function(w) {})
		if(locJSON$status_code == 200){
			resJSON  <- unlist(fromJSON(as.character(locJSON)))
			if(!is.null(resJSON)){
				name <- as.character(resJSON["geonames.name"])
				lat <- as.character(resJSON["geonames.lat"])
				lng <- as.character(resJSON["geonames.lng"])
				countryCode <- as.character(resJSON["geonames.countryCode"])
			}	
		}	
	} else if(type == "Name"){	
		locJSON  <- tryCatch(GET("http://api.geonames.org/", 
			path = "searchJSON", query = list("q"=location, 
				"maxRows"=1, "countryBias"=countryBias, "fuzzy"=fuzzy, 
				"orderby"=orderby, "username" = username, "password" = password)), 
		error =  function(e) {}, warning = function(w) {})
		if(locJSON$status_code == 200){
			resJSON  <- unlist(fromJSON(as.character(locJSON)))
			if(!is.null(resJSON)){
				name <- as.character(resJSON["geonames.name"])
				lat <- as.character(resJSON["geonames.lat"])
				lng <- as.character(resJSON["geonames.lng"])
				countryCode <- as.character(resJSON["geonames.countryCode"])
			}	
		}
	} else {
		warning("invalid type")
	}
	resultList <- list("name" = name, "lat" = lat, "lng" = lng, 
		"countryCode" = countryCode)
	return(resultList)
}
