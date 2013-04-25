latLong2RegionNum <-
function(lon, lat, map) {
  require(sp)
  require(maps)
  require(maptools)
  
  if (length(lon) != length(lat))
    stop('\'lon\' and \'lat\' must be of equal length')
  if (!is.numeric(lon))
    stop('\'lon\' must be numeric')
  if (!is.numeric(lat))
    stop('\'lat\' must be numeric')
  # create data.frame
  pointsDF <- cbind.data.frame(lon, lat)
  # replace NAs with (0,0)
  if (length(which(is.na(pointsDF$lon))) > 0)
    pointsDF[which(is.na(pointsDF$lon)), ] <- c(0,0)
  
  # Prepare SpatialPolygons object with one SpatialPolygon
  states <- map(map, fill = TRUE, col = "transparent", plot = FALSE)
  IDs <- states$names
  states_sp <- map2SpatialPolygons(
    states, IDs = IDs, proj4string = CRS("+proj=longlat +datum=wgs84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string = CRS("+proj=longlat +datum=wgs84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  names <- stateNames[indices]
  
  # Look up state ID number by name
  match(names, IDs)
}
