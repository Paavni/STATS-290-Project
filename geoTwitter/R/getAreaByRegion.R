getAreaByRegion <-
function (rx, ry, rdata = NULL, group = NULL, 
                             units = 'miles') { 
  ## Notes -----------------------------
  # - Can run into trouble if polygons self-overlap. See ?areaPolygon.
  # ====================================
  
  require(geosphere) # for areaPolygon()
  
  # check if 'rdata' is a data.frame
  if (!is.null(rdata)) {
    if(!is.data.frame(rdata)) stop('\'rdata\' must be a data.frame.')
    rx    <- eval(substitute(rx), rdata)
    ry    <- eval(substitute(ry), rdata)
    group <- eval(substitute(group), rdata)
  }
  
  if (length(rx) != length(ry))
    stop('\'rx\' and \'ry\' must be the same length.')
  
  # make sure longitudes are in [-180, 180]
  rx[rx > 180] <- rx[rx > 180] - 360
  rx[rx < -180] <- rx[rx < -180] + 360
  
  # Earth radius in 
  r <- switch(units, km = 6378.1370, m = 6378137, miles = 3963.19)
  
  # Check if <group> is provided
  if (!is.null(group)) {
    if (length(group) != length(rx))
      stop('\'group\' must of the same length as \'rx\' and \'ry\'.')
    groups <- unique(group)
    result <- list()
    # need to sum areas for the groups that are part of the region
    for (grp in groups) { 
      if (is.na(grp)) grpIdx <- which(is.na(group))
      else grpIdx <- which(group == grp)
      pol.x <- rx[grpIdx]
      pol.y <- ry[grpIdx]
      if (length(grpIdx) < 4) result[[grp]] <- 0
      else result[[grp]] <- areaPolygon(cbind(pol.x, pol.y), r = r)
    }
  } else { 
    # if no <group>, then assume all one polygon
    result <- areaPolygon(cbind(rx, ry), r = r)
  }
  return(result)
}
