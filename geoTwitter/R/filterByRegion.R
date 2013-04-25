filterByRegion <-
function(x, y, data = NULL, rx, ry, rdata = NULL, 
                           group = NULL) {
  require(sp) # for point.in.polygon()
  # Check if <data> is a data.frame
  if (!is.null(data)) {
    if(!is.data.frame(data)) stop('\'data\' must be a data.frame.')
    x <- eval(substitute(x), data)
    y <- eval(substitute(y), data)
  }
  if (length(x) != length(y)) stop('\'x\' and \'y\' must be the same length.')
  # Check if <rdata> is a data.frame
  if (!is.null(rdata)) {
    if(!is.data.frame(rdata)) stop('\'rdata\' must be a data.frame.')
    rx    <- eval(substitute(rx), rdata)
    ry    <- eval(substitute(ry), rdata)
    group <- eval(substitute(group), rdata)
  }
  if (length(rx) != length(ry))
    stop('\'rx\' and \'ry\' must be the same length.')
  # Check if <group> is provided
  if (!is.null(group)) {
    if (length(group) != length(rx))
      stop('\'group\' must of the same length as \'rx\' and \'ry\'.')
    groups <- unique(group)
    result <- list()
    for (grp in groups) { 
      if (is.na(grp)) grpIdx <- which(is.na(group))
      else grpIdx <- which(group == grp)
      pol.x <- rx[grpIdx]
      pol.y <- ry[grpIdx]
      result[[grp]] <- which(point.in.polygon(x, y, pol.x, pol.y) > 0)
    }
  } else { 
    # if no <group>, then assume all one polygon
    result <- which(point.in.polygon(x, y, rx, ry) > 0)
  }
  return(result)
}
