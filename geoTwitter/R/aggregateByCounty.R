aggregateByCounty <-
function(x, y, data = NULL, area = TRUE) {
  require(ggplot2)
  require(maps)
  rdata    <- map_data('county')
  countyID <- paste(rdata$subregion, rdata$region, sep=',')
  counties <- unique(countyID)
  args       <- as.list(match.call(expand.dots = TRUE)[-1])
  args$rx    <- substitute(long)
  args$ry    <- substitute(lat)
  args$rdata <- rdata
  args$group <- substitute(group)
  summary <- do.call(aggregateByRegion, args)
  result  <- numeric()
  for (x in counties) {
    grpIdx <- unique(rdata$group[countyID == x])
    if(area){
      total   <- colSums(summary[grpIdx, c('counts', 'areas')])
      density <- as.numeric(total[1] / total[2])
      if(is.na(density)) density <- 0
      result <- data.frame(rbind(result, c(total, 'density' = density)))
    } else {
      result <- c(result, sum(summary[grpIdx]))
    }
  }
  cbind.data.frame(counties, result)
}
