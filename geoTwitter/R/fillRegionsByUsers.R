fillRegionsByUsers <-
function (x, y, data, rx, ry, rdata, group, 
                               density=FALSE) {
  require(ggplot2)
  require(maps)
  
  if (missing(data)) stop('Must provide coordinate data in <data>')
  if(!is.data.frame(data)) stop('\'data\' must be a data.frame.')
  x <- match.call()[['x']]
  y <- match.call()[['y']]

  if (missing(rdata)) stop('Must provide region data in <rdata>.')
  if(!is.data.frame(rdata)) stop('\'rdata\' must be a data.frame.')
  rx    <- match.call()[['rx']]
  ry    <- match.call()[['ry']]
  group <- match.call()[['group']]
  
  args      <- as.list(match.call(expand.dots = TRUE)[-c(1,9)])
  args$area <- density
  summaries <- do.call(aggregateByRegion, args)
  
  p <- ggplot()
  if (density) {
    summaries <- cbind(rownames(summaries), summaries)
    colnames(summaries) <- c('group', 'counts', 'areas', 'densities')
    data <- merge(rdata, summaries, by = 'group')
    p <- p + geom_polygon(data = data, aes_string(x = rx, y = ry, 
                                                  fill = 'densities', 
                                                  group = group))
    p <- p + scale_fill_gradient(trans = 'log10')
  } else {
    summaries <- data.frame(cbind(seq(1:length(summaries)), summaries))
    colnames(summaries) <- c('group', 'counts')
    data <- merge(rdata, summaries, by = 'group')
    p <- p + geom_polygon(data = data, aes_string(x = rx, y = ry, 
                                                  fill = 'counts', 
                                                  group = group))
  }
  p <- p + geom_polygon(data = rdata, aes_string(x = rx, y = ry, group = group),
                        color = 'white', fill = NA)
  return(p)
}
