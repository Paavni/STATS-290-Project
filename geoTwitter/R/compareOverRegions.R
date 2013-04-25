compareOverRegions <-
function (xlist, ylist, datalist, 
                                rx, ry, rdata, group) {
  require(ggplot2)
  require(maps)
  
  xlist <- unlist(xlist)
  ylist <- unlist(ylist)
  if (!all(c(length(xlist),length(ylist)) == length(datalist)))
    stop('\'All input lists must be the same length\'.')
  
  if (missing(rdata)) stop('Must provide region data in <rdata>.')
  if(!is.data.frame(rdata)) stop('\'rdata\' must be a data.frame.')
  rxStr    <- match.call()[['rx']]
  ryStr    <- match.call()[['ry']]
  groupStr <- match.call()[['group']]
  rx    <- eval(substitute(rx), rdata)
  ry    <- eval(substitute(ry), rdata)
  group <- eval(substitute(group), rdata)
  n <- length(xlist)
  result <- list
  for (i in 1:n) {
    if(!is.data.frame(datalist[[i]]))
      stop('All elements of \'datalist\' must be data.frames.')
    x <- datalist[[i]][[xlist[[i]]]]
    y <- datalist[[i]][[ylist[[i]]]]
    #dataset <- datalist[[i]]
    summary <- aggregateByRegion(x, y, NULL, rx, ry, NULL, group, F)
    if (i == 1) {
      result <- data.frame(I(summary), I(names(datalist)[i]))
      names(result) <- c('count', 'winner')
      result$winner[summary == 0] <- NA
    } else {
      idx <- which(summary > result[, 1])
      result[idx, 1] <- summary[idx]
      result[idx, 2] <- names(datalist)[i]
    }
  }
  result <- data.frame(seq(1:length(summary)),result$count, as.factor(result$winner))
  names(result) <- c('group', 'count', 'winner')
  datapoly <- merge(result, rdata)
  p <- ggplot()
  p <- p + geom_polygon(data = datapoly, color='#DDDDDD', 
                        aes_string(x = rxStr, y = ryStr, fill = 'winner', 
                                   group = groupStr))
  return(p)
}
