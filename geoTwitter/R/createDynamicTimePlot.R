createDynamicTimePlot <- function(times, regionIds, map, startTime, units,
                                  interval, rampColor = c("white", "blue"),
                                  labelText) {
  require(tcltk)
  require(ggplot2)
  require(tkrplot)
  # create data.frame with input data or use given dataframe
  if (length(times) != length(regionIds))
    stop('\'times\' and \'regionIds\' must be the same length.')
  if (length(rampColor) != 2)
    stop('\'rampColor\' must contain exactly 2 colors')
  if (missing(labelText))
    labelText = paste("Time Since Start (in ", interval,
                      substr(units, 1, nchar(units)-1)," segments)")
  if (!is.character(labelText))
    stop('\'labelText\' must be of type character')
  dataset <- data.frame(time=times, region=regionIds)
  if(missing(startTime)) startTime <- min(dataset$time)
  timeSinceStart <- as.numeric(difftime(dataset$time, startTime, units=units))
  if (!is.numeric(interval))
    stop('\'interval\' must be a real number')
  if (interval <= 0)
    stop('\'interval\' must be > 0')
  timeGroups <- ceiling(timeSinceStart / interval)
  # append data.frame with timeGroups and column of ones
  dataset <- cbind(dataset, timeGroups, count = rep(1, nrow(dataset)))
  # remove dates to allow for later aggregation with sum
  dataset <- dataset[,-1]
  # add ids for all regions if their count is 0
  regs <- map(map, fill=TRUE, regions=".", col="transparent", plot=FALSE)
  addedRegs <- cbind.data.frame(1:length(regs$names),
                                rep(min(dataset$timeGroups),
                                    length(regs$names)),
                                rep(0, length(regs$names)))
  names(addedRegs) <- names(dataset)
  dataset <- rbind(dataset, addedRegs)
  # add every combination of region and timeGroup with zeros so
  # all combinations are included in aggregation
  a <- sapply(unique(dataset$region), "[")
  b <- sapply(unique(dataset$timeGroups), "[")
  c <- expand.grid(a,b)
  extra <- cbind(c, rep(0, nrow(c)))
  names(extra) <- names(dataset)
  dataset <- rbind(dataset, extra)
  # Aggregate dataset by region and timeGroups
  # return only columns for region, timeGroup and count
  # (which now contains row counts from dataset)
  dataAgg <- aggregate(dataset,
                       by=list(id = dataset$region, timeG = dataset$timeGroups),
                       sum)[,c(1,2,5)]
  
  ##########
  # Slider
  ##########
  tt = tktoplevel()
  time = tclVar(0)
  plotMap = function(...){
    tim = as.numeric(tclvalue(time))
    counts = tclVar(init=subset(dataAgg, timeG == tim)[,3])
    cnts = as.numeric(unlist(strsplit(tclvalue(counts), split = " ")))
    colfunc <- colorRampPalette(c(rampColor[1], rampColor[2]))
    classcolors <- colfunc(max(c(0,cnts))+1)
    map(map, fill = TRUE, col = classcolors[cnts+1])
  }
  img = tkrplot(tt,plotMap)
  Mapplot = function(...)tkrreplot(img)
  labelScale <- tklabel(tt, text=labelText)
  scl <- tkscale(tt, command = Mapplot, from = 0, to = max(dataset$timeGroups),
                 showvalue = TRUE, variable = time, resolution = 1,
                 orient = 'horiz')
  tkpack(img, side = 'top')
  tkpack(labelScale, scl, side = 'top')
}