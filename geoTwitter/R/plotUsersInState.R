plotUsersInState <-
function (x, y, data, state, types = c('points'), 
                              colors = c('#132B43')) {
  require(ggplot2)
  require(maps)
  
  stateData <- map_data('state')
  # tolower() used to ignore case
  rdata <- stateData[tolower(stateData$region) == tolower(state), ]
  if (nrow(rdata) == 0) stop('No such state.')
  args       <- as.list(match.call(expand.dots = TRUE)[-c(1,5)])
  args$rx    <- substitute(long)
  args$ry    <- substitute(lat)
  args$rdata <- rdata
  args$group <- substitute(group)
  p <- do.call(plotUsersInRegion, args)
  return(p)
}
