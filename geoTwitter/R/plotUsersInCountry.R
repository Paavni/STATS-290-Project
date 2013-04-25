plotUsersInCountry <-
function (x, y, data, country, types = c('points'), 
                                colors = c('#132B43')) {
  require(ggplot2)
  require(maps)
  
  countryData <- map_data('world')
  # tolower() used to ignore case
  rdata <- countryData[tolower(countryData$region) == tolower(country), ]
  if (nrow(rdata) == 0) stop('No such country.')
  args       <- as.list(match.call(expand.dots = TRUE)[-c(1,5)])
  args$rx    <- substitute(long)
  args$ry    <- substitute(lat)
  args$rdata <- rdata
  args$group <- substitute(group)
  p <- do.call(plotUsersInRegion, args)
  return(p)
}
