plotUsersInRegion <-
function (x, y, data, rx, ry, rdata, group = NULL, 
                               types = c('points'), colors = c('#132B43')) {
  require(ggplot2)
  require(maps)
  
  if (any(!(types %in% c('points', 'contour', 'cloud'))))
    stop('Unknown plot type.')
  
  if (missing(data)) stop('Must provide coordinate data in \'data>\'.')
  if(!is.data.frame(data)) stop('\'data\' must be a data.frame.')
  x <- match.call()[['x']]
  y <- match.call()[['y']]

  if (missing(rdata)) stop('Must provide region data in <rdata>')
  if(!is.data.frame(rdata)) stop('\'rdata\' must be a data.frame.')
  rx <- match.call()[['rx']]
  ry <- match.call()[['ry']]
  group <- match.call()[['group']]

  # this will filter the data into the appropriate regions as defined by group
  args <- as.list(match.call(expand.dots = TRUE)[-c(1, 9, 10)])
  idx <- unlist(do.call(filterByRegion, args))
  
  p <- ggplot()
  p <- p + geom_polygon(data=rdata, aes_string(x = rx, y = ry, group = group),
                        color = 'grey', fill = 'white')
  # this makes sure each type suplied has a color
  if (length(colors) < length(types))
    colors <- rep(colors,3)[1:length(types)]
  # then a layer is made for each plot type that is supplied and stacked in the
  #   appropriate order in p$layers
  if ('points' %in% types) {
    points <- geom_point(data = data[idx,], aes_string(x = x, y = y), 
                         color = colors[types == 'points'])
    p$layers[[which(types == 'points') + 1]] <- points
  }
  if ('contour' %in% types) {
    contour <- geom_density2d(data = data[idx,], aes_string(x = x, y = y),
                              color = colors[types == 'contour'], size=1)
    p$layers[[which(types == 'contour') + 1]] <- contour
  }
  if ('cloud' %in% types) {
    colfunc <- colorRampPalette(c(colors[types == 'cloud'], 'white'))
    cloud <- stat_density2d(data = data[idx,], geom = "tile", n = 100, 
                   aes_string(x = x, y = y, fill = '(..density..)'),
                   contour = FALSE, alpha = 0.5)
    cloudIdx <- which(types == 'cloud')
    p$layers[[cloudIdx + 1]] <- cloud
    p <- p + scale_fill_gradientn(colours = c(colors[cloudIdx], 'white'))
  }
  return(p)
}
