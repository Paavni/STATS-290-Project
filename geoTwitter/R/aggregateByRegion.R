aggregateByRegion <-
function(x, y, data = NULL, rx, ry, rdata = NULL, 
                              group = NULL, area = TRUE) {
  args <- as.list(match.call(expand.dots = TRUE)[-c(1, 9)])
  idx  <- do.call(filterByRegion, args)
  if (is.list(idx)) counts <- unlist(lapply(idx, length))
  else counts <- length(idx)
  if (area) {
    args <- args[c('rx', 'ry', 'rdata', 'group')]
    args <- args[!sapply(args, is.null)]
    areas <- do.call(getAreaByRegion, args)
    areas <- unlist(areas)
    densities <- unlist(counts) / unlist(areas)
    densities[is.nan(densities)] <- 0
    data.frame(cbind(counts, areas, densities))
  } else {
    counts
  }
}
