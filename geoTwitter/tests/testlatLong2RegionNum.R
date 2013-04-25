# test testlatLong2RegionNum
# run function from package
library(geoTwitter)
pointsDF <- data.frame(lon = c(-79.4042, -122.1419, -75.1642),
                       lat = c(43.6481, 37.4419, 39.9522))
rownames(pointsDF) <- c("Toronto, CA", "Palo Alto, CA", "Philadelphia, PA")
regionIds <- latLong2RegionNum(pointsDF$lon, pointsDF$lat, map = 'state')
# return names to check accuracy
processed <- map('state', fill = TRUE, col = "transparent", plot = FALSE)$names[regionIds]

# actual values
actual <- c(NA, "california", "pennsylvania")
stopifnot(all.equal(processed, actual))
