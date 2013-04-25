# test filterByRegion
# run function from package
#    filter points into separate regions and grouped regions
# ======================================
library(geoTwitter)
set.seed(0)
t   <- c(seq(0, 2 * pi, pi / 12), 0)
r1x <- cos(t) + 2
r1y <- sin(t) 
r2x <- cos(t) - 2
r2y <- sin(t) 
# generate sample points
x <- rnorm(40, 0, 2)
y <- rnorm(40, 0, 1)
# combine regions into one data frame and give them groupings
xy      <- data.frame(x = x, y = y)
regions <- data.frame(x = c(r1x, r2x),
                      y = c(r1y, r2y),
                      grp = c(rep(1, length(r1x)), rep(2,length(r2x))))
# filter by combined group (regions 2, 3) and a separate group (region 1)
processed <- filterByRegion(x, y, xy, x, y, regions, grp)
actual <- list(c(3, 11, 24, 35, 37), c(12, 18, 28, 29, 32, 34))
stopifnot(all.equal(processed, actual))

# test getAreaByRegion()
# run function from package
# get areas for separate group of regions on a sphere (lon/lat coordinates)
# ======================================
# define region 1 - circle of radius 1
set.seed(0)
t   <- c(seq(0, 2 * pi, pi / 36), 0)
r1x <- cos(t)
r1y <- sin(t)
# define region 2 - 1x1 box
r2x <- c(-2, -3, -3, -2, -2)
r2y <- c(2, 2, 3, 3, 2)
# combine regions into one data frame and give them groupings
regions <- data.frame(x = c(r1x, r2x),
                      y = c(r1y, r2y),
                      grp = c(rep(1, length(r1x)), rep(2, length(r2x))))
processed <- lapply(getAreaByRegion(x,y, regions, grp),signif,6)
actual <- list(15011.6, 4780.09)
stopifnot(all.equal(processed, actual))
