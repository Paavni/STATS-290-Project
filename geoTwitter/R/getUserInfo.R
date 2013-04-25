getUserInfo <-
function(user) {
  error <- function(e) NULL
  userid    <- tryCatch(user$id, error = error)
  followers <- tryCatch(user$followers_count, error = error)
  statuses  <- tryCatch(user$statuses_count, error = error)
  location  <- tryCatch(user$location, error = error)
  timezone  <- tryCatch(user$time_zone, error = error)
  lat       <- tryCatch(user$status$coordinate$coordinates[2], error = error)
  lon       <- tryCatch(user$status$coordinate$coordinates[1], error = error)
  if(is.null(userid))    userid    <- NA
  if(is.null(followers)) followers <- NA
  if(is.null(statuses))  statuses  <- NA
  if(is.null(location))  location  <- NA
  if(is.null(timezone))  timezone  <- NA
  if(is.null(lat))       lat       <- NA
  if(is.null(lon))       lon       <- NA
  c(userid, followers, statuses, location, timezone, lat, lon)
}
