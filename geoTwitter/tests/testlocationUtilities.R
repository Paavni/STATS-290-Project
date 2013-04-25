library(geoTwitter)
# test matchPostalCode
# run function from package
processed <- matchPostalCode("94305-1234")

# actual values
actual <- TRUE
stopifnot(all.equal(processed, actual))

# run function from package
processed <- matchPostalCode("94305")

# actual values
actual <- TRUE
stopifnot(all.equal(processed, actual))

# run function from package
processed <- matchPostalCode("943")

# actual values
actual <- FALSE
stopifnot(all.equal(processed, actual))

# run function from package
processed <- matchPostalCode("943051")

# actual values
actual <- FALSE
stopifnot(all.equal(processed, actual))

# test trim
# run function from package
processed <- trim("     I     live in Stanford     ")

# actual values
actual <- "I live in Stanford"
stopifnot(all.equal(processed, actual))


# test removeStopWords, loadStopWords and updateStopWords
loadStopWords()
updateStopwords("today")
# run function from package
processed <- trim(removeStopWords(" dream LA today"))

# actual values
actual <- "LA"
stopifnot(all.equal(processed, actual))
rm(stopWords, envir = .GlobalEnv)
