trim <-
function (str) {
	str <- gsub("^\\s+|\\s+$", "", str)
	str <- gsub("\\s+", " ", str)
	return(str)
}
