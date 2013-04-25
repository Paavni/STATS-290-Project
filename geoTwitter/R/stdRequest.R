stdRequest <-
function (uri, params, cainfo = NULL) {
  require(rjson)
  require(RCurl)
  if(is.null(cainfo)) 
    cainfo <- system.file('CurlSSL', 'cacert.pem', package = 'RCurl')
  uriExtra <- paste(names(params), params, collapse = '&', sep = '=')
  get <- tryCatch(getURI(paste(uri, uriExtra, sep = '?'), cainfo = cainfo), 
                  error=function(e) e)
  if ('error' %in% class(get)) {
    get <- list(error = get)
  } else {
    get <- try(fromJSON(get, unexpected.escape = 'keep'), silent = TRUE)
    if(class(get) == 'try-error') get <- ''
  }
  return(get)
}
