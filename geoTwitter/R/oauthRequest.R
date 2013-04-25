oauthRequest <-
function (uri, params, oauth, cainfo=NULL) {
  require(rjson)
  require(RCurl)
  if(is.null(cainfo)) 
    cainfo <- system.file('CurlSSL', 'cacert.pem', package = 'RCurl')
  get <- tryCatch(oauth$OAuthRequest(URL = uri, params = params, method = 'GET', 
                                     cainfo=cainfo), error=function(e) e)
  if ('error' %in% class(get)) {
    get <- list(error = get)
  } else {
    get <- try(fromJSON(get, unexpected.escape = 'keep'), silent = TRUE)
    if(class(get) == 'try-error') get <- ''
  }
  return(get)
}
