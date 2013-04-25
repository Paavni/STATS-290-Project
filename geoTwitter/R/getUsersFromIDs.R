getUsersFromIDs <-
function (userids, timeout = 3600, file.name = NULL,
                             oauth = NULL, append = FALSE, cainfo = NULL,
                             start = 1, include_entities = FALSE, 
                             suppressOAuthPrompt = FALSE) {
  ## Notes -----------------------------
  # - API via OAuth will allow only 100 ids to be requested at a time
  # - Invalid and/or protected profile IDs may be skipped over
  # - fromJSON() breaks sometimes, possibly due to users putting non-ASCII
  #   characters in their tweets/profiles. Even with unexpected.character='keep'
  #   option in the fromJSON() call, the function still breaks at times. Only
  #   soltion at this time is to just skip the chunk of IDs for which it breaks.
  # ====================================
  
  # Read in userids from either input vector or file
  if (length(userids) > 1){
    userids <- as.vector(as.matrix(unlist(userids)))
  } else {
    userids <- as.vector(as.matrix(read.table(userids)))
  }
  # Handle writing to <file.name>
  if (!is.character(file.name)) 
    stop ('file.name must be specified as type \'character\'.')
  # Overwrite file if <append = FALSE>
  if (!append) { 
    file <- file(file.name, 'w')
    header <- t(matrix(c('id_str', 'followers', 'statuses', 'location', 
                                      'timezone', 'lat', 'lon')))
    write.table(header, file, sep = ',', col.names = FALSE, row.names = FALSE)
    close(file)
  }
  # Check if authentication is provided via oauth
  if (is.null(oauth) & !suppressOAuthPrompt) {
    continue <- readline(paste('Providing OAuth will allow 240 more API ',
                               'requests per hour.\n',
                               'Continue without OAuth? (y/n): ', sep=''))
    while (!(continue %in% c('y', 'Y', 'n', 'N'))) {
      continue <- readline('Please enter y or n: ')
    }
    if (continue %in% c('n', 'N')) stop ('User terminated function.')
  } else if (attr(class(oauth),'package') != 'ROAuth') {
    stop ('oauth must be of class OAuth from the package ROAuth.')
  } else {
    require(ROAuth)
  }
  
  # Some setup
  maxUsers <- 100 # maximum number of userids in request
  idx1 <- start
  idx2 <- maxUsers
  len  <- length(userids)
  params <- list()
  params$user_id <- paste(userids[idx1:idx2], collapse = ',')
  params$include_entities <- as.character(include_entities)
  errors <- list()
  end <- Sys.time() + timeout
  # Main loop for retrieving and writing request results
  while (end > Sys.time() & idx1 != len) {
    stdEnd <- Sys.time() + 3600 # NOTE: time may need adjusting
    # Try using APIv1 first - get requests until rate limited, timeout, or done
    while (all(end > Sys.time(), idx1 != len)) {
      stdUrl <- 'https://api.twitter.com/1/users/lookup.json'
      get    <- stdRequest(stdUrl, params, cainfo)
      if ('error' %in% names(get)) { # rate limited
        errors = c(errors, get$error)
        break()
      } else {
        idx1 <- min(idx2 + 1, len)
        idx2 <- min(idx1 + maxUsers - 1, len)
        get  <- t(sapply(get, getUserInfo))
        file <- file(file.name, 'a')
        write.table(get, file, col.names = FALSE, row.names = FALSE, sep = ',')
        close(file)
        params$user_id <- paste(userids[idx1:idx2], collapse = ',')
      }
    } 
    # ...then try getting APIv1.1 OAuth'ed requests...
    if(!is.null(oauth)) {
      authEnd <- Sys.time() + 900 # note: time may need to be adjusted
      # ...until APIv1 limit is reset, timeout, or done
      while (all(c(end, stdEnd) > Sys.time(), idx1 != len)) {
        authUrl <- 'https://api.twitter.com/1.1/users/lookup.json'
        get <- oauthRequest(authUrl, params, oauth, cainfo)
        if ('error' %in% names(get)) { # rate limited
          errors = c(errors, get$error) 
          if (any(c(authEnd, stdEnd) + 1 < end)) { # can get more requests in
            # so wait a bit, before trying again
            Sys.sleep(max(min(c(stdEnd, authEnd) - Sys.time()), end - end))
          } else { # no point in waiting, so just quit now
            break()
          }
        } else {
          idx1 <- min(idx2 + 1, len)
          idx2 <- min(idx1 + maxUsers - 1, len)
          get  <- t(sapply(get, getUserInfo))
          file <- file(file.name, 'a')
          write.table(get,file, col.names = FALSE, row.names = FALSE, sep = ',')
          close(file)
          params$user_id <- paste(userids[idx1:idx2], collapse = ',')
        }
      }
    }
    # Wait until we can make standard requests again or until timeout is up
    if (stdEnd + 1 < end) {
      Sys.sleep(max(min(stdEnd - Sys.time()), end - end))
    } else {
      break()
    }
  }
  return(list(endIdx = idx2, errors = errors)) 
}
