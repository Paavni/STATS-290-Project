getFollowerIDs <-
function (screenname, userid, timeout=3600, file.name, 
                            append = FALSE, oauth = NULL, cursor = '-1', 
                            cainfo = NULL, suppressOAuthPrompt = FALSE) {
  ## Notes -----------------------------
  # - Some duplicate IDs are found, possibly due to the list of follower IDs
  #   changing as requests are being made, which could mess up the pagination.
  #   Not a big deal though, as duplicates are about 0.055% of the IDs scraped
  # ====================================
  
  # Determine whether to lookup by userid or screenname
  if (missing(userid)) {
    userid <- character()
    lookup <- 'screen_name'
  }
  if (missing(screenname)) {
    screenname <- character()
    lookup <- 'user_id'
  }
  # Handle improper arguments
  if (length(paste(userid,screenname)) == 0) {
    stop ('Either userid or screenname must be specified')
  } else if (length(userid) > 0 & length(screenname) > 0) {
    stop ('Only one of userid or screenname can be specified')
  } else {
    lookupVal <-  paste0(userid,screenname)
  }
  # Handle writing to <file.name>
  if (!is.character(file.name)) 
    stop ('file.name must be specified as type \'character\'.')
  # Overwrite file if <append = FALSE>
  if (!append) { 
    file <- file(file.name, 'w')
    close(file)
  }
  # Check if authentication is provided via oauth
  if (is.null(oauth) & !suppressOAuthPrompt) {
    continue <- readline(paste('Providing OAuth will allow 60 more API ',
                               'requests per hour (for a total of 210).\n',
                               'Continue without OAuth? (y/n): ', sep=''))
    while (!(continue %in% c('y', 'Y', 'n', 'N'))) {
      continue <- readline('Please enter y or n: ')
    }
    if (continue %in% c('n', 'N')) stop ('User terminated function.')
  } else if (attr(class(oauth), 'package') != 'ROAuth'){
    stop ('oauth must be of class OAuth from the package ROAuth.')
  } else {
    require(ROAuth)
  }
  # Some setup
  params <- list()
  params[['cursor']] <- cursor
  params[[lookup]]   <- lookupVal
  errors <- list()
  end    <- Sys.time() + timeout
  # Main loop for retrieving and writing request results
  while (end > Sys.time() & params$cursor != '0') {
    stdEnd <- Sys.time() + 3600 # NOTE: time may need adjusting
    # Try using APIv1 first - get requests until rate limited, timeout, or done
    while (all(end > Sys.time(), params$cursor != '0')) {
      stdUrl <- 'https://api.twitter.com/1/followers/ids.json'
      get <- stdRequest(stdUrl, params, cainfo)
      if ('error' %in% names(get)) {
        errors = c(errors, get$error)
        break()
      } else {
        file <- file(file.name, 'a')
        write(get$ids, file, sep = ' ')
        close(file)
        params$cursor <- get$next_cursor_str
      }
    } 
    # ...then try APIv1.1 OAuth'ed requests...
    if(!is.null(oauth)) {
      authEnd <- Sys.time() + 900 # note: time may need adjusting
      # ...until APIv1 limit is reset, timeout, or done
      while (all(c(end, stdEnd) > Sys.time(), params$cursor != '0')) {
        authUrl <- 'https://api.twitter.com/1.1/followers/ids.json'
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
          file <- file(file.name, 'a')
          write(get$ids, file, sep = ' ')
          close(file)
          params$cursor <- get$next_cursor_str
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
  return(list(cursor = params$cursor, errors = errors))
}
