matchPostalCode <-
function(str){
    return(grepl('^\\d{5}([\\-]?\\d{4})?$',str, perl = TRUE))
}
