

guess_date_format <- function(x) NULL


#' @export
timestamp_date <- function(date){
  #substr(anytime(date),1,10)
  substr(as.POSIXct(date, origin="1970-01-01"),1,10)
}

