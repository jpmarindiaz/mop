
#' @export
which_in <- function(x,y) x[x %in% y]

#' @export
which_not_in <- function(x,y) x[!x %in% y]

#' @export
remove_na <- function(x) x[!is.na(x)]


