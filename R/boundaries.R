#' Get get the start date of the current Red Hat fiscal year, or of a specific date.
#'
#' @import zoo
#' @import lubridate
#' @param x The date for which to calculate the beginning of the fiscal year.
#'
#'
#' @examples
#' x <- year_start_date("2016-04-25")
#' y <- year_start_date(Sys.Date())
#' @export
year_start_date = function(x) {
  return(as.Date(as.yearqtr(as.Date(as.yearmon(x)) + months(10))) - months(10) - years(1))
}

#' Get get the end date of the current Red Hat fiscal year, or of a specific date.
#'
#' @import zoo
#' @import lubridate
#' @param x The date for which to calculate the end of the fiscal year.
#'
#'
#' @examples
#' x <- year_end_date("2016-04-25")
#' y <- year_end_date(Sys.Date())
#' @export
year_end_date = function(x) {
  return(as.Date(as.yearqtr(as.Date(as.yearmon(x)) + months(10))) - months(10) - days(1))
}
