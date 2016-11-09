#' Get the red hat quarter for a given date, "as.yearmon", or a date-like string
#'
#' @import zoo
#' @import lubridate
#' @param x The date-type value to be converted. Either a date, date-type
#'   string like '2016-04-25', or a yearmon using the zoo::as.yearmon()
#'   function.
#'
#' @examples
#' x <- redhat_qtr("2016-04-25")
#' y <- as.Date("2016-04-12")
#' y <- redhat_qtr(y)
#' z <- zoo::as.yearmon("2016-01-01")
#' z <- redhat_qtr(z)
#' @export
redhat_qtr = function(x) {
  tmp.qtrs <- format(as.yearqtr(as.Date(as.yearmon(x)) + months(10)), "Q%qFY%y")
  return(tmp.qtrs)
}

#' Get the current red hat quarter for today
#'
#'
#' @import zoo
#' @import lubridate
#' @examples
#' x <- current_redhat_qtr()
#' @export
current_redhat_qtr = function() {
  return(redhat_qtr(Sys.Date()))
}

#' Get the red hat quarter previous to the current quarter
#'
#'
#' @import zoo
#' @import lubridate
#' @examples
#' x <- previous_redhat_qtr()
#' @export
previous_redhat_qtr = function() {
  tmp.qtrs <- format(as.yearqtr(as.Date(as.yearmon(Sys.Date())) + months(7)), "Q%qFY%y")
  return(tmp.qtrs)
}
