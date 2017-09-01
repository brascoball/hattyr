#' Get a Red Hat quarter for a given date or vector of dates
#'
#' @import zoo
#' @import lubridate
#' @param x The date-type value to be converted. Either a date, date-type
#'   string like '2016-04-25', or a yearmon using the zoo::as.yearmon()
#'   function.
#' @param previous A logical scalar. Do you want the previous Red Hat quarter?
#' @param old A logical scalar. Do you want the old Q\%qFY\%y format?
#'
#' @details
#' The function can take muliple date types (see examples), and returns a Red Hat
#' quarter.
#'
#' @return
#' Returns a Red Hat quarter or vector or quarters.
#' If \code{old = TRUE}, then returns the Red Hat quarter in the old Q\%qFY\%y format.
#'
#' @examples
#' redhatqtr()
#' redhatqtr("2016-04-25")
#' y <- as.Date("2016-04-12")
#' redhatqtr(y)
#' z <- zoo::as.yearmon("2016-01-01")
#' redhatqtr(z)
#' redhatqtr(previous = TRUE)
#' redhatqtr(old = TRUE)
#' @export
redhatqtr = function(x = Sys.Date(), previous = FALSE, old = FALSE) {
  tmp.fmt = "FY%yQ%q"
  if (old) tmp.fmt = "Q%qFY%y"
  tmp.mon = 10
  if (previous) tmp.mon = 7
  tmp.qtrs <- format(as.yearqtr(as.Date(as.yearmon(x)) + months(tmp.mon)), tmp.fmt)
  return(tmp.qtrs)
}

#' Old function to get the Red Hat dates
#'
#' @import zoo
#' @import lubridate
#' @param x The date-type value to be converted. Either a date, date-type
#'   string like '2016-04-25', or a yearmon using the zoo::as.yearmon()
#'   function.
#' @export
redhat_qtr = function(x = Sys.Date()) {
  return(redhatqtr(x, old = TRUE))
}

#' Get the first date of a quarter based on a Red Hat quarter
#'
#' @import zoo
#' @import lubridate
#' @param x The red hat quarter to be converted. Either the old format like 'Q1FY17',
#'   or the new format like 'FY17Q1'.
#'
#' @examples
#' redhatqtr_start()
#' redhatqtr_start("FY17Q1")
#' redhatqtr_start("Q1FY17")
#' @export
redhatqtr_start = function(x = redhatqtr(Sys.Date())) {
  # if old qtr year style
  if (grepl("Q\\dFY\\d\\d", x[1])) tmp.fmt = 'Q%qFY%y'
  # if new qtr year style
  if (grepl("FY\\d\\dQ\\d", x[1])) tmp.fmt = 'FY%yQ%q'
  tmp.date <- as.Date(as.yearqtr(x, tmp.fmt)) - months(10)
  return(tmp.date)
}

#' Get a start and end dates for Red Hat quarters
#' @description
#' Get a data frame of start dates and end dates for previous quarters,
#' or the start and end dates for the current or provided Red Hat quarter.
#'
#'
#' @import zoo
#' @import lubridate
#' @param x The date-type value to determine quarters.  Either a date, yearmon,
#'   date-type string like '2016-04-25', year like '2017', an old-style qtr year
#'   like 'Q1FY18' or a new-style year qtr like 'FY18Q1'
#' @param n An integer. How many previous quarters?
#' @param current A logical scalar. Do you want just the current start and end?
#'
#' @details
#' The function can take muliple date types (see examples), and produces a data frame
#' with the previous four quarters. It can provide as many quarters as needed. It can
#' also just provide a vector of the start date and end date of the current quarter.
#'
#' @return
#' Returns a data frame with start date, end date, new quarter format, and old quarter format.
#' If \code{current = TRUE}, then returns a vector of start and end dates
#'
#' @examples
#' redhatqtr_dates()
#' redhatqtr_dates("2016-04-25")
#' redhatqtr_dates(as.Date("2016-04-12"))
#' redhatqtr_dates(zoo::as.yearmon("2016-04-05"))
#' redhatqtr_dates(n = 8)
#' redhatqtr_dates('2017', n = 12)
#' redhatqtr_dates('Q1FY18')
#' redhatqtr_dates('FY18Q1')
#' redhatqtr_dates('FY18Q1', current = TRUE)
#' @export
redhatqtr_dates = function(x = Sys.Date(), n = 4, current = FALSE) {
  # First, convert any date information to a red hat quarter format
  # if just the year
  if (grepl("^\\d{4}$", x)) curr.start = paste0('FY', as.integer(x)-1999, 'Q1')
  # if old or new qtr year style
  if (grepl("^Q\\dFY\\d\\d|FY\\d\\dQ\\d$", x)) curr.start = x
  # if a date
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) curr.start = redhatqtr(x)
  # if a yearmon
  if (class(x) == 'yearmon') curr.start = redhatqtr(x)
  # convert to the start date of the redhat quarter
  curr.start = redhatqtr_start(curr.start)
  if (current) {
    curr.end <- curr.start + months(3) - days(1)
    return(c(curr.start, curr.end))
  } else {
    qtr_start <- as.Date(1:n, origin=Sys.Date())
    qtr_end <- as.Date(1:n, origin=Sys.Date())
    for (i in 1:n) {
      qtr_start[i] <- curr.start - months(3*i)
      qtr_end[i] <- curr.start - months(3*(i-1)) - days(1)
    }
    dates_list <- data.frame(qtr_start, qtr_end)
    dates_list <- dates_list[order(dates_list$qtr_start),]
    rownames(dates_list) <- NULL
    dates_list$qtr_name <- redhatqtr(dates_list$qtr_start)
    dates_list$old_qtr_name <- redhatqtr(dates_list$qtr_start, old = TRUE)
    return(dates_list)
  }
}

