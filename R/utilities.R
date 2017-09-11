#' Determine the most recently saved file
#'
#' With many similar filenames, find the most recently saved version
#'
#' @param directory the location of the files in question.
#' @param reg_ex the regular expression to search for in the directory
#'
#' @usage recent_file(directory, reg_ex)
#'
#' @details
#' This function asks for a directory and a wildcard regular expression in order to
#' search a directory for the most recently saved file. This is helpful if you want
#' to keep historical versions of csv files or data files in the same place, but
#' want to use the most recent in your calculations.
#'
#' \code{vdm.conn <- redhat_db_conn("~/Documents/R_code/", "output")}
#'
#' @return
#' recent_file returns a filename (including path) that can be then read into R
#'
#' @export
recent_file <- function(directory, reg_ex) {
  setwd(directory)
  filename <- file.info(list.files(pattern = reg_ex))
  filename <- rownames(filename)[order(filename$mtime)][nrow(filename)]
  filename <- paste0(directory, filename)
  return(filename)
}
