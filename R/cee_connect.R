#' Connect to a database in R
#'
#' @import jsonlite
#' @import urltools
#' @import getPass
#' @import RJDBC
#' @param filename the location of the database config file. Should contain the scheme,
#' host, port, database (if applicable), classname, jars, and user. Password will be
#' prompted.
#'
#' @details
#' This function requires the user to have access to the database, have downloaded
#' the driver, and set up the certificates on their computer. The process for using
#' the package is to create a config file, then pass it along with the path:
#'
#' \code{vdm.conn <- redhat_db_conn("~/vdm.cfg").}
#'
#' Then, the created connection can be used with the DBI package:
#'
#' \code{DBI::dbGetQuery(vdm.conn, "SELECT * FROM APL_CEE_VDM.stg_gss_case LIMIT 10")}
#'
#' @export
redhat_db_conn = function(filename) {
  # A function that takes the config filename, establishes a database connection,
  # then returns the connection.

  # Read in the JSON file
  db.cfg <- data.frame(read_json(filename), stringsAsFactors = FALSE)
  # If there isn't a specific database to connect to, make it NA, and get a
  # password if not in the config file
  if(is.null(db.cfg$database)) { db.cfg$database <- NA }
  if(is.null(db.cfg$password)) { db.cfg$password <- getPass(msg="Enter the Database Password:") }

  # Create a JDBC url from the components
  db.url <- url_compose(data.frame(scheme = db.cfg$scheme,
                                   domain = db.cfg$host, port = db.cfg$port,
                                   path = db.cfg$database, parameter = NA, fragment = NA))

  # Create the JDBC driver
  drv <- JDBC(driverClass = db.cfg$classname,
              classPath = db.cfg$jars,
              identifier.quote="`")

  # Create the connection
  conn <- dbConnect(drv, db.url, user=db.cfg$user, password=db.cfg$password)
  return(conn)
}
