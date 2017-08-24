#' Connect to a database in R
#'
#' Establish a connection to a JDBC or PostgreSQL database, and return that value
#' in order to query the database.
#'
#' @import jsonlite
#' @import urltools
#' @import getPass
#' @import sodium
#' @import DBI
#' @import RJDBC
#' @import RPostgres
#' @param filename the location of the database config file. Should contain the scheme,
#' host, port, database (if applicable), classname, jars, and user. Password will be
#' prompted.
#' @param env.passname the name of the environment variable that is storing the
#' obscured password.
#'
#' @usage redhat_db_conn(filename, env.passname)
#'
#' @details
#' This function requires the user to have access to the database, have downloaded
#' the driver, and set up the certificates on their computer. The process for using
#' the package is to create a config file, then pass it along with the path:
#'
#' \code{vdm.conn <- redhat_db_conn("~/.vdm.cfg")}
#'
#' \code{sales.conn <- redhat_db_conn("~/.sales.cfg")}
#'
#' Then, the created connection can be used with the DBI package:
#'
#' \code{DBI::dbGetQuery(vdm.conn, "SELECT id FROM APL_CEE_VDM.stg_gss_case LIMIT 10")}
#'
#' \code{DBI::dbGetQuery(sales.conn, "SELECT unique_identifier FROM
#' rsds_ops_clv.customer_timeline_oracle_18q1 WHERE unique_identifier != '' LIMIT 10")}
#'
#' @return
#' redhat_db_conn returns a database connection that can be used for SQL queries
#'
#' @export
redhat_db_conn = function(filename, env.passname = NULL) {

  # Read in the JSON file
  db.cfg <- data.frame(read_json(filename), stringsAsFactors = FALSE)
  # If there isn't a specific database to connect to, make it NA, and get a
  # password if not in the config file
  if (is.null(db.cfg$database)) db.cfg$database <- NA
  if (is.null(db.cfg$user)) db.cfg$user <- Sys.getenv("USER")
  if (is.null(db.cfg$password)) {
    if (!is.null(env.passname)) {
      db.cfg$password <- rawToChar(hex2bin(Sys.getenv(env.passname)))
    } else {
      db.cfg$password <- getPass(msg="Enter the Database Password:")
    }
  }
  # If it's VDM or bugzilla (a JDBC connection)
  if (length(grep("jdbc", db.cfg$classname)) > 0) {
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
  # If it's redshift (a postresql connection)
  } else {
    # Create the postgresql connection
    conn <- dbConnect(RPostgres::Postgres(), dbname = db.cfg$database,
                      host = db.cfg$host, port = db.cfg$port, sslmode = 'require',
                      user = db.cfg$user, password = db.cfg$password)
  }
  return(conn)
}


#' Create an obscured password to keep in as an environment variable
#'
#' @description
#' This function allows you to enter your password, and then stores a slightly
#' obscured version as an environment variable
#'
#' @import getPass
#' @import sodium
#' @param passname the name of the password (and therefore environment variable) to
#' be stored as an R environment variable.
#'
#' @usage set_env_pass(passname)
#'
#' @details
#' Since passwords are annoying to enter, and you also don't want them stored as-is,
#' even in an environment variable file, you can at least mask it a little bit. These
#' will be saved in a place that only R uses, so if you use python you'll have to do
#' that separately.
#'
#' @export
set_env_pass = function(passname) {
  tmp.pass <- bin2hex(charToRaw(getPass(msg=paste0('Enter "', passname, '" Password:'))))
  tmp.line <- paste0(passname, '="', tmp.pass, '"')
  write(tmp.line, file="~/.Renviron", append=TRUE)
  readRenviron("~/.Renviron")
}
