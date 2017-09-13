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
#' @param filename the location of the database config file. Should contain the scheme,
#' host, port, database (if applicable), classname, jars, and user. Password will be
#' prompted.
#' @param instance if the config file has multiple instances in it (e.g. production
#' and development), specific which instance to connect to.
#' @param env.passname the name of the environment variable that is storing the
#' obscured password. Create this using \code{\link{set_env_pass}}
#'
#' @usage redhat_db_conn(filename, instance, env.passname)
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
redhat_db_conn = function(filename, instance = NULL, env.passname = NULL) {
  if (!grepl("/", filename)) filename = slash_paste(normalizePath("~") , '/', filename)
  # Read in the JSON file
  db.cfg <- read_json(filename)
  if (!is.null(instance)) db.cfg <- db.cfg[[instance]]
  db.cfg <- data.frame(db.cfg, stringsAsFactors = FALSE)
  # If there isn't a specific database to connect to, make it NA, and get a
  # password if not in the config file
  if (is.null(db.cfg$scheme)) db.cfg$scheme <- NA
  if (is.null(db.cfg$database)) db.cfg$database <- NA
  if (is.null(db.cfg$user)) db.cfg$user <- Sys.getenv("USER")
  if (is.null(db.cfg$password)) {
    if (!is.null(env.passname)) {
      db.cfg$password <- rawToChar(hex2bin(Sys.getenv(env.passname)))
    } else {
      db.cfg$password <- getPass(msg="Enter the Database Password:")
    }
  }
  # If it's VDM, CEE Redshift or bugzilla (a JDBC connection)
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
  # If it's sales redshift (a postresql connection)
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

#' Read SQL scripts from a directory
#'
#' Read SQL scripts from a directory into R for use against a database.
#'
#' @import stringr
#' @param base_path the directory holding the sql files
#' @param sqlvars a list containing all variables you wish to set within your queries
#' @param prefix a prefix from your SQL filenames if you want to remove before
#' creating the data frames
#' @param pattern a suffix to include in the data frame names
#'
#' @usage read_scripts(base_path, sqlvars, prefix, pattern)
#'
#' @details
#' Just place all scripts in the same directory with a .sql extension, and provide
#' the directory path. It's also possible to set any variables in your query
#' with the format of ${variable}.
#'
#' @return
#' sql.list returns a list of SQL queries with the future filename as the name
#'
#' @export
read_scripts = function(base_path, sqlvars = list(), prefix = 'get_', pattern = '.df') {
  list2env(sqlvars, envir = .GlobalEnv)
  filelist <- list.files(base_path, pattern = "*.sql", ignore.case = TRUE, full.names = TRUE)
  filenames <- str_split_fixed(filelist, "//", n=2)[,2]
  filenames <- str_split_fixed(gsub(prefix, '', filenames), "[.]", n=2)[,1]
  dfnames <- paste(filenames, pattern, sep='')
  sql.list <- lapply(filelist, function(x) str_interp(paste(readLines(x, warn=F), collapse="\n")))
  names(sql.list) <- dfnames
  print("SQL query data imported from directory.")
  return(sql.list)
}

#' Run SQL scripts on a database
#'
#' Run SQL scripts that are stored in a list on a database.
#'
#' @import DBI
#' @param conn the database connection created using \code{\link{redhat_db_conn}}
#' @param sql.list a list of queries created using \code{\link{read_scripts}}
#'
#' @usage run_scripts(conn, sql.list)
#'
#' @details
#' After running \code{\link{redhat_db_conn}} and \code{\link{read_scripts}},
#' use these parameters to read the database query data into R as data frames.
#'
#' @return
#' nothing is returned. The output from the scripts will be added to your environment.
#'
#' @export
run_scripts = function(conn, sql.list) {
  df.list = list()
  for (query in names(sql.list)) {
    df.list[[query]] <- dbGetQuery(conn, statement = sql.list[[query]])
  }
  list2env(df.list, envir = .GlobalEnv)
}


#' Write a bunch of data frames to csv
#'
#' Write data frames matching a pattern to separate csv files.
#'
#' @import utils
#' @param outdir the directory to drop the dataframes
#' @param pattern a pattern for dataframes to output. The default is to
#' include all data with the suffix ".df". Works well with \code{\link{run_scripts}}.
#'
#' @usage dfs_to_csv(outdir, pattern)
#'
#' @details
#' Write data frames matching a pattern to separate csv files. Will
#' write all matching the pattern of '.df' by default.
#'
#' @return
#' nothing is returned. The output will be csv files.
#'
#' @export
dfs_to_csv = function(outdir, pattern='.df') {
  setwd(outdir)
  suffix <- paste0('_', gsub('-', '', Sys.Date()), '.csv')
  output_dfs <- as.list(ls(envir=.GlobalEnv, pattern=pattern))
  lapply(output_dfs, function(x) write.csv(eval(as.name(x)), file=paste0(gsub(pattern, '', x), suffix)))
  print('csv files were created.')
}
