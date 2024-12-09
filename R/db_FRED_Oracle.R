#' @title FRED-Oracle
#'
#' @description
#' Functions for interacting with FRED-Oracle.
#'
#' @details
#' Both "FRED" and "fred" capitalization are supported.
#'
#'  - `fred_oracle_connection()` creates an connection to FRED-Oracle.
#'     This function will *not* automatically close the connection, so take
#'     care to use [DBI::dbDisconnect()] once you are done.
#'  - `import_from_fred_oracle()` imports a given table from FRED-Oracle. The connection will
#'     be automatically closed after the table is imported.
#'
#' @note See `vignette("Credentials")` to review credential management.
#'
#'  FRED-Oracle requires its own user id and password, separate from your Met Council
#'  credentials. Contact Matt Schroeder for assistance.
#'
#'  You must be set up with the appropriate database drivers to use these functions.
#'
#'  **Windows** users need ODBC with Oracle drivers. Contact IS support for ODBC installation.
#'
#'  **Mac** users need `unixodbc` and `freetds`, plus Java and JDBC drivers. Additionally,
#'  you must have set `FREDOracle.url` in your keyring and `JDBC_HOME` in your .Renviron.
#'  See instructions in the
#'  [onboarding guide](http://mtapshiny1p/MT/Strategic_Initiatives/Onboarding/rodbc.html#configuring-oracle-connections---use-rjdbc-on-any-platform).
#'
#'  Further examples can be found in `vignette("Databases")`.
#'
#' @rdname fred-oracle
#' @family database functions
#'
#' @param uid character, FRED-Oracle user id. Default value is
#'    `keyring::key_get("FREDOracle.uid")`,
#' @param pwd character, FRED-Oracle password. Default value is
#'    `keyring::key_get("FREDOracle.pwd")`.
#' @param url character, FRED-Oracle URL. Default value is
#'     `keyring::key_get("FREDOracle.url")`.
#' @param dsn character, FRED-Oracle data source name. Default value is
#'     `keyring::key_get("FREDOracle.dsn")`.
#'
#' @return `fred_oracle_connection()` - An ODBC or JDBC connection object
#' @export
#'
#' @examples
#' \dontrun{
#' library(councilR)
#' library(DBI)
#'
#' # create connection
#' conn <- fred_oracle_connection()
#'
#' # pull table using SQL
#' DBI::dbGetQuery(conn, "SELECT * FROM RESEARCH_WEB.RES_PERMIT_TYPE")
#'
#' # disconnect
#' DBI::dbDisconnect(conn)
#'
#' # import a specific table, with no additional SQL logic
#' import_from_FRED(table_name = "RESEARCH_WEB.RES_PERMIT_TYPE")
#' }
#'
#' @importFrom DBI dbCanConnect dbGetQuery dbConnect dbDisconnect
#' @importFrom odbc odbc
#' @importFrom utils osVersion
#' @importFrom purrr map
#' @importFrom cli cli_abort
FRED_oracle_connection <- function(
    uid = keyring::key_get("FREDOracle.uid"),
    pwd = keyring::key_get("FREDOracle.pwd"),
    url = keyring::key_get("FREDOracle.url"),
    dsn = keyring::key_get("FREDOracle.dsn")) {
  # check input types
  purrr::map(
    c(uid, pwd, url, dsn),
    check_string
  )


  # decide which driver to use based on OS
  if (is_mac()) {
    requires_pkg("rJava")
    requires_pkg("RJDBC")
    drv <- RJDBC::JDBC(
      driverClass = "java.sql.Driver",
      classPath = file.path(Sys.getenv("JDBC_HOME"), "ojdbc17.jar")
    )
  } else {
    drv <- odbc::odbc()
  }

  # check that DB connection works
  if (class(drv) == "JDBCDriver") {
    if (
      DBI::dbCanConnect(
        drv = drv,
        url = url,
        user = uid,
        password = pwd
      )
      == FALSE) {
      cli::cli_abort("Database failed to connect")
    }
  } else if (class(drv) == "OdbcDriver") {
    if (
      DBI::dbCanConnect(
        dsn = dsn,
        drv = drv,
        user = uid,
        pwd = pwd
      ) == FALSE) {
      cli::cli_abort("Database failed to connect")
    }
  }

  # create and return connection object
  conn <-
    if (class(drv) == "JDBCDriver") {
      DBI::dbConnect(
        drv = drv,
        url = url,
        user = uid,
        password = pwd
      )
    } else if (class(drv) == "OdbcDriver") {
      DBI::dbConnect(
        dsn = dsn,
        drv = drv,
        user = uid,
        pwd = pwd
      )
    }

  # return connection
  return(conn)
}

#' @rdname fred-oracle
#' @export
fred_oracle_connection <- FRED_oracle_connection

#'
#' @param table_name character, which table to pull.
#'
#' @return `import_from_fred_oracle()` - Requested table
#' @export
#'
#' @importFrom DBI dbGetQuery dbDisconnect
#' @rdname fred-oracle
import_from_FRED_oracle <- function(table_name,
                                    uid = keyring::key_get("FREDOracle.uid"),
                                    pwd = keyring::key_get("FREDOracle.pwd"),
                                    url = keyring::key_get("FREDOracle.url"),
                                    dsn = keyring::key_get("FREDOracle.dsn")) {
  # check input types
  purrr::map(
    c(table_name, uid, pwd, url, dsn),
    check_string
  )

  conn <- fred_oracle_connection(
    uid = uid,
    pwd = pwd,
    url = url,
    dsn = dsn
  )

  db_sp_table <- DBI::dbGetQuery(
    conn,
    paste0("SELECT * FROM ", table_name)
  )

  DBI::dbDisconnect(conn)

  return(db_sp_table)
}



#' @rdname fred-oracle
#' @export
import_from_fred_oracle <- import_from_FRED_oracle
