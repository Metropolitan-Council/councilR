#' @title FRED
#'
#' @description
#' Functions for interacting with FRED, the main CD Research database, stored
#'  Azure SQL Server
#'
#' @details
#' Both "FRED" and "fred" capitalization are supported.
#'
#'  - `fred_connection()` creates an S4 Microsoft SQL Server object for FRED.
#'     This function will *not* automatically close the connection, so take
#'     care to use [DBI::dbDisconnect()] once you are done.
#'  - `import_from_fred()` imports a given table from FRED. The connection will
#'     be automatically closed after the table is imported.
#'
#' @note See `vignette("Options")` to review package options.
#'
#'  You must be set up with the appropriate database drivers to use these functions.
#'
#'  **Windows** users need ODBC with Microsoft SQL. Contact IS support for ODBC installation.
#'
#'  **Mac** users need `unixodbc`, `freetds`, and properly configured `odbc.ini`.
#'
#'  See instructions in the
#'  [onboarding guide](http://mtapshiny1p/MT/Strategic_Initiatives/Onboarding/rodbc.html).
#'  Further examples can be found in `vignette("Databases")`.
#'
#' @rdname fred
#' @family database functions
#'
#' @param uid character, your network ID.
#'     Default is `getOption("councilR.uid")`.
#' @param pwd character, your network password.
#'     Default is `getOption("councilR.pwd")`. For example, `"mypwd"`
#' @param db character, database name. Default is `"CD_RESEARCH_WEB"`.
#' @param prod logical, whether to pull from the test or production db.
#'     Default is `TRUE`.
#'
#' @return `fred_connection()` - A S4 Microsoft SQL Server object
#' @export
#'
#' @examples
#' \dontrun{
#' library(councilR)
#' library(DBI)
#'
#' # set options if you haven't already
#' options(
#'   councilR.uid = "mc\\you",
#'   councilR.pwd = "mypwd"
#' )
#' # create connection
#' conn <- FRED_connection(prod = FALSE)
#'
#' # pull table using SQL
#' DBI::dbGetQuery(conn, "SELECT * FROM GQ_UNIT WHERE UNIT_ZIP = 55104")
#'
#' # disconnect
#' DBI::dbDisconnect(conn)
#'
#' # import a specific table, with no additional SQL logic
#' import_from_FRED(table_name = "GQ_UNIT", prod = FALSE)
#' }
#'
#' @importFrom DBI dbCanConnect dbGetQuery dbConnect dbDisconnect
#' @importFrom odbc odbc
#' @importFrom utils osVersion
#' @importFrom purrr map
#' @importFrom cli cli_abort
FRED_connection <- function(
    uid = getOption("councilR.uid"),
    pwd = getOption("councilR.pwd"),
    db = "CD_RESEARCH_WEB",
    prod = TRUE) {
  # check input types
  purrr::map(
    c(uid, pwd, db),
    check_string
  )
  purrr::map(
    c(prod),
    check_bool
  )

  # decide which server to use based on local
  serv <- if (prod == FALSE) {
    "azdbsqlcl11t.test.local"
  } else if (prod == TRUE) {
    "azdbsqlcl11.mc.local"
  }

  # decide which driver to use based on OS
  drv <- if (is_mac()) {
    "FreeTDS"
  } else {
    "SQL Server"
  }

  # if on Mac, you need  to specify which database name
  # based on prod
  db_name <- if (prod) {
    "CD_RESEARCH_WEB_PROD"
  } else {
    "CD_RESEARCH_WEB_TEST"
  }

  # check that DB connection works
  if (drv == "FreeTDS") {
    if (
      DBI::dbCanConnect(
        odbc::odbc(),
        DSN = db_name,
        Uid = uid,
        Pwd = pwd
      )
      == FALSE) {
      cli::cli_abort("Database failed to connect")
    }
  } else if (drv == "SQL Server") {
    if (
      DBI::dbCanConnect(
        odbc::odbc(),
        Driver = drv,
        Database = db,
        Uid = uid,
        Pwd = pwd,
        Server = serv,
        Trusted_Connection = "yes"
      ) == FALSE) {
      cli::cli_abort("Database failed to connect")
    }
  }



  conn <-
    if (drv == "FreeTDS") {
      DBI::dbConnect(
        odbc::odbc(),
        DSN = db_name,
        Uid = uid,
        Pwd = pwd
      )
    } else if (drv == "SQL Server") {
      DBI::dbConnect(
        odbc::odbc(),
        Driver = drv,
        Database = db,
        Uid = uid,
        Pwd = pwd,
        Server = serv,
        Trusted_Connection = "yes"
      )
    }

  # return connection
  return(conn)
}

#' @rdname fred
#' @export
fred_connection <- FRED_connection

#'
#' @param table_name character, which table to pull.
#'
#' @return `import_from_FRED()` - Requested table
#' @export
#'
#' @importFrom DBI dbGetQuery dbDisconnect
#' @rdname fred
import_from_FRED <- function(table_name,
                             uid = getOption("councilR.uid"),
                             pwd = getOption("councilR.pwd"),
                             db = "CD_RESEARCH_WEB",
                             prod = TRUE) {
  # check input types
  purrr::map(
    c(table_name, uid, pwd, db),
    check_string
  )
  purrr::map(
    c(prod),
    check_bool
  )

  conn <- fred_connection(
    uid = uid,
    pwd = pwd,
    db = db,
    prod = prod
  )

  db_sp_table <- DBI::dbGetQuery(
    conn,
    paste0("SELECT * FROM ", table_name)
  )

  DBI::dbDisconnect(conn)

  return(db_sp_table)
}



#' @rdname fred
#' @export
import_from_fred <- import_from_FRED
