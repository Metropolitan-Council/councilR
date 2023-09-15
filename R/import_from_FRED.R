#' @title Import data table from FRED (main CD Research database) data stored
#'  Azure SQL Server
#'
#' @param table_name character, which table to pull.
#' @param uid character, your network ID.
#'     Default is `getOption("councilR.uid")`.
#' @param pwd character, your network password.
#'     Default is `getOption("councilR.pwd")`. For example, `"mypwd"`
#' @param db character, database name. Default is `"CD_RESEARCH_WEB"`.
#' @param prod logical, whether to pull from the test or production db.
#'     Default is `TRUE`.
#'
#'
#' @note See `vignette("Options")` to review package options.
#'     You must be set up with the appropriate database drivers to use this function.
#'     **Windows** users need ODBC with Microsoft SQL. Contact IS support for ODBC installation.
#'     **Mac** users need `unixodbc`, `freetds`, and properly configured `odbc.ini`.
#'     See instructions in the
#'     [onboarding guide](https://furry-adventure-596f3adb.pages.github.io/database-connections.html)
#'     and contact package maintainer for assistance.
#'
#'     This function relies on `[{rlang}]` internal functions.
#'
#' @return Requested table
#' @export
#'
#' @examples
#' \dontrun{
#' library(councilR)
#'
#' # set options if you haven't already
#' options(
#'   councilR.uid = "mc\\you",
#'   councilR.pwd = "mypwd"
#' )
#'
#' import_from_FRED(table_name = "GQ_UNIT", prod = FALSE)
#' }
#' @importFrom DBI dbCanConnect dbGetQuery dbConnect dbDisconnect
#' @importFrom odbc odbc
#' @importFrom utils osVersion
#' @importFrom purrr map
import_from_FRED <- function(table_name,
                             uid = getOption("councilR.uid"),
                             pwd = getOption("councilR.pwd"),
                             db = "CD_RESEARCH_WEB",
                             prod = TRUE) {
  # check input types
  purrr::map(
    c(table_name, uid, pwd, db),
    rlang:::check_string
  )
  purrr::map(
    c(prod),
    rlang:::check_bool
  )

  # decide which server to use based on local
  serv <- if (prod == FALSE) {
    "azdbsqlcl11t.test.local"
  } else if (prod == TRUE) {
    "azdbsqlcl11.mc.local"
  }

  # decide which driver to use based on OS
  drv <- if (grepl("mac", osVersion)) {
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
      stop("Database failed to connect")
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
      stop("Database failed to connect")
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

  db_sp_table <- DBI::dbGetQuery(
    conn,
    paste0("SELECT * FROM ", table_name)
  )

  DBI::dbDisconnect(conn)

  return(db_sp_table)
}
