#' @title Import data table from greenhouse gas emissions scenario planning database
#'
#' @param table_name character, which table to pull.
#' @param uid character, your network ID.
#'     Default is `getOption("councilR.uid")`.
#' @param pwd character, your network password.
#'     Default is `getOption("councilR.pwd")`. For example, `"my_password"`
#' @param db character, database name. Default is `"CD_RESEARCH_WEB"`.
#' @param prod logical, whether to pull from the test or production db.
#'     Default is `TRUE`.
#'
#'
#' @note See `vignette("Options")` to review package options.
#'     You must be set up with the appropriate database drivers to use this function.
#'     **Windows** users need ODBC with Microsoft SQL. Contact IS support for ODBC installation.
#'     **Mac** users need `unixodbc` and `freetds`. See instructions in the
#'     [onboarding guide](https://furry-adventure-596f3adb.pages.github.io/database-connections.html)
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
#' }
#' @importFrom DBI dbCanConnect dbGetQuery dbConnect dbDisconnect
#' @importFrom odbc odbc
#' @importFrom utils osVersion
#' @importFrom purrr map
import_from_FRED <- function(table_name,
                                  uid = getOption("councilR.uid"),
                                  pwd = getOption("councilR.pwd"),
                                  local = TRUE,
                                  db = "CD_RESEARCH_WEB",
                                  prod = TRUE) {
  # check input types
  purrr::map(
    c(table_name, uid, pwd, db),
    rlang:::check_string
  )
  purrr::map(
    c(local),
    rlang:::check_bool
  )


  # decide which server to use based on local
  if (prod == FALSE) {
    serv <- "azdbsqlcl11t.test.local"
  } else if (prod == TRUE) {
    serv <- "azdbsqlcl11.mc.local"
  }

  # decide which driver to use based on OS
  drv <- if (grepl("mac", osVersion)) {
    "FreeTDS"
  } else {
    "SQL Server"
  }

  # check that DB connection works
  if (drv == "FreeTDS") {
    if (
      DBI::dbCanConnect(
        odbc::odbc(),
        Driver = drv,
        Database = db,
        Uid = uid,
        Pwd = pwd,
        Server = serv
      ) == FALSE) {
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
        Driver = drv,
        Database = db,
        Uid = uid,
        Pwd = pwd,
        Server = serv
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
