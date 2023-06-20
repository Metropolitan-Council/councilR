#' @title Import data table from greenhouse gas emissions scenario planning database
#'
#' @param table_name character, which table to pull.
#' @param uid character, your network ID.
#'     Default is `getOption("councilR.uid")`. If you are accessing Azure,
#'     this value should be your full email address.
#' @param pwd character, your network password.
#'     Default is `getOption("councilR.pwd")`. For example, `"my_password"`
#' @param db character, database name. Default is `"CD_Emissions"`.
#' @param local logical, whether to pull from the onsite database or Azure.
#'     Default is `TRUE`.
#'
#' @details
#'  To access Azure, your IP address must be approved. Contact Sean Molloy.
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
#' t_electricity_residential_ctu <- import_from_emissions(
#'   table_name = "metro_energy.vw_electricity_residential_ctu"
#' )
#' t_eia_energy_consumption_state <- import_from_emissions(
#'   table_name = "state_energy.eia_energy_consumption_state"
#' )
# t_utility_natural_gas_by_ctu <- import_from_emissions(
#'   table_name = "metro_energy.vw_utility_natural_gas_by_ctu")
#' }
#' @importFrom DBI dbCanConnect dbGetQuery dbConnect dbDisconnect
#' @importFrom odbc odbc
#' @importFrom utils osVersion
#' @importFrom purrr map
import_from_emissions <- function(table_name,
                                  uid = getOption("councilR.uid"),
                                  pwd = getOption("councilR.pwd"),
                                  local = TRUE,
                                  db = "CD_Emissions") {
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
  if (local == FALSE) {
    serv <- "sqldb-cdemiss-p-c1-01.database.windows.net"
  } else if (local == TRUE) {
    serv <- "dbsqlcl11t.test.local,65414"
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
