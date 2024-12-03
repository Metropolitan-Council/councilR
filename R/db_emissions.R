#' @title Import data table from greenhouse gas emissions scenario planning database
#'
#' @param table_name character, which table to pull.
#' @param db character, database name. Default is `"CD_Emissions"`.
#' @param local logical, whether to pull from the onsite database or Azure.
#'     Default is `TRUE`.
#' @inheritParams fred_connection
#'
#' @details
#'  To access Azure, your IP address must be approved. Contact Sean Molloy.
#'
#' @note See `vignette("Credentials")` to review credential management.
#'     You must be set up with the appropriate database drivers to use these functions.
#'     **Windows** users need ODBC with Microsoft SQL. Contact IS support for ODBC installation.
#'     **Mac** users need `unixodbc` and `freetds`. See instructions in the
#'     [onboarding guide](https://furry-adventure-596f3adb.pages.github.io/database-connections.html)
#'
#'
#' @return Requested table
#' @export
#' @family database functions
#' @rdname emissions
#'
#' @examples \dontrun{
#'
#' library(councilR)
#' library(DBI)
#'
#' # set credentials if you haven't already
#' keyring::key_set_with_value("councilR.uid", "mc\\myuuid")
#' keyring::key_set_with_value("councilR.pwd", "password")
#'
#' # create connection
#' conn <- emissions_connect()
#'
#' # pull table using SQL
#' DBI::dbGetQuery(conn, "SELECT * FROM metro_energy.vw_electricity_residential_ctu")
#'
#' # disconnect
#' DBI::dbDisconnect(conn)
#'
#' # get specific tables
#' t_electricity_residential_ctu <- import_from_emissions(
#'   table_name = "metro_energy.vw_electricity_residential_ctu"
#' )
#'
#' t_eia_energy_consumption_state <- import_from_emissions(
#'   table_name = "state_energy.eia_energy_consumption_state"
#' )
#'
#' t_utility_natural_gas_by_ctu <- import_from_emissions(
#'   table_name = "metro_energy.vw_utility_natural_gas_by_ctu"
#' )
#' }
#'
#' @importFrom DBI dbCanConnect dbConnect
#' @importFrom odbc odbc
#' @importFrom utils osVersion
#' @importFrom purrr map
#' @importFrom cli cli_abort
emissions_connection <- function(
    uid = keyring::key_get("councilR.uid"),
    pwd = keyring::key_get("councilR.pwd"),
    local = TRUE,
    db = "CD_Emissions") {
  # check input types
  purrr::map(
    c(uid, pwd, db),
    check_string
  )
  purrr::map(
    c(local),
    check_bool
  )


  # decide which server to use based on local
  if (local == FALSE) {
    serv <- "sqldb-cdemiss-p-c1-01.database.windows.net"
  } else if (local == TRUE) {
    serv <- "dbsqlcl11t.test.local,65414"
  }

  # decide which driver to use based on OS
  drv <- if (is_mac()) {
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

  return(conn)
}


#' @param table_name character, which table to pull.
#'
#' @return `import_from_emissions()` - Requested table
#' @export
#'
#' @importFrom DBI dbGetQuery dbDisconnect
#' @importFrom purrr map
#' @rdname emissions
import_from_emissions <- function(table_name,
                                  uid = keyring::key_get("councilR.uid"),
                                  pwd = keyring::key_get("councilR.pwd"),
                                  local = TRUE,
                                  db = "CD_Emissions") {
  # check input types
  purrr::map(
    c(table_name, uid, pwd, db),
    check_string
  )
  purrr::map(
    c(local),
    check_bool
  )


  conn <- emissions_connection(
    uid = uid,
    pwd = pwd,
    local = local,
    db = db
  )

  db_sp_table <- DBI::dbGetQuery(
    conn,
    paste0("SELECT * FROM ", table_name)
  )

  DBI::dbDisconnect(conn)

  return(db_sp_table)
}
