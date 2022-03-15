#' @title Import data table from greenhouse gas emissions scenario planning database
#'
#'
#' @param table_name character, which table to pull.
#' @param uid character, your network id.
#'     Default is `getOption("councilR.uid")`. For example, `"mc\\rotenle"`
#' @param pwd character, your network password.
#'     Default is `getOption("councilR.pwd")`. For example, `"my_password"`
#' @param serv character, database server.
#'     Default is `"dbsqlcl11t.test.local,65414"` (the test database).
#' @param db character, database name. Default is `"CD_Emissions"`
#' @param local logical, whether to pull from the onsite database or Azure.
#'
#' @description WARNING: Function error may results in RStudio crash.
#'     Requires a password for access to the database.
#'
#' @note See `vignette("Options")` to review package options.
#'     You must be set up with the appropriate database drivers to use this function.
#'     **Windows** users need ODBC with Microsoft SQL. Contact IS support for ODBC installation.
#'     **Mac** users need `unixodbc` and `freetds`. See instructions in
#'     [`{MetroTransitR}`](https://github.com/Metropolitan-Council/MetroTransitR)
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
#'   councilR.uid = "mc\\myuid",
#'   councilR.pwd = "mypwd"
#' )
#'
#' t_electricity_residential_ctu <- import_from_emissions(table_name = "metro_energy.vw_electricity_residential_ctu")
#' t_eia_energy_consumption_state <- import_from_emissions(table_name = "state_energy.eia_energy_consumption_state")
# t_utility_natural_gas_by_ctu <- import_from_emissions(table_name = "metro_energy.vw_utility_natural_gas_by_ctu")
#' }
#' @importFrom DBI dbCanConnect dbGetQuery dbConnect dbDisconnect
#' @importFrom odbc odbc
#' @importFrom utils osVersion
import_from_emissions <- function(table_name,
                                  uid = getOption("councilR.uid"),
                                  pwd = getOption("councilR.pwd"),
                                  local = TRUE,
                                  serv = "dbsqlcl11t.test.local,65414",
                                  db = "CD_Emissions") {
  # browser()
  # decide which driver to use based on OS

  if (local == FALSE) {
    stop("Non-local isn't ready yet!")
  }

  drv <- if (grepl("mac", osVersion)) {
    "FreeTDS"
  } else {
    "SQL Server"
  }

  # check that DB connection works
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



  conn <- DBI::dbConnect(
    odbc::odbc(),
    Driver = drv,
    Database = db,
    Uid = uid,
    Pwd = pwd,
    Server = serv
  )

  db_sp_table <- DBI::dbGetQuery(conn,
                                 paste0("SELECT * FROM ", table_name))

  DBI::dbDisconnect(conn)

  return(db_sp_table)
}
