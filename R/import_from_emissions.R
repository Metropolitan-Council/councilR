#' Import data table from Emissions Database
#'
#' @param uid character, your network id.
#'     Default is `getOption("councilR.uid")`. For example, `"mc\\rotenle"`
#' @param pwd character, your network password.
#'     Default is `getOption("councilR.pwd")`. For example, `"my_password"`
#' @param serv character, database server.
#'     Default is `"dbsqlcl11t.test.local,65414"` (the test database).
#' @param db character, database name. Default is `"CD_Emissions"`
#' @param table_name character, which table to pull
#'
#' @description WARNING: Function error may results in RStudio crash. Requires a password for access to the database.
#'
#' @note To make connection seamless, add `councilR.uid` and `councilR.pwd` to your `.Rprofile`. Edit your `.Rprofile` as such
#'
#'  ```
#'  options(
#'      ...,
#'      councilR.uid = "mc\\myuid",
#'      councilR.pwd = keyring::key_get("MetC")
#'  )
#'  ```
#'  See more details in [`{councilR}` documentation](https://github.com/Metropolitan-Council/councilR/blob/main/vignettes/Options.Rmd).
#'
#' @return a table from the CD_Emissions database.
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
#' t_utility_natural_gas_by_ctu <- import_from_emissions(table_name = "metro_energy.vw_utility_natural_gas_by_ctu")
#' }
#' @importFrom DBI dbCanConnect dbGetQuery dbConnect dbDisconnect
#' @importFrom odbc odbc
#' @importFrom purrr map flatten
#' @importFrom utils osVersion
import_from_emissions <- function(uid = councilR.uid,
                                  pwd = councilR.pwd,
                                  table_name = "metro_energy.vw_electricity_residential_ctu",
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
  if (DBI::dbCanConnect(
    odbc::odbc(),
    Driver = drv,
    Database = db,
    Server = serv,
    Trusted_Connection = "yes"
  ) == FALSE) {
    stop("Database failed to connect")
  }

  conn <- DBI::dbConnect(
    odbc::odbc(),
    Driver = drv,
    Database = db,
    Uid = uid,
    Pwd = pwd,
    Server = serv,
    Trusted_Connection = "yes"
  )

  db_sp_table <- DBI::dbGetQuery(conn,
                  paste0("SELECT * FROM ", table_name))

  DBI::dbDisconnect(conn)

  return(db_sp_table)
}
