#' @title Fetch Greenhouse gas scenario planning data tables
#'
#' @param uid character, your network id.
#'     Default is `getOption("councilR.uid")`. For example, `"mc\\rotenle"`
#' @param pwd character, your network password.
#'     Default is `getOption("councilR.pwd")`. For example, `"my_password"`
#' @param serv character, database server.
#'     Default is `"dbsqlcl11t.test.local,65414"` (the test database).
#' @param db character, database name. Default is `"CD_Emissions"`
#' @param module character, which module tables to pull. One of `"mod_1"`, `"mod_2"`,
#'     `"mod_3"`,` "metro_demos"`,
#'     `"state_demos"`, `"metro_energy"`, `"state_energy"`, or `"all"`
#' @param local logical, whether to pull from the onsite database or Azure.
#'
#' @description WARNING: Function error may results in RStudio crash.
#'
#' @note See `vignette("Options")` to review package options.
#'     You must be set up with the appropriate database drivers to use this function.
#'     **Windows** users need ODBC with Microsoft SQL. Contact IS support for ODBC installation.
#'     **Mac** users need `unixodbc` and `freetds`. See instructions in
#'     [`{MetroTransitR}`](https://github.com/Metropolitan-Council/MetroTransitR)
#'
#' @return a list of tables from the CD_Emissions database. List length depends
#'     `module` parameter.
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
#' mod_1_tables <- import_from_emissions(module = "mod_1")
#' mod_2_tables <- import_from_emissions(module = "mod_2")
#' mod_3_tables <- import_from_emissions(module = "mod_3")
#'
#' # or fetch all tables
#'
#' all <- import_from_emissions(module = "all")
#' }
#'
#' @importFrom DBI dbCanConnect dbGetQuery dbConnect dbDisconnect
#' @importFrom odbc odbc
#' @importFrom purrr map flatten
#' @importFrom utils osVersion
import_from_emissions <- function(uid = getOption("councilR.uid"),
                                  pwd = getOption("councilR.pwd"),
                                  module = c(
                                    "mod_1", "mod_2", "mod_3",
                                    "metro_demos", "state_demos",
                                    "metro_energy", "state_energy",
                                    "all"
                                  ),
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


  tables_to_fetch <- if (module == "all") {
    purrr::flatten(emissions_db_table_names)
  } else {
    emissions_db_table_names[[module]]
  }

  if (length(tables_to_fetch) == 0) {
    stop("No matching module name")
  }

  conn <- DBI::dbConnect(odbc::odbc(),
    Driver = drv,
    Database = db,
    Uid = uid,
    Pwd = pwd,
    Server = serv
  )

  db_sp_tables <- purrr::map(
    tables_to_fetch,
    function(x) {
      DBI::dbGetQuery(
        conn,
        paste0("SELECT * FROM ", x)
      )
    }
  )

  names(db_sp_tables) <- names(tables_to_fetch)

  DBI::dbDisconnect(conn)

  return(db_sp_tables)
}
