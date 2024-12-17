#' @title GISLibrary
#'
#' @description
#' Functions for interacting with GISLibrary, the primary Met Council
#' ESRI geospatial database.
#'
#' @details
#'
#'  - `gis_connection()` creates an S4 Microsoft SQL Server object for GISLibrary.
#'     This function will *not* automatically close the connection, so take
#'     care to use [DBI::dbDisconnect()] once you are done.
#'  - `import_from_gis()` imports a given table from GISLibrary and
#'     converts it into a [sf::sf()] object. The connection will
#'     be automatically closed after the table is imported. If the table
#'     does not have any spatial data, the table will be returned as a
#'     data.frame.
#'
#'  Further examples can be found in `vignette("Databases", package = "councilR")`.
#'
#' @rdname gis
#' @family database functions
#' @export
#'
#' @param dbname character, database name. Default is `"GISLibrary"`.
#' @inheritParams fred_connection
#'
#' @examples \dontrun{
#' library(councilR)
#' library(DBI)
#' library(sf)
#'
#' # set credentials if you haven't already
#' keyring::key_set_with_value("councilR.uid", "mc\\myuuid")
#' keyring::key_set_with_value("councilR.pwd", "password")
#'
#' # create connection
#' gis <- gis_connection()
#'
#' # pull table using SQL and convert to sf
#' DBI::dbGetQuery(gis, "select *, Shape.STAsText() as wkt from GISLibrary.dbo.COUNTIES where CO_NAME = 'ANOKA'") %>%
#'   st_as_sf(wkt = "wkt", crs = 26915)
#'
#' # disconnect
#' DBI::dbDisconnect(gis)
#'
#' # import a specific table, with no additional SQL logic
#' import_from_gis(query = "GISLibrary.dbo.COUNTIES", dbname = "GISLibrary")
#' }
#'
#' @return `gis_connection()` - A S4 Microsoft SQL Server object
#'
#' @family database functions
#' @importFrom DBI dbCanConnect dbConnect
#' @importFrom purrr map
#' @importFrom cli cli_abort
#' @importFrom odbc odbc
gis_connection <- function(
    dbname = "GISLibrary",
    uid = keyring::key_get("councilR.uid"),
    pwd = keyring::key_get("councilR.pwd")) {
  purrr::map(
    c(dbname, uid, pwd),
    check_string
  )

  serv <- "azdbsqlcl11.mc.local"

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
        DSN = dbname,
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
        Database = dbname,
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
        DSN = dbname,
        Uid = uid,
        Pwd = pwd
      )
    } else if (drv == "SQL Server") {
      DBI::dbConnect(
        odbc::odbc(),
        Driver = drv,
        Database = dbname,
        Uid = uid,
        Pwd = pwd,
        Server = serv,
        Trusted_Connection = "yes"
      )
    }
}

#' @param query character, string with the database connection and feature class
#' @param .quiet logical, whether to print time elapsed message.
#' @param geometry logical, whether to pull the geometry column, if it exists.
#'   Default value is `TRUE`.
#'
#' @rdname gis
#'
#' @return `import_from_gis()` - A [sf::sf()] object or a data frame
#' @export
#' @importFrom sf st_as_sf
#' @importFrom DBI dbGetQuery dbDisconnect
#' @importFrom tictoc tic toc
#' @importFrom magrittr extract2
#' @importFrom dplyr filter
import_from_gis <- function(query,
                            dbname = "GISLibrary",
                            uid = keyring::key_get("councilR.uid"),
                            pwd = keyring::key_get("councilR.pwd"),
                            geometry = TRUE,
                            .quiet = FALSE) {
  if (.quiet == FALSE) {
    tictoc::tic()
  }

  conn <- gis_connection(
    dbname = dbname,
    uid = uid,
    pwd = pwd
  )

  # fetch query table column names
  column_names <- DBI::dbGetQuery(
    conn,
    paste0(
      "SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = '",
      # remove GISLibrary.dbo. to get just the table name
      gsub(pattern = "GISLibrary.dbo.", replacement = "", x = query), "'"
    )
  )

  # if there are any geometry columns, and we want those columns
  # pull as wkt
  if (("geometry" %in% column_names$DATA_TYPE) & geometry == TRUE) {
    # fetch column name with geometry
    geo_column <- column_names %>%
      dplyr::filter(DATA_TYPE == "geometry") %>%
      magrittr::extract2("COLUMN_NAME")

    # fetch query
    que <- DBI::dbGetQuery(
      conn,
      paste0("SELECT *, ", geo_column, ".STAsText() as wkt FROM ", query)
    )

    # convert wkt to sf
    return_table <- sf::st_as_sf(
      que,
      wkt = "wkt", crs = 26915
    )
  }
  # otherwise, if there are geometry columns and we do NOT want those columns
  else if (("geometry" %in% column_names$DATA_TYPE) & geometry == FALSE) {
    # find columns that are NOT geometry
    non_geo_columns <- column_names %>%
      dplyr::filter(DATA_TYPE != "geometry")

    # fetch query
    que <- DBI::dbGetQuery(
      conn,
      paste0(
        "SELECT ", paste0(non_geo_columns$COLUMN_NAME, collapse = ", "),
        " FROM ", query
      )
    )
    return_table <- que
  } else {
    # otherwise, just pull the table with all columns
    return_table <- DBI::dbGetQuery(
      conn,
      paste0("SELECT * FROM ", query)
    )
  }

  DBI::dbDisconnect(conn)

  tictoc::toc()

  return(return_table)
}
