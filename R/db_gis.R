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
#'     be automatically closed after the table is imported.
#'
#'  Further examples can be found in `vignette("Databases", package = "councilR")`.
#'
#' @rdname gis
#' @family database functions
#'
#' @param dbname character, database name. Default is `"GISLibrary"`.
#' @param uid character, user ID. Default is `getOption("councilR.uid")`
#' @param pwd character, user password. Default is `getOption("councilR.pwd")`.
#'
#'
#' @examples \dontrun{
#' library(councilR)
#' library(DBI)
#' library(sf)
#'
#' # set options if you haven't already
#' options(
#'   councilR.uid = "mc\\uid",
#'   councilR.pwd = "mypwd"
#' )
#'
#' # create connection
#' gis <- gis_connection()
#'
#' # pull table using SQL and convert to sf
#' DBI::dbGetQuery(gis, "select *, Shape.STAsText() as wkt from GISLibrary.dbo.AIRPORTS where APNAME ='Flying Cloud'") %>%
#'   st_as_sf(wkt = "wkt", crs = 26915)
#'
#' # disconnect
#' DBI::dbDisconnect(gis)
#'
#' # import a specific table, with no additional SQL logic
#' import_from_gis(query = "GISLibrary.dbo.AIRPORTS", dbname = "GISLibrary")
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
    uid = getOption("councilR.uid"),
    pwd = getOption("councilR.pwd")) {
  purrr::map(
    c(dbname, uid, pwd),
    rlang:::check_string
  )

  if (DBI::dbCanConnect(
    odbc::odbc(),
    dbname,
    timeout = 10,
    Uid = uid,
    Pwd = pwd
  ) != TRUE) {
    cli::cli_abort("Database could not connect.")
  }

  conn <- DBI::dbConnect(
    odbc::odbc(),
    dbname,
    timeout = 10,
    Uid = uid,
    Pwd = pwd
  )
}

#' @param query character, string with the database connection and feature class
#' @param .quiet logical, whether to print time elapsed message.
#'
#' @rdname gis
#'
#' @return `import_from_gis()` - A [sf::sf()] object
#' @export
#' @importFrom sf st_as_sf
#' @importFrom DBI dbGetQuery dbDisconnect
#' @importFrom tictoc tic toc
import_from_gis <- function(query,
                            dbname = "GISLibrary",
                            uid = getOption("councilR.uid"),
                            pwd = getOption("councilR.pwd"),
                            .quiet = FALSE) {
  if (.quiet == FALSE) {
    tictoc::tic()
  }

  conn <- gis_connection(
    dbname = dbname,
    uid = uid,
    pwd = pwd
  )

  que <- DBI::dbGetQuery(
    conn,
    paste0("SELECT *, Shape.STAsText() as wkt FROM ", query)
  )

  sf_df <- sf::st_as_sf(
    que,
    wkt = "wkt", crs = 26915
  )

  DBI::dbDisconnect(conn)

  tictoc::toc()

  return(sf_df)
}
