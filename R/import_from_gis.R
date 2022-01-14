#' Import a dataset from ArcCatalog
#'
#' @param query character, string with the database connection and feature class
#' @param dbname character, database name. Usually either `"GIS"` or `"GISLibrary"`
#' @param uid character, user ID. default is `getOption("councilR.uid")`
#' @param pwd character, user password. Default is `getOption("councilR.pwd")`.
#'
#' @note See `vignette("Options")` to review package options.
#' @return an `sf` object
#' @export
#' @examples \dontrun{
#' import_from_gis("GISLibrary.dbo.AIRPORTS")
#' }
#' @importFrom sf st_as_sf
#' @importFrom odbc odbc dbConnect
#' @importFrom DBI dbGetQuery dbDisconnect
#' @importFrom tictoc tic toc
import_from_gis <- function(query,
                            dbname = "GISLibrary",
                            uid = getOption("councilR.uid"),
                            pwd = getOption("councilR.pwd")) {
  tictoc::tic()
  browser()
  if (DBI::dbCanConnect(odbc::odbc(),
    # driver = "FreeTDS",
    dbname,
    timeout = 10,
    Uid = uid,
    Pwd = pwd
  ) != TRUE) {
    stop("Database could not connect.")
  }

  conn <- DBI::dbConnect(odbc::odbc(),
    # driver = "FreeTDS",
    dbname,
    timeout = 10,
    Uid = uid,
    Pwd = pwd
  )

  que <- DBI::dbGetQuery(
    conn,
    paste0("SELECT *, Shape.STAsText() as wkt FROM ", query)
  )

  DBI::dbFetch(que)

  sf_df <- sf::st_as_sf(
    wkt = "wkt", crs = 26915
  )
  DBI::dbDisconnect(conn)

  tictoc::toc()

  return(sf_df)
}
