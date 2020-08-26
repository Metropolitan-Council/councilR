#' Import a dataset from ArcCatalog
#'
#' @param query character, string with the database connection and feature class
#' @param dbname character, database name. Usually either `"GIS"` or `"GISLibrary"`
#'
#' @return an `sf` object
#' @export
#' @examples \dontrun{import_from_gis("GISTransit.dbo.PublicParcelsMetroCTUs")}
#' @importFrom sf st_as_sf
#' @importFrom odbc odbc dbConnect
#' @importFrom DBI dbGetQuery dbDisconnect
#' @importFrom tictoc tic toc
import_from_gis <- function(query,
                            dbname = "GISLibrary"){
  tictoc::tic()

  if(DBI::dbCanConnect(odbc::odbc(), dbname) != TRUE){
    stop("Database could not connect.")
  }

  conn <- odbc::dbConnect(odbc::odbc(), dbname)

  sf_df <- sf::st_as_sf(DBI::dbGetQuery(conn,
                                        paste0("SELECT *, Shape.STAsText() as wkt FROM ", query)),
                        wkt = "wkt", crs = 26915)
  DBI::dbDisconnect(conn)
  tictoc::toc()
  return(sf_df)
}
