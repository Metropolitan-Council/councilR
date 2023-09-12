#' Import a dataset from ArcCatalog
#'
#' @param query character, string with the database connection and feature class
#' @param dbname character, database name. Default is `"GISLibrary"`.
#'     Must match the database name in `query`.
#' @param uid character, user ID. Default is `getOption("councilR.uid")`
#' @param pwd character, user password. Default is `getOption("councilR.pwd")`.
#' @param .quiet logical, whether to print time elapsed message.
#'
#' @note See `vignette("Options")` to review package options.
#'     You must be set up with the appropriate database drivers to use this function.
#'     **Windows** users need ODBC with Microsoft SQL. Contact IS support for ODBC installation.
#'     **Mac** users need `unixodbc`, `freetds`, and properly configured `odbc.ini`.
#'     See instructions in the
#'     [onboarding guide](https://furry-adventure-596f3adb.pages.github.io/database-connections.html)
#'     and contact package maintainer for assistance.
#'
#'    This function relies on `[{rlang}]` internal functions.
#'
#' @return An [`sf`] object
#' @export
#' @examples \dontrun{
#' library(councilR)
#'
#' options(councilR.uid = "mc\\uid",
#'         councilR.pwd = "mypwd")
#'
#' # query db name matches
#' import_from_gis(query = "GISLibrary.dbo.AIRPORTS", dbname = "GISLibrary")
#' }
#' @importFrom sf st_as_sf
#' @importFrom odbc odbc
#' @importFrom DBI dbGetQuery dbDisconnect dbCanConnect dbConnect
#' @importFrom tictoc tic toc
#' @importFrom purrr map
import_from_gis <- function(query,
                            dbname = "GISLibrary",
                            uid = getOption("councilR.uid"),
                            pwd = getOption("councilR.pwd"),
                            .quiet = FALSE) {
  requireNamespace("rlang", quietly = TRUE)

  purrr::map(
    c(query, dbname, uid, pwd),
    rlang:::check_string
  )

  if (.quiet == FALSE) {
    tictoc::tic()
  }

  if (DBI::dbCanConnect(
    odbc::odbc(),
    dbname,
    timeout = 10,
    Uid = uid,
    Pwd = pwd
  ) != TRUE) {
    stop("Database could not connect.")
  }

  conn <- DBI::dbConnect(
    odbc::odbc(),
    dbname,
    timeout = 10,
    Uid = uid,
    Pwd = pwd
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
