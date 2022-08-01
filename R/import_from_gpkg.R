#' @title Import an `sf` object using a geopackage URL
#'
#' @param link character, URL for the `.gpkg` object
#' @param save_file logical, whether to save the downloaded file.
#'     File will be saved in .RDS format
#' @param save_path character, the path where the downloaded file should be saved.
#'     Default location is the working directory.
#' @param .crs numeric, the CRS code or string used to transform the download.
#'     Default is `4326` (WGS 84)
#' @param keep_temp character, whether to keep the temporary download.
#'     Default is `FALSE`.
#' @param .quiet logical, suppress messages. Default is `TRUE`.
#'
#' @return an [`sf`] object
#' @export
#'
#' @description Particularly useful for importing data from Minnesota Geospatial Commons
#'     when access to the internal GIS database is unavailable.
#'
#' @note This function relies on `[{rlang}]` internal functions.
#'
#' @importFrom sf read_sf st_transform
#' @importFrom fs file_delete
#' @importFrom utils download.file tail unzip
#' @importFrom purrr map
#' @examples
#'
#' library(councilR)
#'
#' # import regional parks from Minnesota Geospatial Commons
#' import_from_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/plan_parks_regional/gpkg_plan_parks_regional.zip")
#'
import_from_gpkg <- function(link,
                             save_file = FALSE,
                             save_path = getwd(),
                             .crs = 4326,
                             keep_temp = FALSE,
                             .quiet = TRUE) {

  # check input types
  purrr::map(
    c(link),
    rlang:::check_string
  )
  purrr::map(
    c(save_file, keep_temp, .quiet),
    rlang:::check_bool
  )
  rlang:::check_string(save_path)
  rlang:::check_number(.crs)

  # download to a temp file
  temp <- tempfile()
  download.file(link, temp, quiet = .quiet)

  # get file names
  file_names <- strsplit(link, split = "/")

  # fetch the core file name
  file_name <- tail(file_names[[1]], 1) %>%
    gsub(pattern = "gpkg_", replacement = "") %>%
    gsub(pattern = ".zip", replacement = "")

  # read in the gpkg as an sf
  out_sf <- sf::read_sf(unzip(temp, paste0(file_name, ".gpkg")), quiet = .quiet, ) %>%
    sf::st_transform(crs = .crs)

  # delete the temp file
  if (keep_temp == FALSE) {
    fs::file_delete(paste0(file_name, ".gpkg"))
  }

  # write the transformed sf
  if (save_file == TRUE) {
    saveRDS(out_sf, paste0(save_path, "/", file_name, ".RDS"))
  }

  return(out_sf)
}
