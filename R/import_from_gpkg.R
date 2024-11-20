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
#' @param ... additional parameters passed to [sf::read_sf]
#'
#' @return [sf::sf()] object
#' @export
#'
#' @description This function is particularly useful for importing data from
#'     [Minnesota Geospatial Commons](https://gisdata.mn.gov/)
#'     when access to GISLibrary is unavailable.
#'
#' @note This function relies on `{rlang}` internal functions.
#'
#' @importFrom sf read_sf st_transform
#' @importFrom fs file_delete dir_info
#' @importFrom utils download.file tail unzip
#' @importFrom data.table last
#' @importFrom purrr map
#' @examples
#'
#' library(councilR)
#'
#' # import regional parks from Minnesota Geospatial Commons
#' import_from_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/plan_parks_regional/gpkg_plan_parks_regional.zip")
#' # import the "RegionalEnvironmentalJusticeByCensusTract" layer only
#' import_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/trans_tpp2050/gpkg_trans_tpp2050.zip", layer = "RegionalEnvironmentalJusticeByCensusTract")
#'
import_from_gpkg <- function(link,
                             save_file = FALSE,
                             save_path = getwd(),
                             .crs = 4326,
                             keep_temp = FALSE,
                             .quiet = TRUE,
                             ...) {
  requireNamespace("rlang", quietly = TRUE)

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
  rlang:::check_number_whole(.crs)

  # download to a temp file
  temp <- tempfile()
  download.file(link, temp, quiet = .quiet)


  # create location in which to unzip
  unzip_location <- tempdir()
  # unzip the download
  unzip(temp, exdir = unzip_location)

  # find all the gpkg files in the unzip_location
  # Sometimes, the unzipped gpkg file name may not match the
  # downloaded zip file name
  most_recent_gpkg <-
    # get info on all gpkg in our unzip location
    fs::dir_info(unzip_location, type = "file", glob = "*.gpkg$") %>%
    # filter to get the most recently modified
    subset(subset = (modification_time == max(modification_time)),
           select = path,
           drop = TRUE)

  # read in the gpkg as an sf
  out_sf <- sf::read_sf(
    most_recent_gpkg, quiet = .quiet,
    # pass on additional options
    ...) %>%
    sf::st_transform(crs = .crs)

  # write the transformed sf
  if (save_file == TRUE) {
    # create save file name using the gpkg file name
    file_name <- stringr::str_split(most_recent_gpkg, pattern = "/")[[1]] %>% data.table::last()
    saveRDS(out_sf, paste0(save_path, "/", file_name, ".RDS"))
  }

  return(out_sf)
}

#' @rdname import_from_gpkg
#' @export
import_gpkg <- import_from_gpkg
