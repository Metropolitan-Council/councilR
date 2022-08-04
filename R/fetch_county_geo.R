#' @title Fetch standardized county geography
#'
#' @param core logical, whether to include all counties in the MPO.
#'     Default is `TRUE`.
#' @param ... Arguments passed to `[tigris::counties]`
#'
#' @return An [`sf`] object containing county geographies.
#' @export
#' @family spatial helpers
#' @examples
#' \dontrun{
#' fetch_county_geo()
#' }
#'
#' @note This function relies on `[{rlang}]` internal functions.
#'
#' @importFrom tigris counties
#' @importFrom cli cli_abort
#' @importFrom purrr map
#'
#' @examples
#' \dontrun{
# library(ggplot2)
# library(councilR)
#
# fetch_county_geo() %>%
#   ggplot() +
#   geom_sf() +
#   theme_void()
#'}

fetch_county_geo <- function(core = TRUE, ...) {
  rlang:::check_bool(core)

  county_list <- if (core == TRUE) {
    c(
      "Anoka",
      "Carver",
      "Dakota",
      "Hennepin",
      "Ramsey",
      "Scott",
      "Washington"
    )
  } else if (core == FALSE) {
    c(
      "Anoka",
      "Carver",
      "Dakota",
      "Hennepin",
      "Ramsey",
      "Scott",
      "Sherburne",
      "Washington",
      "Wright"
    )
  }

  # fetch county geograp
  mn_counties <- tigris::counties(state = "MN", ...)

  county_sf <- mn_counties[mn_counties$NAME %in% county_list, ]


  return(county_sf)
}
