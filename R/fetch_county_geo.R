#' @title Fetch standardized geographies
#'
#' @description The default `fetch_county_geo()` to return county outlines, plus a suite of other functions to return more niche geographies.
#'
#' To get city, township, and unorganized territory (CTU) boundaries, use `fetch_ctu_geo()`.
#'
#' @param core logical, whether to include all counties in the MPO.
#'     Default is `TRUE`.
#' @param ... Arguments passed to `[tigris]` functions
#'
#' @return An [`sf`] object containing specified geographies.
#' @export
#' @family spatial helpers
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' fetch_county_geo() %>%
#'     ggplot() +
#'     geom_sf() +
#'     theme_void()
#'
#'   fetch_ctu_geo() %>%
#'   ggplot() +
#'   geom_sf(fill = "grey90") +
#'   theme_void() +
#'   geom_sf_text(aes(label = CTU_NAME),
#'     colour = "black",
#'     check_overlap = F,
#'     size = 2
#'   )
#' }
#'
#' @note This function relies on `[{rlang}]` internal functions.
#'
#' @importFrom tigris counties
#' @importFrom cli cli_abort
#' @importFrom purrr map
#' @importFrom dplyr case_when mutate transmute
#'


fetch_county_geo <- function(core = TRUE, ...) {
  rlang:::check_bool(core)

  county_list <- if (core == TRUE) {
    c(
      "003",  # "Anoka",
      "019",  # "Carver",
      "037",  # "Dakota",
      "053",  # "Hennepin",
      "123" , # "Ramsey",
      "139",  # "Scott",
      "163"   # "Washington"
    )
  } else if (core == FALSE) {
    c(
      "003",  # "Anoka",
      "019",  # "Carver",
      "037",  # "Dakota",
      "053",  # "Hennepin",
      "123", # "Ramsey",
      "139",  # "Scott",
      "163",  # "Washington"
      "141",  # "Sherburne",
      "171"   # "Wright"
    )
  }

  # fetch county geograp
  mn_counties <- tigris::counties(state = 27,...)

  county_sf <- mn_counties[mn_counties$COUNTYFP %in% county_list, ]


  return(county_sf)
}

#' @rdname fetch_county_geo
#' @export
#'

fetch_ctu_geo <- function(core = TRUE, ...) {
  rlang:::check_bool(core)
  NAME <- CTU_NAME <- ALAND <- AWATER <- NULL

  county_list <- if (core == TRUE) {
    c(
      "003",  # "Anoka",
      "019",  # "Carver",
      "037",  # "Dakota",
      "053",  # "Hennepin",
      "123" , # "Ramsey",
      "139",  # "Scott",
      "163"   # "Washington"
    )
  } else if (core == FALSE) {
    c(
      "003",  # "Anoka",
      "019",  # "Carver",
      "037",  # "Dakota",
      "053",  # "Hennepin",
      "123",  # "Ramsey",
      "139",  # "Scott",
      "163",  # "Washington"
      "141",  # "Sherburne",
      "171"   # "Wright"
    )
  }

  cities <- tigris::county_subdivisions(
    state = 27,
    county = county_list,
    class = "sf",
    ...
  ) %>%
    dplyr::mutate(
      NAME = dplyr::case_when(
        LSAD == 44 ~ paste(NAME, "Twp."),
        LSAD == 46 ~ paste(NAME, "(unorg.)"),
        TRUE ~ NAME
      )) %>%
    dplyr::transmute(
      CTU_NAME = NAME,
      ALAND = ALAND,
      AWATER = AWATER
    )


  return(cities)
}

