#' @title Fetch standardized geographies
#'
#' @description The default `fetch_county_geo()` to return county outlines, plus a suite of other functions to return more niche geographies.
#'
#'To get city, township, and unorganized territory (CTU) boundaries, use `fetch_ctu_geo()`.
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
#' fetch_county_geo()
#'
#' library(ggplot2)
#' fetch_ctu_geo() %>%
#' ggplot() +
#' geom_sf(fill = "grey90") +
#' theme_void() +
#' geom_sf_text(aes(label = CTU_NAME),
#' colour = "black",
#' check_overlap = F,
#' size = 2)
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

#' @rdname fetch_county_geo
#' @export
#'

fetch_ctu_geo <- function(core = TRUE, ...) {
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

  cities <- tigris::county_subdivisions(
    state = "MN",
    county = county_list,
    class = "sf"
  ) %>%
    dplyr::mutate(NAME = dplyr::case_when(
      LSAD == 44 ~ paste(NAME, "Twp."),
      LSAD == 46 ~ paste(NAME, "(unorg.)"),
      TRUE ~ NAME
    )) %>%
  ## if expanding to greater mn or another region, you do have to do some unions, and further cleaning.
  #   group_by(NAME) %>%
  #   mutate(n = n()) %>%
  #   left_join(st_drop_geometry(county_outline) %>%
  #               transmute(
  #                 COUNTYFP = COUNTYFP,
  #                 CONAME = NAME
  #               )) %>%
  #   mutate(NAME = case_when(
  #     n > 1 & LSAD != 25 ~ paste0(NAME, " - ", CONAME, " Co."), # cities dont get merged
  #     TRUE ~ NAME
  #   )) %>%
  #   group_by(NAME) %>%
  #   summarise() %>%
  #   # summarize(geometry = st_union(geom)) %>%
  #   arrange(NAME) %>%
  #   rename(GEO_NAME = NAME)
  dplyr::transmute(CTU_NAME = NAME)


  return(cities)
}


#' @rdname fetch_ctu_geo
#' @export
#'

