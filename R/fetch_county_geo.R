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
#'   ggplot() +
#'   geom_sf() +
#'   theme_void()
#'
#' fetch_ctu_geo() %>%
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
#' @importFrom tigris counties
#' @importFrom cli cli_abort
#' @importFrom purrr map
#' @importFrom dplyr case_when mutate transmute
#'
fetch_county_geo <- function(core = TRUE, ...) {
  check_bool(core)

  county_list <- if (core == TRUE) {
    list(
      "Anoka County" = "003", # "Anoka",
      "Carver County" = "019", # "Carver",
      "Dakota County" = "037", # "Dakota",
      "Hennepin County" = "053", # "Hennepin",
      "Ramsey County" = "123", # "Ramsey",
      "Scott County" = "139", # "Scott",
      "Washington County" = "163" # "Washington"
    )
  } else if (core == FALSE) {
    list(
      "Anoka County" = "003", # "Anoka",
      "Carver County" = "019", # "Carver",
      "Dakota County" = "037", # "Dakota",
      "Hennepin County" = "053", # "Hennepin",
      "Ramsey County" = "123", # "Ramsey",
      "Scott County" = "139", # "Scott",
      "Washington County" = "163", # "Washington"
      "Sherburne County" = "141", # "Sherburne",
      "Wright County" = "171" # "Wright"
    )
  }

  # fetch county geograp
  mn_counties <- tigris::counties(state = 27, ...)

  county_sf <- mn_counties[mn_counties$COUNTYFP %in% county_list, ]


  return(county_sf)
}

#' @rdname fetch_county_geo
#' @export
#'

fetch_ctu_geo <- function(core = TRUE, ...) {
  check_bool(core)
  NAME <- CTU_NAME <- ALAND <- AWATER <- NULL

  county_list <- if (core == TRUE) {
    list(
      "Anoka County" = "003", # "Anoka",
      "Carver County" = "019", # "Carver",
      "Dakota County" = "037", # "Dakota",
      "Hennepin County" = "053", # "Hennepin",
      "Ramsey County" = "123", # "Ramsey",
      "Scott County" = "139", # "Scott",
      "Washington County" = "163" # "Washington"
    )
  } else if (core == FALSE) {
    list(
      "Anoka County" = "003", # "Anoka",
      "Carver County" = "019", # "Carver",
      "Dakota County" = "037", # "Dakota",
      "Hennepin County" = "053", # "Hennepin",
      "Ramsey County" = "123", # "Ramsey",
      "Scott County" = "139", # "Scott",
      "Washington County" = "163", # "Washington"
      "Sherburne County" = "141", # "Sherburne",
      "Wright County" = "171" # "Wright"
    )
  }

  cities_geo <- tigris::county_subdivisions(
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
      )
    ) %>%
    dplyr::left_join(mn_fips_codes,
      by = c(
        "COUNTYFP" = "county_code",
        "STATEFP" = "state_code"
      )
    )

  cities <- if (core == TRUE) {
    cities_geo %>%
      dplyr::transmute(
        CTU_NAME = NAME,
        ALAND = ALAND,
        AWATER = AWATER
      )
  } else if (core == FALSE) {
    cities_geo %>%
      dplyr::group_by(NAME) %>%
      dplyr::mutate(n = dplyr::n()) %>%
      dplyr::mutate(CTU_NAME = dplyr::if_else(
        n > 1 & LSAD != 25,
        paste0(NAME, " - ", county, " Co."), # cities dont get merged
        NAME
      )) %>%
      dplyr::group_by(CTU_NAME) %>%
      dplyr::summarise(
        geometry = sf::st_union(geometry),
        ALAND = sum(ALAND, na.rm = TRUE),
        AWATER = sum(AWATER, na.rm = TRUE)
      ) %>%
      dplyr::arrange(CTU_NAME)
  }


  return(cities)
}
