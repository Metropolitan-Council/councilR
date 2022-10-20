#' @title Council map starters
#'
#' @description Building on functionality for creating maps When mapping continuous data, `map_council_continuous()` could save some work.
#'
#' @param df The `sf` object to be mapped
#' @param .fill For continuous data, the variable which should be used to fill polygons
#' @param .lwd The line width of polygons, default setting is `0.5`.
#' @param .low The color to fill the lowest number. Default is brown.
#' @param .mid The color to fill the midpoint number. Default is white
#' @param .high The color to fill the highest number. Default is teal
#' @param .midpoint The midpoint of a diverging color scheme. Default is `0`.
#'
#' @return a [ggplot2] plot
#' @export
#'
#' @family aesthetics
#'
#' @note This function relies on `[{rlang}]` internal functions.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(councilR)
#'
#' fetch_ctu_geo() %>%
#'   map_council_continuous(.fill = ALAND, .midpoint = 5e7)
#' }
#'
#' @importFrom ggplot2 ggplot geom_sf scale_fill_gradient2 aes
#' @importFrom ggspatial annotation_north_arrow north_arrow_fancy_orienteering annotation_scale
#' @importFrom rlang enquo
#'

map_council_continuous <- function(df,
                                   .fill,
                                   .lwd = .5,
                                   .low = "#8c510a",
                                   .mid = "white",
                                   .high = "#01665e",
                                   .midpoint = 0) {
  df %>%
    ggplot() +
    geom_sf(aes(fill = !!enquo(.fill)), lwd = .lwd) +
    theme_council_geo() +
    scale_fill_gradient2(low = .low, mid = .mid, high = .high, midpoint = .midpoint) +
    annotation_scale(
      location = "bl",
      bar_cols = c("grey60", "white")
    ) +
    annotation_north_arrow(
      location = "tr", which_north = "true",
      # pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
      style = north_arrow_fancy_orienteering(
        fill = c("grey40"),
        line_col = "grey20"
      )
    )
}

#' @rdname map_council_continuous
#' @export
#'
