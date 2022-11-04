#' @title Council map starters
#'
#' @description Building on functionality for creating maps when mapping continuous data, `map_council_continuous()` could save some work.
#'
#' @param df The `sf` object to be mapped
#' @param .fill For continuous data, the variable (column name) which
#'     should be used to fill polygons
#' @param .lwd numeric, line width of polygons, default setting is `0.5`.
#' @param .low character, color name or hex code to fill the lowest number.
#'     Default is `"#8c510a"` (brown)
#' @param .mid character, color name or hex code to fill the midpoint number.
#'     Default is `"white"`
#' @param .high character,  color name or hex code to fill the highest number.
#'     Default is `"#01665e"` (teal)
#' @param .midpoint numeric, midpoint of a diverging color scheme.
#'     Default is `0`.
#'
#' @return a [ggplot2] plot
#' @export
#'
#' @family aesthetics
#'
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
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = !!enquo(.fill)), lwd = .lwd) +
    theme_council_geo() +
    ggplot2::scale_fill_gradient2(low = .low, mid = .mid,
                                  high = .high, midpoint = .midpoint) +
    ggspatial::annotation_scale(
      location = "bl",
      bar_cols = c("grey60", "white")
    ) +
    ggspatial::annotation_north_arrow(
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
