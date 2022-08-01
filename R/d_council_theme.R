#' @title DEPRECATED Council ggplot2 theme
#'
#' @param use_showtext Logical, whether to use Council fonts.
#' @param size_header Header font size in pt. Default is 22
#' @param size_axis_title Axis title font size in pt. Default is 14.
#' @param size_legend_title Legend title font size in pt. Default is 14.
#' @param size_axis_text Axis text font size in pt. Default is 11
#' @param size_legend_text Legend test font size in pt. Default is 10.
#' @param size_caption Caption font size in pt. Default is 8.
#' @param size_margin Margin size in pt. Default is 10
#'
#'
#' @return an error
#' @export
#'
#' @importFrom rlang abort
#'
council_theme <- function(use_showtext = FALSE,
                          size_header = 22,
                          size_axis_title = 14,
                          size_legend_title = 14,
                          size_axis_text = 11,
                          size_legend_text = 10,
                          size_caption = 8,
                          size_margin = 10) {
  rlang::abort(
    message = "council_theme() is deprecated as of version 0.1.1. Please use theme_council()",
    body = c("Equivalent is `theme_council(use_showtext = FALSE, use_manual_font_sizes = TRUE)`"))

}
