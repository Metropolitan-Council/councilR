#' @title Council colors turned into palettes
#' @description `scale_color_council()` and `scale_fill_council` will give
#'     a color palette starting with `councilBlue`, followed by `cdGreen` and `mtsRed`,
#'     and then a couple other Council colors which will look
#'     decent afterwards (up to 8 levels).
#' @family aesthetics
#'
#' @param n numeric, value between 1-8
#' @param name character, starting color name. One of `councilR::colors`.
#'
#' @description Modified [RColorBrewer::brewer.pal()]
#' @examples
#' \dontrun{
#' library(scales)
#' library(councilR)
#' show_col(council_pal2())
#'
#' }
#' @export
council.pal <- function(n, name) {
  switch(name,
    councilCat = switch(n - 2,
      c(colors$councilBlue, colors$cdGreen, colors$mtsRed),
      c(colors$councilBlue, colors$cdGreen, colors$mtsRed, colors$transitYellow),
      c(colors$councilBlue, colors$cdGreen, colors$mtsRed, colors$transitYellow, colors$playLiBlue),
      c(colors$councilBlue, colors$cdGreen, colors$mtsRed, colors$transitYellow, colors$playLiBlue, colors$metrostatsBrown),
      c(colors$councilBlue, colors$cdGreen, colors$mtsRed, colors$transitYellow, colors$playLiBlue, colors$metrostatsBrown, colors$metrostatsMePurp),
      c(colors$councilBlue, colors$cdGreen, colors$mtsRed, colors$transitYellow, colors$playLiBlue, colors$metrostatsBrown, colors$metrostatsMePurp, colors$metroOrange)
    )
  )
}

#' @title Discrete Council palettes
#'
#' @description Modified from [scales::brewer_pal()].
#'     Stripped down all arguments, just to show the core
#'
#' @family aesthetics
#' @return council.pal
#' @export
#'
council_pal2 <- function() {
  function(n) {
    council.pal(n, "councilCat")
    ## modified, usually this is selected by a function
    ## with type and name as arguments,
    # selecting a palette from a list called scales:::brewer
  }
}

#' @title Fill scale
#'
#' @param ... Arguments passed [ggplot2::discrete_scale()]
#' @param aesthetics The names of the aesthetics that this scale works with.
#'
#' @return a Scale object
#' @export
#' @family aesthetics
#' @examples
#' \dontrun{
#' test <- tibble(x = c(1:8), y = c(1:8), col = c(1:8))
#' ggplot(test, aes(x = x, y = y, col = as.factor(col), fill = as.factor(col))) +
#'   geom_point(size = 4, pch = 21) +
#'   scale_fill_council()
#' }
#' @importFrom ggplot2 discrete_scale
#'
scale_fill_council <- function(..., aesthetics = "fill") {
  ggplot2::discrete_scale(
    aesthetics, "custom",
    council_pal2(), ...
  ) ## give a new name to the
  ## scale, it will create a new Scale object.
}

#' @title Color scale
#'
#' @description Modified from [ggplot2::scale_fill_brewer()]
#'
#' @inheritParams scale_fill_council
#'
#' @return a Scale object
#' @export
#' @family aesthetics
#' @examples
#' \dontrun{
#' test <- tibble(x = c(1:8), y = c(1:8), col = c(1:8))
#' ggplot(test, aes(
#'   x = x, y = y,
#'   col = as.factor(col),
#'   fill = as.factor(col)
#' )) +
#'   geom_point(size = 4, pch = 21) +
#'   scale_color_council()
#' }
#' @importFrom ggplot2 discrete_scale
scale_color_council <- function(..., aesthetics = "colour") {
  ggplot2::discrete_scale(
    aesthetics, "custom",
    council_pal2(), ...
  )
  ## give a new name to the
  ## scale, it will create a new Scale object.
}
