#' @title Council blue menu background and text colors
#'
#' @description A named list of background and text colors for cascading menus
#'
#' @format A named list of 8 lists. Each item contains a named list of two items:
#'     - "background" is the background or fill color
#'     - "color" is the text or contrast color
#' \describe{
#'   \item{level1}{Level 1, contains the darkest background and lightest color}
#'   \item{level2}{Level 2}
#'   \item{level3}{Level 3}
#'   \item{level4}{Level 4}
#'   \item{level5}{Level 5}
#'   \item{level6}{Level 6}
#'   \item{level7}{Level 7}
#'   \item{level8}{Level 8, contains the lightest background and darkest color}
#' }
#'
#' @family aesthetics
#' @note See these colors in action in `vignette("Color")`
#'
#' @examples
#' blue_cascade$level1$background
#' blue_cascade$level2$color
#' @source \url{https://metrocouncil.org}
"blue_cascade"
