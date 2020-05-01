#' Council ggplot2 theme
#'
#' @param use_showtext logical, whether to use Council fonts.
#' @param size_header header font size in px
#' @param size_axis_title axis title font size in px
#' @param size_legend_title legend title font size in px
#' @param size_axis_text axis text font size in px
#' @param size_legend_text legend test font size in px
#' @param size_caption caption font size in px
#' @param size_margin the margin size in pt
#'
#'
#' @details
#' # Council fonts
#'
#' If you wish to use Council fonts, you must have the following fonts installed:
#'   - HelveticaNeueLT Standard Light, Standard Condensed, and Standard Medium Condensed
#'   - Arial Narrow
#'   - Palatino Linotype
#'
#' You must also have the [showtext](https://github.com/yixuan/showtext) and [sysfonts](https://github.com/yixuan/sysfonts)
#'  packages installed.
#'
#' # Font size suggestions
#'  If you are creating plots for a presentation, you should increase all font sizes.
#'  Check the default font sizes in the presentation platform you are working in
#'  and adjust from there.
#'
#' # Theme modifications
#'  If you wish to make changes to this theme template, such as removing
#'  the legend, adusting text positons,  access the source code by typing
#'  `councilR::council_theme` in the console. You can then copy/paste the
#'  function into whatever document you are working on and make changes.
#'
#' @return a [ggplot2::theme()] object
#' @export
#'
#' @family aesthetics
#'
#' @importFrom ggplot2 theme_minimal theme element_text margin
#'
council_theme <- function(use_showtext = FALSE,
                         size_header = 22,
                         size_axis_title = 14,
                         size_legend_title = 14,
                         size_axis_text = 11,
                         size_legend_text = 10,
                         size_caption = 8,
                         size_margin = 10) {
  if (use_fonts == TRUE) {
    requireNamespace("sysfonts", quietly = TRUE)
    requireNamespace("showtext", quietly = TRUE)

    showtext::showtext_auto()
    sysfonts::font_paths()
    files <- sysfonts::font_files()
    sysfonts::font_add("HelveticaNeueLT Std Cn", "HelveticaNeueLTStd-Cn.otf")
    sysfonts::font_add("HelveticaNeueLT Std Lt", "HelveticaNeueLTStd-Lt.otf")
    sysfonts::font_add("Arial Narrow", "ARIALN.TTF")
    sysfonts::font_add("HelveticaNeueLT Std Med Cn", "HelveticaNeueLTStd-MdCn.otf")
    sysfonts::font_add("Palatino Linotype", "pala.ttf")


    font_title <- "HelveticaNeueLT Std Lt"
    font_caption <- "Palatino Linotype"
    font_axis <- "Arial Narrow"
  } else {
    font_title <- "sans"
    font_caption <- "serif"
    font_axis <- "sans"
  }


  ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(size_margin, size_margin, size_margin, size_margin, "pt"),

      axis.title.x = ggplot2::element_text(
        vjust = -1,
        family = font_axis,
        size = size_axis_title
      ),
      axis.title.y = ggplot2::element_text(
        vjust = 2,
        family = font_axis,
        size = size_axis_title
      ),
      axis.text.x = ggplot2::element_text(
        family = font_axis,
        size = size_axis_text,
        vjust = 0.5
      ),
      axis.text.y = ggplot2::element_text(
        family = font_axis,
        size = size_axis_text,
        vjust = 1
      ),
      plot.caption = ggplot2::element_text(
        family = font_caption,
        hjust = 1,
        size = size_caption
      ),
      plot.title = ggplot2::element_text(
        family = font_title,
        # hjust = 0.5,
        size = size_header
      ),
      legend.title = ggplot2::element_text(
        family = font_title,
        size = size_legend_title,
        vjust = -2
      ),
      legend.justification = c(1, 0),
      legend.position = c(1, .5),
      legend.text = ggplot2::element_text(
        family = font_axis,
        size = size_legend_text
      ),
      strip.text.x = ggplot2::element_text(
        family = font_axis,
        size = size_axis_title
      )
    )
}
