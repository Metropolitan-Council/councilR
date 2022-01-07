#' Council ggplot2 theme
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
#' @details
#' # Council fonts
#'
#' There are a few requirements needed if you want to use Council font families.
#'
#' 1. You must have the optional [`{showtext}`](https://github.com/yixuan/showtext)
#'  and [`{sysfonts}`](https://github.com/yixuan/sysfonts) packages installed.
#'
#' 2. You must have the following fonts installed:
#'   - HelveticaNeueLT Standard Light, Standard Condensed, and Standard Medium Condensed
#'   - Arial Narrow
#'   - Palatino Linotype
#'   If you do not have the font files handy, contact a package author or your manager.
#'
#' 3. The font family and file names much match those listed below
#'   - "HelveticaNeueLT Std Cn" = "HelveticaNeueLTStd-Cn.otf"
#'   - "HelveticaNeueLT Std Lt" = "HelveticaNeueLTStd-Lt.otf"
#'   - "Arial Narrow" ="ARIALN.TTF"
#'   - "HelveticaNeueLT Std Med Cn" = "HelveticaNeueLTStd-MdCn.otf")
#'   - "Palatino Linotype" = "pala.ttf"
#'
#' 4. Consider options for your OS
#'   - On Windows, be sure that the fonts are installed for the entire system, not just a single user.
#'     These are located in `C:/Windows/Fonts`.
#'   - On Mac OSX, be sure that fonts are installed for the user. These are located in `~/Library/Fonts`.
#'     Consider copying the entire system font directory (`/Library/Fonts`) to the user font directory for easy access.
#'
#' # Font size suggestions
#'  If you are creating plots for a presentation, you should increase all font sizes.
#'  Check the default font sizes in the presentation platform you are working in
#'  and adjust from there. For example, Microsoft PowerPoint sets slide headings at 77 pnt and
#'  body text at 46 pnt. You will likely want to make the plot title less than 77 pnt so you
#'  can preserve the text hierarchy in the slide.
#'
#' # Theme modifications
#'  If you wish to make changes to this theme template, such as removing
#'  the legend, adjusting text positions,  access the source code by typing
#'  `councilR::council_theme` in the console. You can then copy/paste the
#'  function into whatever document you are working on and make changes there.
#'  If you want to make a change to the default template, open an Issue on GitHub
#'  and ask for the change to be made, and/or make the change yourself and make
#'  a pull request.
#'
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
  if (use_showtext == TRUE) {
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
        size = size_legend_title
      ),
      legend.text = ggplot2::element_text(
        family = font_axis,
        size = size_legend_text,
      ),
      strip.text.x = ggplot2::element_text(
        family = font_axis,
        size = size_axis_title
      )
    )
}
