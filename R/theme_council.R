#' Council ggplot2 theme
#'
#' @param base_size numeric, base font size, given in pts. Default is `11`
#' @param base_family character, base font family. Default is `""`
#' @param base_line_size numeric, base size for line elements. Default is `base_size/22`
#' @param base_rect_size numeric, base size for rect elements. Default is `base_size/22`
#' @param use_showtext logical, whether to use Council fonts.
#' @param use_manual_font_sizes logical, use supplied font sizes.
#' @param font_sizes named list of font sizes. Only used if `use_manual_font_sizes` is `TRUE`
#'
#' @return a [ggplot2::theme()] object
#' @export
#'
#' @family aesthetics
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
#'
#'  Generally, font sizes should be no smaller than 11 point.
#'
#'  If you are creating plots for a presentation, you should increase all font sizes.
#'  Check the default font sizes in the presentation platform you are working in
#'  and adjust from there. For example, Microsoft PowerPoint sets slide headings at 77 point and
#'  body text at 46 point. You will likely want to make the plot title less than 77 point so you
#'  can preserve the text hierarchy in the slide.
#'
#' # Theme modifications
#'  If you wish to make changes to this theme template, such as removing
#'  the legend, adjusting text positions,  access the source code by typing
#'  `councilR::theme_council` in the console. You can then copy/paste the
#'  function into whatever document you are working on and make changes there.
#'
#'  If you want to make a change to the default template, open an Issue on GitHub
#'  and ask for the change to be made, and/or make the change yourself and make
#'  a pull request.
#'
#'
#'
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(councilR)
#'
#' ggplot(datasets::iris) +
#'   aes(Sepal.Length, Sepal.Width, color = Species) +
#'   geom_point() +
#'   scale_color_viridis_d() +
#'   theme_council(
#'     use_showtext = TRUE,
#'     use_manual_font_sizes = TRUE
#'   )
#' }
#'
#' @importFrom ggplot2 theme element_text element_blank element_rect element_line margin unit rel
#'
#'
theme_council <- function(base_size = 11,
                          base_family = "",
                          base_line_size = base_size / 22,
                          base_rect_size = base_size / 22,
                          use_showtext = FALSE,
                          use_manual_font_sizes = FALSE,
                          font_sizes = list(
                            "title" = 22,
                            "subtitle" = 16,
                            "axis_title" = 14,
                            "axis_text" = 11,
                            "legend_title" = 14,
                            "legend_text" = 10,
                            "caption" = 8,
                            "strip" = 14
                          )) {
  if (use_showtext == TRUE) {
    requireNamespace("sysfonts", quietly = TRUE)
    requireNamespace("showtext", quietly = TRUE)

    showtext::showtext_auto()
    sysfonts::font_paths()
    files <- sysfonts::font_files()

    sysfonts::font_add("HelveticaNeueLT Std Cn", "HelveticaNeueLTStd-Cn.otf")
    sysfonts::font_add("HelveticaNeueLT Std Lt", "HelveticaNeueLTStd-Lt.otf")
    sysfonts::font_add("HelveticaNeueLT Std Med Cn", "HelveticaNeueLTStd-MdCn.otf")
    sysfonts::font_add("Arial Narrow", "ARIALN.TTF")
    sysfonts::font_add("Palatino Linotype", "pala.ttf")


    font_families <-
      list(
        "title" = "HelveticaNeueLT Std Lt",
        "subtitle" = "HelveticaNeueLT Std Cn",
        "axis_title" = "Arial Narrow",
        "axis_text" = "Arial Narrow",
        "legend_title" = "HelveticaNeueLT Std Cn",
        "legend_text" = "Arial Narrow",
        "caption" = "Arial Narrow", #"Palatino Linotype",
        "strip" = "Arial Narrow"
      )
  } else {
    font_families <-
      list(
        "title" = "sans",
        "subtitle" = "sans",
        "axis_title" = "sans",
        "axis_text" = "sans",
        "legend_title" = "sans",
        "legend_text" = "sans",
        "caption" = "sans", #"serif",
        "strip" = "sans"
      )
  }


  if (use_manual_font_sizes == FALSE) {
    font_sizes <-
      list(
        "title" = ggplot2::rel(1.2),
        "subtitle" = ggplot2::rel(1.1),
        "axis_title" = base_size,
        "axis_text" = ggplot2::rel(0.8),
        "legend_title" = base_size,
        "legend_text" = ggplot2::rel(0.8),
        "caption" = ggplot2::rel(0.8),
        "strip" = ggplot2::rel(0.8)
      )
  }


  half_line <- base_size / 2
  t <- theme(

    # SETUP -----
    line = ggplot2::element_line(
      colour = colors$suppBlack,
      size = base_line_size,
      linetype = 1,
      lineend = "butt"
    ),
    rect = ggplot2::element_rect(
      fill = colors$suppWhite,
      colour = colors$suppBlack,
      size = base_rect_size,
      linetype = 1
    ),
    text = ggplot2::element_text(
      family = base_family,
      face = "plain",
      colour = colors$suppBlack,
      size = base_size,
      lineheight = 0.9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = ggplot2::margin(),
      debug = FALSE
    ),
    # AXES -----
    axis.line = ggplot2::element_blank(),
    axis.line.x = NULL,
    axis.line.y = NULL,

    ## text-----
    axis.text = ggplot2::element_text(
      family = font_families$axis,
      size = font_sizes$axis_text,
      color = "gray30"
    ),
    axis.text.x = ggplot2::element_text(
      size = font_sizes$axis_text,
      family = font_families$axis_text,
      margin = ggplot2::margin(t = 0.8 * half_line / 2),
      vjust = 0.5
    ),
    axis.text.x.top = ggplot2::element_text(
      size = font_sizes$axis_text,
      family = font_families$axis_text,
      margin = ggplot2::margin(b = 0.8 * half_line / 2),
      vjust = 0
    ),
    axis.text.y = ggplot2::element_text(
      size = font_sizes$axis_text,
      family = font_families$axis_text,
      margin = ggplot2::margin(r = 0.8 * half_line / 2),
      hjust = 1
    ),
    axis.text.y.right = ggplot2::element_text(
      size = font_sizes$axis_text,
      family = font_families$axis_text,
      margin = ggplot2::margin(l = 0.8 * half_line / 2),
      hjust = 0
    ),

    ## ticks ----
    axis.ticks = element_line(color = "grey92"),
    axis.ticks.length = ggplot2::unit(half_line / 2, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,

    ## title-----
    axis.title.x = ggplot2::element_text(
      margin = ggplot2::margin(t = half_line / 2),
      vjust = -1,
      family = font_families$axis_title,
      size = font_sizes$axis_title
    ),
    axis.title.x.top = ggplot2::element_text(
      margin = ggplot2::margin(b = half_line / 2),
      vjust = 0,
      family = font_families$axis_title,
      size = font_sizes$axis_title
    ),
    axis.title.y = ggplot2::element_text(
      angle = 0,
      family = font_families$axis_title,
      size = font_sizes$axis_title,
      margin = ggplot2::margin(r = half_line / 2),
      vjust = .5
    ),
    axis.title.y.right = ggplot2::element_text(
      angle = -90,
      family = font_families$axis_title,
      size = font_sizes$axis_title,
      margin = ggplot2::margin(l = half_line / 2),
      vjust = 0
    ),
    # LEGEND ----
    legend.background = ggplot2::element_blank(),
    legend.spacing = ggplot2::unit(2 * half_line, "pt"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.margin = ggplot2::margin(
      half_line,
      half_line,
      half_line,
      half_line
    ),

    ## key -----
    legend.key = ggplot2::element_blank(),
    legend.key.size = ggplot2::unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,

    ## text -----
    legend.text = ggplot2::element_text(
      size = font_sizes$legend_text,
      family = font_families$axis_text
    ),
    legend.text.align = NULL,

    ## title -----
    legend.title = ggplot2::element_text(
      hjust = 0,
      family = font_families$title,
      size = font_sizes$legend_title
    ),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",

    ## box -----
    legend.box = NULL,
    legend.box.margin = ggplot2::margin(
      0, 0, 0,
      0, "cm"
    ),
    legend.box.background = ggplot2::element_blank(),
    legend.box.spacing = ggplot2::unit(2 * half_line, "pt"),

    # PANEL ----
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    panel.grid = ggplot2::element_line(colour = "grey92"),
    panel.grid.minor = element_blank(),#ggplot2::element_line(size = ggplot2::rel(0.5)),
    panel.grid.major = ggplot2::element_line(size = ggplot2::rel(1)),
    panel.spacing = ggplot2::unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,

    # STRIP -----
    strip.background = ggplot2::element_blank(),

    ## text-----
    strip.text = ggplot2::element_text(
      family = font_families$strip,
      colour = "grey10",
      size = font_sizes$strip,
      margin = ggplot2::margin(
        0.8 * half_line,
        0.8 * half_line,
        0.8 * half_line,
        0.8 * half_line
      )
    ),
    strip.text.x = NULL,
    strip.text.y = ggplot2::element_text(angle = -90),
    strip.text.y.left = ggplot2::element_text(angle = 90),

    ## placement -----
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = ggplot2::unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = ggplot2::unit(half_line / 2, "pt"),

    # PLOT -----
    plot.background = ggplot2::element_rect(colour = colors$suppWhite),

    ## title -----
    plot.title = ggplot2::element_text(
      size = font_sizes$title,
      hjust = 0,
      vjust = 1,
      family = font_families$title,
      margin = ggplot2::margin(b = half_line)
    ),
    plot.title.position = "panel",
    plot.subtitle = ggplot2::element_text(
      size = font_sizes$subtitle,
      family = font_families$subtitle,
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = half_line)
    ),

    ## caption -----
    plot.caption = ggplot2::element_text(
      hjust = 1,
      vjust = 1,
      size = font_sizes$caption,
      color = "grey30",
      family = font_families$caption,
      margin = ggplot2::margin(t = half_line)
    ),
    plot.caption.position = "panel",

    ## tag -----
    plot.tag = ggplot2::element_text(
      size = ggplot2::rel(1.2),
      hjust = 0.5,
      vjust = 0.5
    ),
    plot.tag.position = "topleft",
    plot.margin = ggplot2::margin(10, 10, 10, 10, unit = "pt"),
    complete = TRUE,
    validate = TRUE
  )

  return(t)
}



### theme_council_open
# appropriate for single scatterplots / line graphs
theme_council_open <- function(base_size = 11,
                                base_family = "",
                                base_line_size = base_size / 22,
                                base_rect_size = base_size / 22,
                                use_showtext = FALSE,
                                use_manual_font_sizes = FALSE,
                                font_sizes = list(
                                  "title" = 22,
                                  "subtitle" = 16,
                                  "axis_title" = 14,
                                  "axis_text" = 11,
                                  "legend_title" = 14,
                                  "legend_text" = 10,
                                  "caption" = 8,
                                  "strip" = 14
                                )) {

  # Starts with theme_council and then modifies some parts
  theme_council() %+replace%
    theme(
      # remove grid lines
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_blank(),

      # add axis line
      axis.line = element_line(color = "grey92"),
      complete = TRUE
    )
}
