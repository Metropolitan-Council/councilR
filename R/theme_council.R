#' @title Council ggplot2 theme
#'
#' @description The default `theme_council()` plus a more simple `theme_council_open()` for making MetCouncil figures. `theme_council()` will be appropriate in most cases while `theme_council_open()` is appropriate for single scatter plots or line graphs. For geospatial plots, `theme_council_geo()` may be useful to set some initial parameters.
#'
#' Please note that the y-axis text is horizontal, and long axis names will need to be wrapped;  [stringr::str_wrap()] is useful for managing length.
#'  For example, consider using this piece of code: `labs(y = stringr::str_wrap("Axis labels are now horizontal, but you still need to insert some code to wrap long labels", width = 15))`
#'
#' @param base_size numeric, base font size, given in pts. Default is `11`
#' @param base_family character, base font family. Default is `""`
#' @param base_line_size numeric, base size for line elements. Default is `base_size/22`
#' @param base_rect_size numeric, base size for `rect` elements. Default is `base_size/22`
#' @param use_showtext logical, whether to use Council fonts.
#' @param use_manual_font_sizes logical, use supplied font sizes.
#' @param font_sizes named list, font sizes. Only used if `use_manual_font_sizes` is `TRUE`
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
#' @note Further examples in `vignette("Color")`.
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
#'
#' fetch_ctu_geo() %>%
#'   ggplot() +
#'   geom_sf() +
#'   theme_council_geo()
#' }
#'
#' @importFrom ggplot2 theme element_text element_blank element_rect element_line margin unit rel %+replace% theme_void
#' @importFrom purrr map map2
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
  purrr::map(
    c(use_showtext, use_manual_font_sizes),
    check_bool
  )

  purrr::map(
    c(base_size),
    check_number_decimal
  )

  check_string(base_family)

  if (use_showtext == TRUE) {
    requireNamespace("sysfonts", quietly = TRUE)
    requireNamespace("showtext", quietly = TRUE)

    showtext::showtext_auto()
    if (is_mac()) {
      # if mac, search default font paths
      sysfonts::font_paths()
    } else {
      # if windows, add the user-level font files to font paths
      sysfonts::font_paths(
        paste0(
          "C:\\Users\\",
          Sys.info()["user"],
          "\\AppData\\Local\\Microsoft\\Windows\\Fonts"
        )
      )
    }


    # find font files for given families
    font_locs <- subset(
      sysfonts::font_files(),
      family %in% c(
        "HelveticaNeueLT Std Cn",
        "HelveticaNeueLT Std Lt",
        "Arial Narrow"
      ) &
        face == "Regular"
    )

    # add each font to the sysfonts database
    purrr::map2(
      font_locs$family,
      font_locs$file,
      sysfonts::font_add
    )

    font_families <-
      list(
        "title" = "HelveticaNeueLT Std Lt",
        "subtitle" = "HelveticaNeueLT Std Cn",
        "axis_title" = "Arial Narrow",
        "axis_text" = "Arial Narrow",
        "legend_title" = "HelveticaNeueLT Std Cn",
        "legend_text" = "Arial Narrow",
        "caption" = "Arial Narrow",
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
        "caption" = "sans", # "serif",
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
  t <- ggplot2::theme(

    # SETUP -----
    line = ggplot2::element_line(
      colour = colors$suppBlack,
      linewidth = base_line_size,
      linetype = 1,
      lineend = "butt"
    ),
    rect = ggplot2::element_rect(
      fill = colors$suppWhite,
      colour = colors$suppBlack,
      linewidth = base_rect_size,
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
    axis.ticks = ggplot2::element_line(color = "grey92"),
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
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(linewidth = ggplot2::rel(1)),
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

#' @rdname theme_council
#' @export
#' @param ... arguments passed to `theme_council()`
theme_council_open <- function(...) {
  # Starts with theme_council and then modifies some parts
  ggplot2::`%+replace%`(
    theme_council(...),
    ggplot2::theme(
      # remove grid lines
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),

      # add axis line
      axis.line = ggplot2::element_line(color = "grey92"),
      complete = TRUE
    )
  )
}

#' @rdname theme_council
#' @export
#'
theme_council_geo <- function(...) {
  # Starts with theme_council() then modifies
  # to match theme_void()

  ggplot2::`%+replace%`(
    theme_council(...),
    ggplot2::theme(
      line = ggplot2::element_blank(),
      rect = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks.length = unit(0, "pt"),
      axis.ticks.length.x = NULL,
      axis.ticks.length.x.top = NULL,
      axis.ticks.length.x.bottom = NULL,
      axis.ticks.length.y = NULL,
      axis.ticks.length.y.left = NULL,
      axis.ticks.length.y.right = NULL,
      axis.text = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.x.bottom = ggplot2::element_blank(),
      axis.text.x.top = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.text.y.left = ggplot2::element_blank(),
      axis.text.y.right = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),

      # legend.title = ggplot2::element_text(size = 6),
      # legend.text = ggplot2::element_text(size = 6),
      legend.key.size = ggplot2::unit(.75, "lines"),
      complete = TRUE
    )
  )
}
