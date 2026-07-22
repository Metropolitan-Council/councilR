#' @title Council plotly formatting
#'
#' @param a_plotly [plotly::plot_ly()] object
#' @param main_title character, plot title
#' @param subtitle character, plot subtitle
#' @param x_title character, x-axis title
#' @param y_title character y-axis title
#' @param legend_title character, legend title
#' @param ... additional parameters passed to [plotly::layout()]
#'
#'
#' @note
#'   Further examples in `vignette("Color", package = "councilR")`.
#'
#'   The `subtitle` returned is an annotation, and so cannot be further modified.
#'   If you want to modify the subtitle aesthetics, leave `subtitle = ""` and
#'   add a subtitle using `plotly::layout(annotations = list(...))`.
#'
#' @return [plotly::plot_ly()] object with Council styling
#' @export
#' @importFrom plotly layout
#' @importFrom purrr map
#' @family aesthetics
#' @examples
#' \dontrun{
#'
#' library(plotly)
#' library(councilR)
#'
#' plotly::plot_ly(
#'   type = "scatter",
#'   mode = "markers",
#'   data = iris,
#'   x = ~Sepal.Length,
#'   y = ~Sepal.Width,
#'   color = ~Species,
#'   hoverinfo = "text",
#'   hovertext = ~ paste0(
#'     Species, "<br>",
#'     "Sepal Length: ", Sepal.Length, "<br>",
#'     "Sepal Width: ", Sepal.Width
#'   ),
#'   marker = list(
#'     size = 10,
#'     opacity = 0.8
#'   )
#' ) %>%
#'   plotly_layout(
#'     main_title = "Iris",
#'     subtitle = "Sepal characteristics",
#'     x_title = "Sepal Length",
#'     y_title = "Sepal Width",
#'     legend_title = "Species",
#'     legend = list(orientation = "h")
#'   )
#' }
council_layout <- function(a_plotly,
                           main_title = "",
                           subtitle = "",
                           x_title = "",
                           y_title = "",
                           legend_title = "",
                           ...) {
  purrr::map(
    list(x_title, y_title, main_title, subtitle, legend_title),
    check_character
  )

  plotly_margin <- list(
    l = 50,
    r = 50,
    b = 50,
    t = 80,
    pad = 0.5
  )

  p <- a_plotly %>%
    plotly::layout(
      margin = plotly_margin,
      barmode = "group",
      # title -----
      title = list(
        text = main_title,
        font = list(
          family = "Arial Narrow",
          size = 24
        ),
        align = "left",
        x = 0,
        y = 1.15,
        xref = "paper",
        yref = "paper"
      ),
      annotations = list(
        # subtitle -----
        list(
          text = subtitle,
          font = list(
            family = "Arial Narrow",
            size = 16
          ),
          align = "left",
          x = 0,
          y = 1.08,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE
        )
      ),
      # legend -----
      legend = list(
        title = list(
          text = legend_title,
          font = list(
            family = "Arial Narrow",
            size = 20
          )
        ),
        font = list(
          family = "Arial Narrow",
          size = 14
        ),
        orientation = "v"
      ),
      # yaxis -----
      yaxis = list(
        title = list(
          text = y_title,
          font = list(
            family = "Arial Narrow",
            size = 20
          )
        ),
        tickfont = list(
          family = "Arial Narrow",
          size = 16
        )
      ),
      ## yaxis2 -----
      yaxis2 = list(
        title = list(
          text = y_title,
          font = list(
            family = "Arial Narrow",
            size = 20
          )
        ),
        tickfont = list(
          family = "Arial Narrow",
          size = 16
        )
      ),
      ## yaxis3 ----
      yaxis3 = list(
        title = list(
          text = y_title,
          font = list(
            family = "Arial Narrow",
            size = 20
          )
        ),
        tickfont = list(
          family = "Arial Narrow",
          size = 16
        )
      ),
      ## yaxis4 ----
      yaxis4 = list(
        title = list(
          text = y_title,
          font = list(
            family = "Arial Narrow",
            size = 20
          )
        ),
        tickfont = list(
          family = "Arial Narrow",
          size = 16
        )
      ),
      # xaxis -----
      xaxis = list(
        tickfont = list(
          family = "Arial Narrow",
          size = 16
        ),
        title = list(
          text = x_title,
          font = list(
            family = "Arial Narrow",
            size = 20
          )
        )
      ),
      ## xaxis2 -----
      xaxis2 = list(
        tickfont = list(
          family = "Arial Narrow",
          size = 16
        ),
        title = list(
          text = x_title,
          font = list(
            family = "Arial Narrow",
            size = 24
          )
        )
      ),
      ## xaxis3 -----
      xaxis3 = list(
        tickfont = list(
          family = "Arial Narrow",
          size = 16
        ),
        title = list(
          text = x_title,
          font = list(
            family = "Arial Narrow",
            size = 24
          )
        )
      ),
      ## xaxis4 -----
      xaxis4 = list(
        tickfont = list(
          family = "Arial Narrow",
          size = 16
        ),
        title = list(
          text = x_title,
          font = list(
            family = "Arial Narrow",
            size = 24
          )
        )
      ),

      # hover mode ----
      hovermode = "closest",
      hoverdistance = "10",
      hoverlabel = list(
        font = list(
          size = 18,
          family = "Arial Narrow",
          color = colors$suppWhite
        ),
        # bgcolor = "white",
        stroke = list(
          colors$suppGray,
          colors$suppGray,
          colors$suppGray,
          colors$suppGray
        ),
        padding = list(l = 5, r = 5, b = 5, t = 5)
      )
    ) %>%
    # configuration -----
    plotly::config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        "drawopenpath",
        "lasso",
        "editInChartStudio",
        "sendDataToCloud",
        "zoom2d",
        "pan2d"
      )
    )

  plotly_format <- do.call(plotly::layout, list(p, ...))

  layout_names <- names(plotly_format$x$layout)

  n_y_axes <- axis_count(layout_names = layout_names, which_axis = "y")
  n_x_axes <- axis_count(layout_names = layout_names, which_axis = "x")

  # multi-axis adjustments ------
  if (sum(n_x_axes, n_y_axes) > 1) {
    if (!is.null(plotly_format$x$layout$annotations)) {
      purrr::walk(
        seq_along(plotly_format$x$layout$annotations),
        function(which_annotation) {
          # reassign font for all x annotations
          plotly_format$x$layout$annotations[[which_annotation]]$font <- list(
            family = "Arial Narrow",
            size = 18
          )
        }
      )
    }

    purrr::walk(
      paste0("xaxis", seq_len(n_x_axes)),
      function(which_ax) {
        if (!is.null(plotly_format$x$layout[[which_ax]])) {
          plotly_format$x$layout[[which_ax]]$tickfont <- list(
            family = "Arial Narrow",
            size = 14
          )
        }
      }
    )

    purrr::walk(
      paste0("yaxis", seq_len(n_y_axes)),
      function(which_ax) {
        if (!is.null(plotly_format$x$layout[[which_ax]])) {
          plotly_format$x$layout[[which_ax]]$tickfont <- list(
            family = "Arial Narrow",
            size = 14
          )
        }
      }
    )

    # if multiple axes, there may be multiple rows with x and y titles
    # determine the number of rows by dividing the number of plots by 3,
    # rounding up when not round number
    n_facet_rows <- ceiling(n_y_axes / 3)
    middle_row <- floor(mean(c(1, n_facet_rows)))

    # determine which rows to label based on the number of rows, and the number of axes
    label_which_y <- dplyr::case_when(
      middle_row == 1 ~ 2,
      middle_row == 2 ~ 4,
      middle_row == 3 ~ 7,
      middle_row == 4 ~ 10,
      middle_row == 5 ~ 13,
      TRUE ~ max(2, n_y_axes)
    )

    yaxis_to_label <- paste0("yaxis", label_which_y)
    if (!is.null(plotly_format$x$layout[[yaxis_to_label]])) {
      plotly_format$x$layout[[yaxis_to_label]]$title <- list(
        text = y_title,
        font = list(
          family = "Arial Narrow",
          size = 20
        )
      )
    }

    label_which_x <- dplyr::case_when(
      n_x_axes %% 3 == 0 ~ n_x_axes - 1,
      TRUE ~ n_x_axes
    )

    xaxis_to_label <- paste0("xaxis", label_which_x)
    if (!is.null(plotly_format$x$layout[[xaxis_to_label]])) {
      plotly_format$x$layout[[xaxis_to_label]]$title <- list(
        text = x_title,
        font = list(
          family = "Arial Narrow",
          size = 20
        )
      )
    }

    if (!is.null(plotly_format$x$layout$xaxis)) {
      plotly_format$x$layout$xaxis$title <- ""
    }

    if (!is.null(plotly_format$x$layout$yaxis)) {
      plotly_format$x$layout$yaxis$title <- ""
    }

    if (!is.null(plotly_format$x$layoutAttrs) &&
      length(plotly_format$x$layoutAttrs) >= 1 &&
      !is.null(plotly_format$x$layoutAttrs[[1]]$xaxis)) {
      plotly_format$x$layoutAttrs[[1]]$xaxis$title <- ""
    }

    if (!is.null(plotly_format$x$layoutAttrs) &&
      length(plotly_format$x$layoutAttrs) >= 1 &&
      !is.null(plotly_format$x$layoutAttrs[[1]]$yaxis)) {
      plotly_format$x$layoutAttrs[[1]]$yaxis$title <- ""
    }
  }

  return(plotly_format)
}


#' @rdname council_layout
#' @export
plotly_layout <- council_layout

#' @title Count the number of axes in a plotly layout object
#'
#' @param layout_names character vector of names from a plotly layout object
#' @param which_axis character, either "x" or "y"
#'
#' @return numeric, the number of axes in the object
#' @noRd
#' @family internal
axis_count <- function(layout_names, which_axis) {
  axis_numbers <- layout_names %>%
    .[grepl(paste0("^", which_axis, "axis"), .)] %>%
    sub(paste0("^", which_axis, "axis"), "", .) %>%
    dplyr::na_if("") %>%
    as.numeric()

  if (all(is.na(axis_numbers))) {
    return(1)
  }

  max(axis_numbers, na.rm = TRUE)
}
