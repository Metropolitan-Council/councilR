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
#'   Further examples in `vignette("Color")`.
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

  return(do.call(plotly::layout, list(p, ...)))
}



#' @rdname council_layout
#' @export
plotly_layout <- council_layout
