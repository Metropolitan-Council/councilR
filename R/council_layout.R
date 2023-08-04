#' Council
#'
#' @param a_plotly [plotly::plot_ly()] object
#' @param main_title character
#' @param subtitle character
#' @param x_title character
#' @param y_title character
#' @param legend_title character
#' @param ... additional parameters passed to [plotly::layout()]
#' @return
#' @export
#' @importFrom plotly layout
#' @family aesthetics
#' @examples
council_layout <- function(a_plotly,
                           main_title = "",
                           subtitle = "",
                           x_title = "",
                           y_title = "",
                           legend_title = "",
                           layout_args = list()){
  browser()
  purrr::map(
    list(x_title, y_title, main_title, subtitle, legend_title),
    rlang:::check_character
  )

  plotly_margin <- list(
    l = 50,
    r = 50,
    b = 50,
    t = 80,
    pad = 0.5
  )

  a_plotly %>%
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
        )
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
    plotly::layout(...) %>%
    plotly::config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        "drawopenpath", "lasso",
        "editInChartStudio", "sendDataToCloud",
        "zoom2d", "pan2d"
      )
    )
}



#' @rdname council_layout
#' @export
plotly_layout <- council_layout



pkgload::load_all()
library(plotly)
library(dplyr)
library(palmerpenguins)

penguins <- palmerpenguins::penguins

plot_ly(
  type = "scatter",
  mode = "markers",
  data = penguins,
  x  = ~flipper_length_mm,
  y = ~body_mass_g/100,
  color = ~species,
  hoverinfo = "text",
  hovertext = ~paste0(
    species, " observed on ", island,  "<br>",
    "Flipper length: ", flipper_length_mm, " mm<br>",
    "Body mass: ", scales::comma(body_mass_g/100), " kg"
  ),
  marker = list(
    size = 10,
    opacity = 0.8
  )) %>%
  plotly_layout(
    main_title = "Palmer penguins",
    subtitle = "",
    x_title = "Flipper Length (mm)",
    y_title = "Body Mass (kg)",
    legend_title = "Species",
    layout_args = list(
      legend = list(
        orientation = "h"
      ))
  )
