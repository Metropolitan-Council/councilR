test_plot <- plotly::plot_ly() %>%
  plotly_layout()

test_that("returns a plotly object", {
  testthat::expect_s3_class(test_plot, "plotly")
})


test_that("title font correct", {
  testthat::expect_equal(
    test_plot$x$layoutAttrs[[1]]$title$font$family,
    "Arial Narrow"
  )

  testthat::expect_equal(
    test_plot$x$layoutAttrs[[1]]$title$font$size,
    24L
  )

  testthat::expect_equal(
    test_plot$x$layoutAttrs[[1]]$title$align,
    "left"
  )
})

testthat::test_that("Hover label stroke correct", {
  testthat::expect_equal(
    unique(test_plot$x$layoutAttrs[[1]]$hoverlabel$stroke),
    list("#666666")
  )


  unique(test_plot$x$layoutAttrs[[1]]$annotations)
})

testthat::test_that("Hover label correct", {
  testthat::expect_equal(
    unique(test_plot$x$layoutAttrs[[1]]$xaxis$tickfont$family),
    "Arial Narrow"
  )

  testthat::expect_equal(
    unique(test_plot$x$layoutAttrs[[1]]$xaxis$tickfont$size),
    16L
  )
})

testthat::test_that("Legend correct", {
  testthat::expect_equal(
    test_plot$x$layoutAttrs[[1]]$legend$orientation,
    "v"
  )
})

testthat::test_that("Additional arguments applied", {
  additional_plot <- plotly::plot_ly(
    x = 1:10,
    y = 1:10,
    color = c(
      rep("green", 5),
      rep("red", 5)
    ),
    colors = c("green", "red"),
    type = "scatter",
    mode = "lines+markers"
  ) %>%
    plotly_layout(
      main_title = "Main",
      subtitle = "Sub",
      x_title = "an eXciting title",
      y_title = "Y would you do this",
      legend_title = "Legend.....ary",
      title = list(
        font = list(
          family = "Papyrus",
          size = 50
        ),
        align = "right",
        x = NULL,
        y = NULL,
        xref = "paper",
        yref = "paper"
      ),
      xaxis = list(
        title = list(
          x = 0,
          y = -1,
          xref = "paper",
          yref = "paper",
          font = list(
            family = "Rockwell",
            size = 34,
            align = "right"
          )
        )
      ),
      yaxis = list(
        title = list(
          x = 0,
          y = -1,
          xref = "paper",
          yref = "paper",
          font = list(
            family = "Comic Sans MS",
            size = 27,
            align = "right"
          )
        )
      ),
      legend = list(orientation = "h")
    )

  # additional_plot
  testthat::expect_equal(
    additional_plot$x$layoutAttrs[[1]]$legend$orientation,
    "v"
  )

  testthat::expect_equal(
    additional_plot$x$layoutAttrs[[2]]$legend$orientation,
    "h"
  )


  testthat::expect_equal(
    additional_plot$x$layoutAttrs[[2]]$title$font$family,
    "Papyrus"
  )


  testthat::expect_equal(
    additional_plot$x$layoutAttrs[[2]]$xaxis$title$font$family,
    "Rockwell"
  )
  testthat::expect_equal(
    additional_plot$x$layoutAttrs[[2]]$xaxis$title$font$align,
    "right"
  )
  testthat::expect_equal(
    additional_plot$x$layoutAttrs[[2]]$xaxis$title$font$size,
    34L
  )



  testthat::expect_equal(
    additional_plot$x$layoutAttrs[[2]]$yaxis$title$font$family,
    "Comic Sans MS"
  )
  testthat::expect_equal(
    additional_plot$x$layoutAttrs[[2]]$yaxis$title$font$align,
    "right"
  )
  testthat::expect_equal(
    additional_plot$x$layoutAttrs[[2]]$yaxis$title$font$size,
    27L
  )
})
