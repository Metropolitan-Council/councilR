test_that("correct class returned", {
  theme_council() %>%
    testthat::expect_s3_class("theme")
})

test_that("error on old theme", {
  council_theme() %>%
    testthat::expect_error()
})

test_that("individual color elements correct", {
  th <- theme_council()

  testthat::expect_equal(th$panel.grid$colour, "grey92")
  testthat::expect_equal(th$axis.text$colour, "gray30")
  testthat::expect_equal(th$axis.line, ggplot2::element_blank())
  testthat::expect_equal(th$plot.background$colour, "#FFFFFF")
  testthat::expect_equal(th$text$colour, "#000000")
})

test_that("individual font size elements correct", {
  th <- theme_council(
    use_manual_font_sizes = TRUE,
    font_sizes = list(
      "title" = 24,
      "subtitle" = 17,
      "axis_title" = 15,
      "axis_text" = 12,
      "legend_title" = 16,
      "legend_text" = 11,
      "caption" = 40,
      "strip" = 10
    )
  )

  testthat::expect_equal(th$plot.title$size, 24)
  testthat::expect_equal(th$plot.subtitle$size, 17)
  testthat::expect_equal(th$axis.title.x$size, 15)
  testthat::expect_equal(th$axis.title.y$size, 15)
  testthat::expect_equal(th$axis.text$size, 12)

  testthat::expect_equal(th$legend.title$size, 16)
  testthat::expect_equal(th$legend.text$size, 11)
  testthat::expect_equal(th$plot.caption$size, 40)
  testthat::expect_equal(th$strip.text$size, 10)
})

test_that("open theme is correct", {
  th <- theme_council_open(
    use_manual_font_sizes = TRUE,
    font_sizes = list(
      "title" = 24,
      "subtitle" = 17,
      "axis_title" = 15,
      "axis_text" = 12,
      "legend_title" = 16,
      "legend_text" = 11,
      "caption" = 40,
      "strip" = 10
    )
  )


  testthat::expect_equal(th$panel.grid$colour, "grey92")
  testthat::expect_equal(th$axis.text$colour, "gray30")
  testthat::expect_equal(th$plot.background$colour, "#FFFFFF")
  testthat::expect_equal(th$text$colour, "#000000")
  testthat::expect_equal(th$panel.grid.minor, ggplot2::element_blank())
  testthat::expect_equal(th$panel.grid.major, ggplot2::element_blank())
  testthat::expect_equal(th$axis.line$colour, "grey92")
})



test_that("geo theme is mostly blank", {
  th <- theme_council_geo()

  testthat::expect_equal(th$line, ggplot2::element_blank())
  testthat::expect_equal(th$rect, ggplot2::element_blank())
  testthat::expect_equal(th$panel.grid, ggplot2::element_blank())
  testthat::expect_equal(th$axis.text, ggplot2::element_blank())
})

test_that("theme_council() works without attaching councilR (#91)", {
  # Calling councilR::theme_council() without library(councilR) used to error
  # ("object of type 'closure' is not subsettable") because the bare `colors`
  # references resolved to grDevices::colors rather than the package data.
  # Run in a fresh process with no attach to guard against that regression.
  testthat::skip_if_not_installed("councilR")
  testthat::skip_if_not_installed("callr")

  th <- callr::r(function() councilR::theme_council())
  testthat::expect_s3_class(th, "theme")

  # A full plot using the theme should also build without error.
  p <- callr::r(function() {
    df <- data.frame(v1 = 1:5, v2 = 6:10)
    plot <- ggplot2::ggplot(df, ggplot2::aes(x = v1, y = v2)) +
      ggplot2::geom_point(size = 4) +
      councilR::theme_council()
    ggplot2::ggplot_build(plot)$plot$theme
    "ok"
  })
  testthat::expect_identical(p, "ok")
})
