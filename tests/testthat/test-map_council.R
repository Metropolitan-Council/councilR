test_that("correct class returned", {
  fetch_ctu_geo(year = "2020") %>%
    map_council_continuous(.fill = ALAND,
                           .midpoint = 5e7,
                           .lwd = 0.5) %>%
    testthat::expect_s3_class("ggplot")
})

test_that("ggplot data same as input data ", {
  mp <- fetch_ctu_geo(year = "2020") %>%
    map_council_continuous(.fill = ALAND,
                           .midpoint = 5e7,
                           .lwd = 0.5)

  testthat::expect_equal(fetch_ctu_geo(year = "2020")$ALAND,
                         mp$data$ALAND)
})
