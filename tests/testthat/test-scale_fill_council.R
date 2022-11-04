test_that("correct color class", {
  scale_color_council() %>%
    testthat::expect_s3_class("ScaleDiscrete")
})

test_that("correct fill class", {
  scale_fill_council() %>%
    testthat::expect_s3_class("ScaleDiscrete")
})
