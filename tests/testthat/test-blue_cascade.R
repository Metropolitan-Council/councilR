test_that("correct length", {
  length(blue_cascade) %>%
    testthat::expect_equal(8)
})

test_that("correct type", {
  testthat::expect_type(blue_cascade, "list")
})
