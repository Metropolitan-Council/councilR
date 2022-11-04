test_that("input check works", {
  testthat::expect_error(fetch_county_geo(core = "foo"))
})

test_that("nrows correct", {
  fetch_county_geo(year = "2020") %>%
    nrow() %>%
    testthat::expect_equal(7)
})

test_that("nrows correct", {
  fetch_county_geo(core = FALSE, year = "2020") %>%
    nrow() %>%
    testthat::expect_equal(9)
})

