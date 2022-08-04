test_that("input check works", {
  testthat::expect_error(fetch_county_geo(core = "foo"))
})
