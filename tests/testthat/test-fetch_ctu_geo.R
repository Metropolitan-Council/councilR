test_that("non T/F core fails", {
  testthat::expect_error(fetch_ctu_geo(core = "foo"))
})

test_that("nrows correct", {
  fetch_ctu_geo(year = "2020") %>%
    nrow() %>%
    testthat::expect_equal(193)
})

test_that("nrows correct", {
  fetch_ctu_geo(core = FALSE, year = "2020") %>%
    nrow() %>%
    testthat::expect_equal(235)
})
