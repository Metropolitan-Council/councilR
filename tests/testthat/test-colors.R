test_that("Council Blue is correct color code", {

  testthat::expect_equal(colors$councilBlue, "#0054A4")

})

test_that("All 29 colors are included", {

  testthat::expect_equal(length(colors), 29L)

})

