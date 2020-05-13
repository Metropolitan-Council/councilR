## https://github.com/gadenbuie/js4shiny/blob/master/tests/testthat/test-utils.R

test_that("requires_pkg", {
  missing_pkg <- function() requires_pkg("flarf")
  has_pkg <- function() requires_pkg("usethis")
  missing_pkg_v <- function() requires_pkg_version("flarf", "1.2")
  outdated_pkg <- function() requires_pkg_version("usethis", "1.0.0")
  uptodate_pkg <- function() requires_pkg_version("usethis", "1.6.0")

  expect_silent(has_pkg())
  expect_silent(uptodate_pkg())

  expect_error(missing_pkg(), "missing_pkg.+requires.+flarf")
  expect_error(missing_pkg_v(), "missing_pkg_v.+requires.+flarf")
  expect_null(outdated_pkg())
})
