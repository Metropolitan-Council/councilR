test_that("use_github_action_council creates directory", {
  testthat::expect_message(use_github_action_council())

  fs::dir_delete(".github/")
})
