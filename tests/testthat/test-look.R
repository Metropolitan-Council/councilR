test_that("titanic dataset look", {
  titanic <- data.table::fread("https://raw.githubusercontent.com/Geoyi/Cleaning-Titanic-Data/master/titanic_clean.csv")

  # look at a random home.dest
  testthat::expect_message(
    titanic[look(home.dest)] # re-run the this line for a different random pull
  )
  # look at a random person
  set.seed(1)
  testthat::expect_equal(
    titanic[look(name)]$ticket,
    "359306"
  ) %>%
    suppressMessages()
  # look at a random fare and person of that fare
  set.seed(4)
  testthat::expect_equal(
    titanic[look(fare)][look(embarked)]$name,
    "Head, Mr. Christopher"
  ) %>%
    suppressMessages()
})
