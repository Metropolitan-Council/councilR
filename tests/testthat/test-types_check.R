# adapted from rlang https://github.com/r-lib/rlang/blob/main/tests/testthat/test-standalone-types-check.R

test_that("`check_bool()` checks", {
  expect_null(check_bool(TRUE))
  expect_null(check_bool(FALSE))
  expect_null(check_bool(NA, allow_na = TRUE))
  expect_null(check_bool(NULL, allow_null = TRUE))

  expect_error(check_bool(NA))
  expect_error(check_bool(NULL))
  expect_error(check_bool(lgl(), allow_na = TRUE, allow_null = TRUE))
  expect_error(check_bool(c(TRUE, FALSE), allow_na = TRUE, allow_null = TRUE))
  expect_error(check_bool(1))
})

test_that("`check_string()` checks", {
  expect_null(check_string("foo"))
  expect_null(check_string(""))
  expect_null(check_string(NA, allow_na = TRUE))
  expect_null(check_string(NA_character_, allow_na = TRUE))
  expect_null(check_string(NULL, allow_null = TRUE))

  expect_error(check_string("", allow_empty = FALSE))
  expect_error(check_string(NA))
  expect_error(check_string(NULL))
  expect_error(check_string(rlang::chr(), allow_na = TRUE))
  expect_error(check_string(rlang::na_chr))
  expect_error(check_string(c("", ""), allow_na = TRUE, allow_null = TRUE))
  expect_error(check_string(1))
})

test_that("`check_name()` checks", {
  expect_null(check_name("foo"))

  expect_error(check_name(NULL))
  expect_error(check_name(NA))
  expect_error(check_name(rlang::chr()))
  expect_error(check_name(rlang::chr(), allow_null = TRUE))
  expect_error(check_name(rlang::na_chr))
  expect_error(check_name(c("", "")))
  expect_error(check_name(1))
  expect_error(check_name(""))
})

test_that("`check_number_whole()` checks", {
  expect_null(check_number_whole(10))
  expect_null(check_number_whole(10L))
  expect_null(check_number_whole(NA, allow_na = TRUE))
  expect_null(check_number_whole(rlang::na_dbl, allow_na = TRUE))
  expect_null(check_number_whole(rlang::na_int, allow_na = TRUE))
  expect_null(check_number_whole(NULL, allow_null = TRUE))
  expect_null(check_number_whole(Inf, allow_infinite = TRUE))
  expect_null(check_number_whole(-Inf, allow_infinite = TRUE))

  check_number_whole(0, max = 0)
  check_number_whole(0, min = 0)
  check_number_whole(1, min = 0, max = 2)

  expect_error(check_number_whole(""))
  expect_error(check_number_whole(NA))
  expect_error(check_number_whole(NULL))
  expect_error(check_number_whole(rlang::int()))
  expect_error(check_number_whole(rlang::na_dbl))
  expect_error(check_number_whole(rlang::na_int))
  expect_error(check_number_whole(10:11))
  expect_error(check_number_whole(10.5))
  expect_error(check_number_whole(1, max = 0))
  expect_error(check_number_whole(-1, min = 0))
  expect_error(check_number_whole(10, min = 1, max = 5))
  expect_error(check_number_whole(10, min = NA))
  expect_error(check_number_whole(10, min = NaN))
})

test_that("`check_number_decimal()` checks", {
  expect_null(check_number_decimal(10))
  expect_null(check_number_decimal(10L))
  expect_null(check_number_decimal(10.5))
  expect_null(check_number_decimal(NA, allow_na = TRUE))
  expect_null(check_number_decimal(rlang::na_dbl, allow_na = TRUE))
  expect_null(check_number_decimal(rlang::na_int, allow_na = TRUE))
  expect_null(check_number_decimal(NULL, allow_null = TRUE))
  expect_null(check_number_decimal(Inf))
  expect_null(check_number_decimal(-Inf))

  expect_error(check_number_decimal(rlang::int()))
  expect_error(check_number_decimal(rlang::na_dbl()))
  expect_error(check_number_decimal(rlang::na_chr()))
  expect_error(check_number_decimal(Inf, allow_infinite = FALSE))
  expect_error(check_number_decimal(-Inf, allow_infinite = FALSE))
  expect_error(check_number_decimal(10, min = NA))
  expect_error(check_number_decimal(10, max = NA))
  expect_error(check_number_decimal(10, min = NaN))
  expect_error(check_number_decimal(10, max = NaN))
  expect_error(check_number_decimal(c(10:11), allow_na = TRUE, allow_null = TRUE))
  expect_error(check_number_decimal(NA))
  expect_error(check_number_decimal(NULL))
})

test_that("`check_symbol()` checks", {
  expect_null(check_symbol(quote(foo)))
  expect_null(check_symbol(NULL, allow_null = TRUE))

  expect_error(check_symbol(NULL))
  expect_error(check_symbol(TRUE))
  expect_error(check_symbol(alist(foo, bar)))
  expect_error(check_symbol("foo"))
  expect_error(check_symbol(quote(foo())))
})

test_that("`check_call()` checks", {
  expect_null(check_call(quote(foo())))
  expect_null(check_call(NULL, allow_null = TRUE))

  expect_error(check_call(NULL))
  expect_error(check_call(TRUE))
  expect_error(check_call(alist(foo(), bar()), allow_null = TRUE))
  expect_error(check_call(quote(foo)))
})

test_that("`check_environment()` checks", {
  expect_null(check_environment(rlang::env()))
  expect_null(check_environment(NULL, allow_null = TRUE))

  expect_error(check_environment(NULL))
  expect_error(check_environment(FALSE))
  expect_error(check_environment(list(env(), env(), env()), allow_null = TRUE))
})

test_that("`check_character()` checks", {
  expect_null(check_character(""))
  expect_null(check_character(rlang::na_chr))
  expect_null(check_character(c("a", NA)))
  expect_null(check_character(rlang::chr()))
  expect_null(check_character("foo"))
  expect_null(check_character(letters))
  expect_null(check_character(NULL, allow_null = TRUE))

  expect_error(check_character(1))
  expect_error(check_character(NULL))
  expect_error(check_character(list("foo", "bar")))
  expect_error(check_character(list("a", NA), allow_na = FALSE))
  expect_error(check_character(list("a", NULL), allow_null = FALSE))
})

test_that("`check_logical()` checks", {
  expect_null(check_logical(TRUE))
  expect_null(check_logical(FALSE))
  expect_null(check_logical(rlang::na_lgl))
  expect_null(check_logical(rlang::lgl()))
  expect_null(check_logical(c(TRUE, FALSE, NA)))
  expect_null(check_logical(NULL, allow_null = TRUE))

  expect_error(check_logical(NULL))
  expect_error(check_logical(NA_integer_))
  expect_error(check_logical(1))
  expect_error(check_logical(list("foo", "bar")))
})

test_that("non-numeric types are not numbers", {
  expect_error(check_number_whole(factor("a")))
  expect_error(check_number_decimal(as.Date("2000-01-01")))
})

test_that("`check_data_frame()` checks", {
  expect_null(check_data_frame(data.frame()))
  expect_null(check_environment(NULL, allow_null = TRUE))

  expect_error(check_data_frame(list(data.frame(), data.frame(), data.frame())))
  expect_error(check_data_frame(NULL))
})
