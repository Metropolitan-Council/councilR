#' @title Object type checks, adapted from [rlang](https://github.com/r-lib/rlang/blob/main/R/standalone-types-check.R).
#' @description
#' Checks for character, string, boolean, logical, whole number, decimal number, call,
#'  name, environment, data frame, and symbol.
#'
#' @keywords internal
#' @noRd
#' @importFrom rlang caller_arg caller_env is_character env_get_list is_string is_symbol is_environment is_logical na_chr  ffi_standalone_is_bool_1.0.7  ffi_standalone_check_number_1.0.7 is_call
#' @importFrom cli cli_abort

IS_NUMBER_true <- 0
IS_NUMBER_false <- 1
IS_NUMBER_oob <- 2

.councilr_check_is_string <- function(x,
                                      allow_empty,
                                      allow_na,
                                      allow_null) {
  if (rlang::is_string(x)) {
    if (allow_empty || !rlang::is_string(x, "")) {
      return(TRUE)
    }
  }

  if (allow_null && is.null(x)) {
    return(TRUE)
  }

  if (allow_na && (identical(x, NA) || identical(x, rlang::na_chr))) {
    return(TRUE)
  }

  FALSE
}

.standalone_types_check_dot_call <- .Call
.stop_not_number <- function(x,
                             ...,
                             exit_code,
                             allow_decimal,
                             min,
                             max,
                             allow_na,
                             allow_null,
                             arg,
                             call) {
  if (allow_decimal) {
    what <- "a number"
  } else {
    what <- "a whole number"
  }

  if (exit_code == IS_NUMBER_oob) {
    min <- min %||% -Inf
    max <- max %||% Inf

    if (min > -Inf && max < Inf) {
      what <- sprintf("%s between %s and %s", what, min, max)
    } else if (x < min) {
      what <- sprintf("%s larger than or equal to %s", what, min)
    } else if (x > max) {
      what <- sprintf("%s smaller than or equal to %s", what, max)
    } else {
      cli::cli_abort("Unexpected state in OOB check", .internal = TRUE)
    }
  }

  stop_input_type(
    x,
    what,
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_character <- function(x,
                            ...,
                            allow_na = TRUE,
                            allow_null = FALSE,
                            arg = rlang::caller_arg(x),
                            call = rlang::caller_env()) {
  if (!missing(x)) {
    if (rlang::is_character(x)) {
      if (!allow_na && any(is.na(x))) {
        cli::cli_abort(
          sprintf("`%s` can't contain NA values.", arg),
          arg = arg,
          call = call
        )
      }

      return(invisible(NULL))
    }

    if (allow_null && is.null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a character vector",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}


check_string <- function(x,
                         ...,
                         allow_empty = TRUE,
                         allow_na = FALSE,
                         allow_null = FALSE,
                         arg = rlang::caller_arg(x),
                         call = rlang::caller_env()) {
  if (!missing(x)) {
    is_string <- .councilr_check_is_string(
      x,
      allow_empty = allow_empty,
      allow_na = allow_na,
      allow_null = allow_null
    )
    if (is_string) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a single string",
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_bool <- function(x,
                       ...,
                       allow_na = FALSE,
                       allow_null = FALSE,
                       arg = rlang::caller_arg(x),
                       call = rlang::caller_env()) {
  if (!missing(x) && .standalone_types_check_dot_call(
    rlang::ffi_standalone_is_bool_1.0.7,
    x, allow_na, allow_null
  )) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    c("`TRUE`", "`FALSE`"),
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}


check_number_whole <- function(x,
                               ...,
                               min = NULL,
                               max = NULL,
                               allow_infinite = FALSE,
                               allow_na = FALSE,
                               allow_null = FALSE,
                               arg = rlang::caller_arg(x),
                               call = rlang::caller_env()) {
  if (missing(x)) {
    exit_code <- IS_NUMBER_false
  } else if (0 == (exit_code <- .standalone_types_check_dot_call(
    rlang::ffi_standalone_check_number_1.0.7,
    x,
    allow_decimal = FALSE,
    min,
    max,
    allow_infinite,
    allow_na,
    allow_null
  ))) {
    return(invisible(NULL))
  }

  .stop_not_number(
    x,
    ...,
    exit_code = exit_code,
    allow_decimal = FALSE,
    min = min,
    max = max,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}


check_number_decimal <- function(x,
                                 ...,
                                 min = NULL,
                                 max = NULL,
                                 allow_infinite = TRUE,
                                 allow_na = FALSE,
                                 allow_null = FALSE,
                                 arg = rlang::caller_arg(x),
                                 call = rlang::caller_env()) {
  if (missing(x)) {
    exit_code <- IS_NUMBER_false
  } else if (0 == (exit_code <- .standalone_types_check_dot_call(
    rlang::ffi_standalone_check_number_1.0.7,
    x,
    allow_decimal = TRUE,
    min,
    max,
    allow_infinite,
    allow_na,
    allow_null
  ))) {
    return(invisible(NULL))
  }

  .stop_not_number(
    x,
    ...,
    exit_code = exit_code,
    allow_decimal = TRUE,
    min = min,
    max = max,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}




check_logical <- function(x,
                          ...,
                          allow_null = FALSE,
                          arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
  if (!missing(x)) {
    if (rlang::is_logical(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is.null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a logical vector",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}


check_data_frame <- function(x,
                             ...,
                             allow_null = FALSE,
                             arg = rlang::caller_arg(x),
                             call = rlang::caller_env()) {
  if (!missing(x)) {
    if (is.data.frame(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is.null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a data frame",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_symbol <- function(x,
                         ...,
                         allow_null = FALSE,
                         arg = rlang::caller_arg(x),
                         call = rlang::caller_env()) {
  if (!missing(x)) {
    if (rlang::is_symbol(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is.null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a symbol",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}


check_call <- function(x,
                       ...,
                       allow_null = FALSE,
                       arg = rlang::caller_arg(x),
                       call = rlang::caller_env()) {
  if (!missing(x)) {
    if (rlang::is_call(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is.null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a defused call",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}


check_name <- function(x,
                       ...,
                       allow_null = FALSE,
                       arg = rlang::caller_arg(x),
                       call = rlang::caller_env()) {
  if (!missing(x)) {
    is_string <- .councilr_check_is_string(
      x,
      allow_empty = FALSE,
      allow_na = FALSE,
      allow_null = allow_null
    )
    if (is_string) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a valid name",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}


check_environment <- function(x,
                              ...,
                              allow_null = FALSE,
                              arg = rlang::caller_arg(x),
                              call = rlang::caller_env()) {
  if (!missing(x)) {
    if (rlang::is_environment(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is.null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "an environment",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

oxford_comma <- function(chr, sep = ", ", final = "or") {
  n <- length(chr)

  if (n < 2) {
    return(chr)
  }

  head <- chr[seq_len(n - 1)]
  last <- chr[n]

  head <- paste(head, collapse = sep)

  # Write a or b. But a, b, or c.
  if (n > 2) {
    paste0(head, sep, final, " ", last)
  } else {
    paste0(head, " ", final, " ", last)
  }
}




#' @param x The object type which does not conform to `what`.
#' @param what The friendly expected type as a string. Can be a
#'   character vector of expected types, in which case the error
#'   message mentions all of them in an "or" enumeration.
#' @param show_value Passed to `value` argument of `obj_type_friendly()`.
#' @param ... Arguments passed to [cli::cli_abort()].
#' @noRd
stop_input_type <- function(x,
                            what,
                            ...,
                            allow_na = FALSE,
                            allow_null = FALSE,
                            show_value = TRUE,
                            arg = rlang::caller_arg(x),
                            call = rlang::caller_env()) {
  # From standalone-cli.R
  cli <- rlang::env_get_list(
    nms = c("format_arg", "format_code"),
    last = topenv(),
    default = function(x) sprintf("`%s`", x),
    inherit = TRUE
  )

  if (allow_na) {
    what <- c(what, cli$format_code("NA"))
  }
  if (allow_null) {
    what <- c(what, cli$format_code("NULL"))
  }
  if (length(what)) {
    what <- oxford_comma(what)
  }
  if (inherits(arg, "AsIs")) {
    format_arg <- identity
  } else {
    format_arg <- cli$format_arg
  }

  message <- sprintf(
    "%s must be %s, not %s.",
    format_arg(arg),
    what,
    typeof(x)
  )

  cli::cli_abort(message, ..., call = call, arg = arg)
}
