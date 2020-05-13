
## many functions here come from https://github.com/gadenbuie/js4shiny/blob/master/R/utils.R


`%||%` <- function(x, y) if (is.null(x)) y else x

null_if_nothing <- function(x) {
  if (is.null(x) || identical(x, "")) NULL else x
}

is_null_or_nothing <- function(x) is.null(x) || identical(x, "")

tabs2spaces <- function(x, spaces = 2) {
  if (is.null(x)) {
    return(NULL)
  }
  gsub("\t", strrep(" ", spaces), x)
}

councilR_file <- function(...) {
  system.file(..., package = "councilR", mustWork = TRUE)
}

requires_pkg <- function(pkg) {
  calling_function <- deparse(sys.calls()[[sys.nframe() - 1]])
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(glue("{calling_function} requires the `{pkg}` package"), call. = FALSE)
  }
}

requires_pkg_version <- function(pkg, version) {
  calling_function <- deparse(sys.calls()[[sys.nframe() - 1]])
  tryCatch(requires_pkg(pkg), error = function(e) {
    stop(glue("{calling_function} requires the `{pkg}` package"), call. = FALSE)
  })
  if (utils::packageVersion(pkg) < package_version(version)) {
    stop(
      glue("{calling_function} requires `{pkg}` version {version} or later."),
      call. = FALSE
    )
  }
}

collapse <- function(..., sep_c = "\n") paste(..., collapse = sep_c)
collapse0 <- function(..., sep_c = "\n") paste(..., sep = "", collapse = sep_c)

read_lines <- function(path, ..., warn = FALSE) readLines(path, warn = warn, ...)


rstudio_gt_1.3 <- function() {
  rstudio_gt_1.3 <- FALSE
  if (rstudioapi::hasFun("versionInfo")) {
    rstudio_gt_1.3 <- rstudioapi::versionInfo()$version >= "1.3.555"
  }
  rstudio_gt_1.3
}

with_rstudio <- function(fn, ..., stopifnot = FALSE) {
  if (rstudioapi::hasFun(fn)) {
    rstudio_fun <- get(fn, asNamespace("rstudioapi"))
    tryCatch(do.call(rstudio_fun, list(...)), error = function(e) {
      stop(glue("Error in rstudioapi::{fn}(): {e$message}"), call. = FALSE)
    })
  } else {
    if (stopifnot) {
      stop(glue(
        "Your version of RStudio does not support this function: {fn}"
      ), call. = FALSE)
    }
  }
}

has_rstudio <- function(fn, stopifnot = FALSE) {
  has <- rstudioapi::hasFun(fn)
  if (!has && stopifnot) {
    stop(glue(
      "Your version of RStudio does not support this function: {fn}"
    ), call. = FALSE)
  }
  has
}

fs_dir_ls <- function(..., recurse = FALSE) {
  if (utils::packageVersion("fs") < package_version("1.3.0")) {
    recurse <- if (is.numeric(recurse) && recurse > 0) TRUE else isTRUE(recurse)
    fs::dir_ls(..., recursive = recurse)
  } else {
    fs::dir_ls(..., recurse = recurse)
  }
}
