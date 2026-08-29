## many functions here come from https://github.com/gadenbuie/js4shiny/blob/master/R/utils.R
# person("Garrick", "Aden-Buie", , "garrick@adenbuie.com", role = "ctb",
# comment = c(ORCID = "0000-0002-7111-0077"))

is_mac <- function() {
  grepl("mac", osVersion)
}

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

#' @importFrom rlang is_installed
requires_pkg <- function(pkg) {
  calling_function <- deparse(sys.calls()[[sys.nframe() - 1]])
  if (!rlang::is_installed(pkg)) {
    cli::cli_abort(glue::glue("{calling_function} requires the `{pkg}` package"), call. = FALSE)
  }
}

requires_pkg_version <- function(pkg, version) {
  calling_function <- deparse(sys.calls()[[sys.nframe() - 1]])
  tryCatch(requires_pkg(pkg), error = function(e) {
    cli::cli_abort(glue::glue("{calling_function} requires the `{pkg}` package"), call. = FALSE)
  })
  if (utils::packageVersion(pkg) < package_version(version)) {
    cli::cli_abort(
      glue::glue("{calling_function} requires `{pkg}` version {version} or later."),
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
      cli::cli_abort(glue::glue("Error in rstudioapi::{fn}(): {e$message}"), call. = FALSE)
    })
  } else {
    if (stopifnot) {
      cli::cli_abort(glue::glue(
        "Your version of RStudio does not support this function: {fn}"
      ), call. = FALSE)
    }
  }
}

has_rstudio <- function(fn, stopifnot = FALSE) {
  has <- rstudioapi::hasFun(fn)
  if (!has && stopifnot) {
    cli::cli_abort(glue::glue(
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

#' Download and read in an Excel file or CSV from a remote URL
#'
#' @param url character, file location URL. Must end with either ".xlsx", ".xls", or ".csv"
#' @param exdir character, directory location where to save downloaded document
#' @param force_download logical, whether to force a fresh download, regardless
#'   of whether the file exists already. Default value is `FALSE`.
#' @param ... Additional arguments passed to readxl::read_excel() or readr::read_csv
#'
#' @return tibble
#'
#' @examples
#'
#' download_read_table("https://www.dot.state.mn.us/traffic/data/reports/Current_CC_StationList.xlsx",
#' "_transportation/data-raw/mndot/",
#' sheet = 1)
#'
#'
download_read_table <- function(url,
                                exdir,
                                force_download = FALSE,
                                ...) {
  # split URL to get file name
  url_split <- strsplit(url, split = "/")
  file_name <- tail(url_split[[1]], n = 1)

  # if the downloaded file does not already OR
  # we are forcing a fresh download
  # download the file and save in exdir
  if(!file.exists(file.path(exdir, file_name)) | force_download == TRUE){
    download.file(url,
                  destfile = file.path(exdir, file_name),
                  mode = "wb"
    )
  }

  # read and return file
  if (fs::path_ext(file_name) == "csv") {
    readr::read_csv(
      file = file.path(exdir, file_name),
      ...
    )
  } else {
    readxl::read_excel(path = file.path(exdir, file_name), ...)
  }
}
