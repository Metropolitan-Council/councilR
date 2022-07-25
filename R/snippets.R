#' @title Install councilR snippets
#'  @description This function installs a set of R snippets that
#' are helpful when working with Metropolitan Council projects in in
#' RStudio. By default, the snippets are installed where RStudio will find
#' them. If you haven't previously installed snippets to RStudio, these
#' snippets will mask some of the built-in snippets that ship with RStudio.
#'
#' @note This function is adapted from
#' \href{https://github.com/gadenbuie/js4shiny/blob/master/R/snippets.R}{js4shiny::snippets_install()}.
#'
#' @section Updating Existing Snippets: If you already have snippets installed,
#'   you can  you can have the installed snippets update the existing snippets
#'   in place with `update = TRUE`. Or you can append the new snippets to the
#'   existing snippets files with `update = FALSE`. This option is desirable if
#'   you want to make sure that no snippets are overwritten. The newer snippets
#'   will mask older snippets, but no data will be lost.
#'
#' @details Snippets will only appear when writing in the snippet's language.
#'  For example, HTML snippets will not appear while editing a ".R" file, but will
#'  appear when editing a ".html" file.
#'
#' @author Garrick Aden-Buie \email{g.adenbuie@@gmail.com},
#'   Liz Roten \email{liz.roten@@metc.state.mn.us}
#'
#' @examples
#' \dontrun{
#' snip_tmp <- tempfile("snippets")
#' dir.create(snip_tmp)
#' snippets_install(snip_tmp)
#' }
#'
#' @param install_path character, Where should the snippets be installed? If `NULL`, the
#'   snippets will install to a default path based on the current version of
#'   RStudio.
#' @param update logical, Should existing snippets be updated in place if there are any
#'   conflicts? Default is yes (`TRUE`). Otherwise, new snippets are appended to
#'   the end of the existing file, ensuring that you can recover your previous
#'   snippets by editing the snippets file.
#' @aliases snippets
#' @export
#'
#' @importFrom fs path_home_r path dir_create dir_exists path_file dir_ls file_copy
#' @importFrom glue glue
#' @importFrom purrr map map_depth reduce flatten
#' @family spatial helpers
snippets_install <- function(install_path = NULL, update = TRUE) {
  # browser()
  new <- snippets_list("councilR")
  old <- snippets_list("system")
  dir <- install_path %||% snippets_dir("system")
  fs::dir_create(dir)

  for (snippet in names(new)) {
    has_mask <- snippets_warn_mask(new[snippet], old[snippet], warn = !update)
    install_to <- fs::path(dir, snippet)
    if (!fs::file_exists(install_to)) {
      fs::file_copy(new[snippet], install_to)
      message(glue::glue("Installed {snippet}"))
    } else {
      snippets <- if (update) {
        snippets_merge(new[snippet], old[snippet])
      } else {
        c("", read_lines(new[snippet]))
      }
      cat(snippets, file = install_to, sep = "\n", append = !update)
      added <- !update || !has_mask
      message(glue::glue(
        "{if (added) 'Added' else 'Updated'} snippets ",
        "{if (added) 'to' else 'in'} {snippet}",
        added = added
      ))
    }
  }
}

snippets_dir <- function(which = c("councilR", "system"), .intern = NULL) {
  # browser()
  switch(match.arg(which),
    councilR = councilR_file("snippets"),
    system = if (rstudio_gt_1.3()) {
      fs::path_home_r(".config", "rstudio", "snippets")
    } else {
      fs::path_home_r(".R", "snippets")
    }
  )
}

snippets_list <- function(which = c("", "system")) {
  # browser()
  dir <- snippets_dir(which)
  if (!fs::dir_exists(dir)) {
    return(character(0))
  }
  x <- fs::dir_ls(dir, regexp = "snippets$")
  names(x) <- fs::path_file(x)
  x
}

snippets_warn_mask <- function(new, old = NULL, warn = FALSE) {
  # returns TRUE if snippets are masked, else FALSE
  # browser()
  file_new <- fs::path_file(new)
  file_old <- fs::path_file(old)
  if (is.null(old) || is.na(old)) {
    message(glue::glue("The new {file_new} may mask the default RStudio snippets"))
    return(FALSE)
  }

  if (!identical(file_new, file_old)) {
    stop("Not a good idea to compare different snippets")
  }

  snp <-
    list(new = new, old = old) %>%
    purrr::map(snippets_read_names) %>%
    purrr::reduce(intersect)

  if (length(snp)) {
    if (warn) {
      warning(glue::glue(
        "New snippets in '{file_new}' mask older versions:",
        "{collapse(snp, sep_c = ', ')}",
        .sep = " "
      ), call. = FALSE)
    }
    TRUE
  } else {
    FALSE
  }
}

snippets_read_names <- function(path) {
  # browser()
  snp <- if (length(path) == 1) read_lines(path) else path
  snp <- grep("^snippet", snp, value = TRUE)
  gsub("^snippet ([^ ]+).*", "\\1", snp)
}

snippets_merge <- function(new, old) {
  if (is.null(old) || is.na(old) || !fs::file_exists(old)) {
    return(read_lines(new))
  }

  snps <-
    list(new = new, old = old) %>%
    purrr::map(read_lines) %>%
    purrr::map(collapse) %>%
    purrr::map(trimws) %>%
    purrr::map(strsplit, split = "(^|\n)snippet ") %>%
    purrr::map_depth(2, ~ .x[.x != ""]) %>%
    purrr::map_depth(2, ~ {
      nm <- gsub("^([^\n\t ]+).*", "\\1", .x)
      names(.x) <- nm
      .x
    }) %>%
    purrr::flatten()

  new_snps <- setdiff(names(snps$new), names(snps$old))
  merged <- c()
  str2snippet <- function(str) {
    str <- ifelse(substr(str, 1, 1) != "#", paste("snippet", str), str)
    strsplit(collapse(str), "\n")[[1]]
  }
  for (snippet in unique(names(snps$old))) {
    if (snippet %in% names(snps$new)) {
      # choose new snippet
      merged <- c(merged, str2snippet(snps$new[[snippet]]))
    } else {
      # choose last old snippet
      old_snippet <- snps$old[which(snippet == names(snps$old))]
      old_snippet <- old_snippet[[length(old_snippet)]]
      merged <- c(merged, str2snippet(old_snippet))
    }
  }
  if (length(new_snps)) {
    for (snippet in new_snps) {
      merged <- c(merged, str2snippet(snps$new[[snippet]]))
    }
  }
  merged
}
