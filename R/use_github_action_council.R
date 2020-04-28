#' Use GitHub Actions template
#'
#' @param type "check". the type of Action to take
#'
#' @return
#' @export
#'
#' @importFrom usethis use_github_actions_badge ui_done ui_value
#' @importFrom utils getFromNamespace
#' @examples
#' use_github_action_council()
#'
use_github_action_council <- function(type = "check",
                                      path = ".github/workflows"){

  # browser()
  check_uses_github = getFromNamespace("check_uses_github", "usethis")
  use_dot_github = getFromNamespace("use_dot_github", "usethis")
  create_directory = getFromNamespace("create_directory", "usethis")

  check_uses_github()
  use_dot_github()

  if(type == "check"){
    if(!dir.exists(path)){
      create_directory(path)
    }

    file.copy(from = system.file("templates/R-CMD-check.yaml", package = "councilR"),
              to = path,
              overwrite = TRUE,
              recursive = TRUE)
    new_path <- list.files(path, full.names = TRUE, pattern = "R-CMD*")

    usethis::ui_done("Writing {usethis::ui_path(new_path)}")

    usethis::use_github_actions_badge("R-CMD-check")
  }

}
