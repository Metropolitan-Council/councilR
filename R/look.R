#' @title Look at data.table subsets
#'
#' @description A function to go into the i statement of a data.table that makes random subsets based on a chosen variable
#'
#' @param var A column from a data.table object
#' @param n numeric, How many values are used in subsetting
#' @param unique logical, Randomly choose from a unique set of levels for subsetting.
#'   Otherwise, choose from all values despite potential repetition.
#' @return A logical vector
#'
#' @examples
#' # load some data
#' library(councilR)
#' library(data.table)
#' titanic <- fread("https://raw.githubusercontent.com/Geoyi/Cleaning-Titanic-Data/master/titanic_clean.csv")
#'
#' # look at a random home.dest
#' titanic[look(home.dest)] # re-run the this line for a different random pull
#'
#' # look at a random person
#' titanic[look(name)]
#'
#' # look at a random fare and person of that fare
#' titanic[look(fare)][look(embarked)]
#'
#' @rdname look
#' @export
#' @importFrom cli cli_inform
#'
look <- function(var, n = 1, unique = TRUE) {
  argname <- sys.call()[2]
  if (unique) {
    if (length(var) > 1) {
      sampleVar <- sample(unique(var), n)
    } else {
      sampleVar <- var
    }
  } else {
    sampleVar <- sample(var, n)
  }
  cli::cli_inform(paste0(argname, " = ", paste(sampleVar, collapse = " • ")))
  var %in% sampleVar
}
