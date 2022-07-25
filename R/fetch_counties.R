#' @title Fetch standardized county geography
#'
#' @param core logical, whether to include all counties in the MPO.
#' @param ... Arguments passed to `[tigris::counties]`
#' @return An `[sf]` object containing the
#' @export
#' @family spatial helpers
#' @examples
#' \dontrun{
#' fetch_counties()
#' }
#' @importFrom tigris counties
#' @importFrom dplyr filter
fetch_counties <- function(core = TRUE, ...){

  county_list <- if(core == TRUE){
    c("Anoka",
      "Carver",
      "Dakota",
      "Hennepin",
      "Ramsey",
      "Scott",
      "Washington")
  } else if(core == FALSE){
    c("Anoka",
      "Carver",
      "Dakota",
      "Hennepin",
      "Ramsey",
      "Scott",
      "Sherburne",
      "Washington",
      "Wright")
  }

  tigris::counties(state = "MN", ...) %>%
    dplyr::filter(.data$NAME %in% county_list)

}
