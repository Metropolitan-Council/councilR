#' @title Fetch standardized county geography
#'
#' @param core logical, whether to include all counties in the MPO.
#' @param ... Arguments passed to `[tigris::counties]`
#' @return An `[sf]` object containing the
#' @export
#' @family spatial helpers
#' @examples
#' \dontrun{
#' fetch_county_geo()
#' }
#'
#' @note This function relies on `[rlang:::obj_type_friendly()]`, an internal function.
#'
#' @importFrom tigris counties
#' @importFrom cli cli_abort
fetch_county_geo <- function(core = TRUE, ...) {
  suppressWarnings(
    if (class(core) != "logical") {
      cli::cli_abort("`core` must be a logical, not {rlang:::obj_type_friendly(core)}.")
    }
  )

  county_list <- if (core == TRUE) {
    c(
      "Anoka",
      "Carver",
      "Dakota",
      "Hennepin",
      "Ramsey",
      "Scott",
      "Washington"
    )
  } else if (core == FALSE) {
    c(
      "Anoka",
      "Carver",
      "Dakota",
      "Hennepin",
      "Ramsey",
      "Scott",
      "Sherburne",
      "Washington",
      "Wright"
    )
  }

  # fetch county geograp
  mn_counties <- tigris::counties(state = "MN", ...)

  county_sf <- mn_counties[mn_counties$NAME %in% county_list, ]


  return(county_sf)
}
