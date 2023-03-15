#' @title Minnesota county FIPS codes
#'
#' @description A data.frame with county codes and names for the
#'   state of Minnesota. Derived from `{tigris}` fips_codes.
#'
#' @format A data.frame with 5 columns and 87 rows
#' \describe{
#'   \item{state}{character, state abbreviation}
#'   \item{state_code}{character, two digit state code}
#'   \item{state_name}{character, full state name}
#'   \item{county_code}{character, three digit county code}
#'   \item{county}{character, full county name}
#' }
#'
#' @family data
#' @examples
#' mn_fips_codes
#' @source `{tigris}` dataset, `fips_codes`.
"mn_fips_codes"
