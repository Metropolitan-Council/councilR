# basic testing

testthat::expect_error(
  mod_2 <- import_from_emissions(
    local = FALSE,
    table_name = "metro_energy.vw_electricity_residential_ctu"
  )
)


# skip on GH Actions
testthat::skip_on_ci()

# skip if not connected to VPN
testthat::skip_if(
  is.null(curl::nslookup("mc.local"))
)

# skip if "councilR.uid" option() is not set
testthat::skip_if(is.null(getOption("councilR.uid")))
testthat::skip_if(is.null(getOption("councilR.pwd")))


electric_residential <- import_from_emissions(
  table_name = "metro_energy.vw_electricity_residential_ctu"
)

testthat::expect_equal(nrow(electric_residential), 186)
