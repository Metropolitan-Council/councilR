# basic testing

test_that("When local == FALSE, return an error message", {
  testthat::expect_error(
    mod_2 <- import_from_emissions(
      local = FALSE,
      table_name = "metro_energy.vw_electricity_residential_ctu"
    )
  )
})


# skip on GH Actions
testthat::skip_on_ci()
testthat::skip_on_cran()

# skip if not connected to VPN
testthat::skip_if(
  is.null(curl::nslookup("mc.local"))
)

testthat::skip_if(httr2::secret_has_key("QUHBRb_yoy2RRj59qno8NVXA7mW402xkins"))

testthat::test_that("Residential electricity data is returned", {
  electric_residential <- import_from_emissions(
    uid = httr2::secret_decrypt("QUHBRb_yoy2RRj59qno8NVXA7mW402xkins", "COUNCILR_KEY"),
    pwd = httr2::secret_decrypt("IMcfHFuibrH6IsuwIE_vd9oJmNTlOkpt", "COUNCILR_KEY"),
    table_name = "metro_energy.vw_electricity_residential_ctu"
  )

  testthat::expect_equal(nrow(electric_residential), 186)
})
