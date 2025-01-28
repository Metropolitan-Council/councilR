# basic testing

testthat::test_that("When local == FALSE, return an error message", {
  testthat::expect_error(
    mod_2 <- import_from_emissions(
      local = FALSE,
      table_name = "metro_energy.vw_electricity_residential_ctu"
    )
  )
})


# skip on GH Actions
testthat::skip_on_ci()

# skip if not connected to VPN
testthat::skip_if(
  is.null(curl::nslookup("mc.local"))
)

testthat::skip_if(httr2::secret_has_key("QUHBRb_yoy2RRj59qno8NVXA7mW402xkins"))



testthat::test_that("emissions connection returns connection object", {
  test_conn <- emissions_connection(
    uid = httr2::secret_decrypt("QUHBRb_yoy2RRj59qno8NVXA7mW402xkins", "COUNCILR_KEY"),
    pwd = httr2::secret_decrypt("0gEYx8sYRcGetutUMIMMbfBq68homB_GRr0LW2hd", "COUNCILR_KEY")
  )

  testthat::expect_s4_class(test_conn, "Microsoft SQL Server")

  ctu_test <- DBI::dbGetQuery(test_conn, "SELECT * FROM metro_energy.vw_electricity_residential_ctu")

  testthat::expect_equal(nrow(ctu_test), 186)

  DBI::dbDisconnect(test_conn)
})




testthat::test_that("Residential electricity data is returned", {
  electric_residential <- import_from_emissions(
    uid = httr2::secret_decrypt("QUHBRb_yoy2RRj59qno8NVXA7mW402xkins", "COUNCILR_KEY"),
    pwd = httr2::secret_decrypt("0gEYx8sYRcGetutUMIMMbfBq68homB_GRr0LW2hd", "COUNCILR_KEY"),
    table_name = "metro_energy.vw_electricity_residential_ctu"
  )

  testthat::expect_equal(nrow(electric_residential), 186)
})
