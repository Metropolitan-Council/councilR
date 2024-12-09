# Can import_from_gis() actually import from the GISLibrary database?

# skip on GH Actions
testthat::skip_on_ci()

# skip if not connected to VPN
testthat::skip_if(
  is.null(curl::nslookup("mc.local"))
)

testthat::skip_if(httr2::secret_has_key("QUHBRb_yoy2RRj59qno8NVXA7mW402xkins"))


testthat::test_that("upercase group quarters table", {
  gq <- import_from_FRED(
    table_name = "GQ_UNIT",
    prod = TRUE,
    uid = httr2::secret_decrypt("QUHBRb_yoy2RRj59qno8NVXA7mW402xkins", "COUNCILR_KEY"),
    pwd = httr2::secret_decrypt("tsGlVlM0KBdLcUPNLX5f0Hll2_6HFF_rQx62cw", "COUNCILR_KEY"),
    db = "CD_RESEARCH_WEB"
  )

  # test that data is returned from gq unit table, currently 11837 rows
  testthat::expect_gte(nrow(gq), 11837)

  # test that object returned is a data.frame
  testthat::expect_equal(class(gq)[[1]], "data.frame")
})


testthat::test_that("lowercase group quarters table", {
  gq <- import_from_fred(
    table_name = "GQ_UNIT",
    prod = TRUE,
    uid = httr2::secret_decrypt("QUHBRb_yoy2RRj59qno8NVXA7mW402xkins", "COUNCILR_KEY"),
    pwd = httr2::secret_decrypt("tsGlVlM0KBdLcUPNLX5f0Hll2_6HFF_rQx62cw", "COUNCILR_KEY"),
    db = "CD_RESEARCH_WEB"
  )

  # test that data is returned from gq unit table, currently 11837 rows
  testthat::expect_gte(nrow(gq), 11837)

  # test that object returned is a data.frame
  testthat::expect_equal(class(gq)[[1]], "data.frame")
})


testthat::test_that("lowercase park unit via SQL", {
  fred <- fred_connection(
    uid = httr2::secret_decrypt("QUHBRb_yoy2RRj59qno8NVXA7mW402xkins", "COUNCILR_KEY"),
    pwd = httr2::secret_decrypt("tsGlVlM0KBdLcUPNLX5f0Hll2_6HFF_rQx62cw", "COUNCILR_KEY"),
    db = "CD_RESEARCH_WEB"
  )

  testthat::expect_s4_class(fred, "Microsoft SQL Server")

  park_unit <- DBI::dbGetQuery(fred, "SELECT * FROM PARK_UNIT WHERE PARK_UNIT_ID_CURRENT = '094'")

  # test that data is returned from gq unit table, currently 11837 rows
  testthat::expect_gte(nrow(park_unit), 1)

  # test that object returned is a data.frame
  testthat::expect_equal(class(park_unit)[[1]], "data.frame")

  DBI::dbDisconnect(fred)
})



testthat::test_that("uppercase park unit via SQL", {
  fred <- FRED_connection(
    uid = httr2::secret_decrypt("QUHBRb_yoy2RRj59qno8NVXA7mW402xkins", "COUNCILR_KEY"),
    pwd = httr2::secret_decrypt("tsGlVlM0KBdLcUPNLX5f0Hll2_6HFF_rQx62cw", "COUNCILR_KEY"),
    db = "CD_RESEARCH_WEB"
  )


  testthat::expect_s4_class(fred, "Microsoft SQL Server")

  park_unit <- DBI::dbGetQuery(fred, "SELECT * FROM MANUFACTURED_HOUSING_PARK WHERE PARK_ID_CURRENT = 23")

  # test that data is returned from gq unit table, currently 11837 rows
  testthat::expect_gte(nrow(park_unit), 1)

  # test that object returned is a data.frame
  testthat::expect_equal(class(park_unit)[[1]], "data.frame")

  DBI::dbDisconnect(fred)
})
