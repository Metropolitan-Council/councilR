# Can import_from_gis() actually import from the GISLibrary database?

# skip on GH Actions
testthat::skip_on_ci()

# skip if not connected to VPN
testthat::skip_if(
  is.null(curl::nslookup("mc.local"))
)

testthat::skip_if(httr2::secret_has_key("QUHBRb_yoy2RRj59qno8NVXA7mW402xkins"))

testthat::test_that("GIS connection returns connection object", {

  test_conn <- gis_connection(
    uid = httr2::secret_decrypt("QUHBRb_yoy2RRj59qno8NVXA7mW402xkins", "COUNCILR_KEY"),
    pwd = httr2::secret_decrypt("rQHk4S39pjfJ6yoKWUUNpQUDk2i9XA3d", "COUNCILR_KEY")
  )

  testthat::expect_s4_class(test_conn, "Microsoft SQL Server")

  airp_test <- DBI::dbGetQuery(test_conn, "SELECT * FROM AIRPORTS WHERE APNAME = 'Flying Cloud'")

  testthat::expect_equal(nrow(airp_test), 4)

  DBI::dbDisconnect(test_conn)


})




# test return object attributes
testthat::test_that("airports spatial dataset", {
  airport <- import_from_gis(
    uid = httr2::secret_decrypt("QUHBRb_yoy2RRj59qno8NVXA7mW402xkins", "COUNCILR_KEY"),
    pwd = httr2::secret_decrypt("rQHk4S39pjfJ6yoKWUUNpQUDk2i9XA3d", "COUNCILR_KEY"),
    query = "GISLibrary.dbo.AIRPORTS",
    dbname = "GISLibrary",
    .quiet = TRUE
  )

  # test that all airports are included
  # there should be 14 airports
  testthat::expect_equal(nrow(airport), 14)

  # test that object returned is an sf object
  testthat::expect_equal(class(airport)[[1]], "sf")
})
