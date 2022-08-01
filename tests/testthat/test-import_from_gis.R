# Can import_from_gis() actually import from the GISLibrary database?

# skip on GH Actions, CRAN
testthat::skip_on_ci()
testthat::skip_on_cran()

# skip if not connected to VPN
testthat::skip_if(
  is.null(curl::nslookup("mc.local"))
)

testthat::skip_if(httr2::secret_has_key("QUHBRb_yoy2RRj59qno8NVXA7mW402xkins"))

# test return object attributes

testthat::test_that("airports spatial dataset", {
  airport <- import_from_gis(
    uid = httr2::secret_decrypt("QUHBRb_yoy2RRj59qno8NVXA7mW402xkins", "COUNCILR_KEY"),
    pwd = httr2::secret_decrypt("YXJZM6G8aHF-KTXwGiUx4Zm04qmRfERs", "COUNCILR_KEY"),
    query = "GISLibrary.dbo.AIRPORTS",
    dbname = "GISLibrary"
  )

  # test that all airports are included
  # there should be 14 airports
  testthat::expect_equal(nrow(airport), 14)

  # test that object returned is an sf object
  testthat::expect_equal(class(airport)[[1]], "sf")
})
