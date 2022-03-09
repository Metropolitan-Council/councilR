# basic testing

# skip on GH Actions
testthat::skip_on_ci()

# skip if not connected to VPN
testthat::skip_if(
  is.null(curl::nslookup("mc.local"))
)

# skip if "councilR.uid" option() is not set
testthat::skip_if(is.null(getOption("councilR.uid")))
testthat::skip_if(is.null(getOption("councilR.pwd")))

# test return object attributes

airport <- import_from_gis(
  query = "GISLibrary.dbo.AIRPORTS",
  dbname = "GISLibrary")

testthat::expect_equal(nrow(airport), 14)
testthat::expect_equal(class(airport)[[1]], "sf")
