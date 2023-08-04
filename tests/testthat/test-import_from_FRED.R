# Can import_from_gis() actually import from the GISLibrary database?

# skip on GH Actions
testthat::skip_on_ci()

# skip if not connected to VPN
testthat::skip_if(
  is.null(curl::nslookup("mc.local"))
)

testthat::skip_if(httr2::secret_has_key("QUHBRb_yoy2RRj59qno8NVXA7mW402xkins"))


testthat::test_that("group quarters dataset", {

  gq <- import_from_FRED(
    table_name = "GQ_UNIT",
    prod = FALSE,
    uid = httr2::secret_decrypt("QUHBRb_yoy2RRj59qno8NVXA7mW402xkins", "COUNCILR_KEY"),
    pwd = httr2::secret_decrypt("rQHk4S39pjfJ6yoKWUUNpQUDk2i9XA3d", "COUNCILR_KEY")
  )

  # test that data is returned from gq unit table, currently 11837 rows
  testthat::expect_equal(nrow(gq), 11837)

  # test that object returned is a data.frame
  testthat::expect_equal(class(gq)[[1]], "data.frame")
})
