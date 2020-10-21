testthat::test_that("error message is returnd", {

  if(DBI::dbCanConnect(odbc::odbc(), "GIS") != TRUE){
    testthat::expect_error(import_from_gis("GISTransit.dbo.PublicParcelsMetroCTUs",
                                           dbname = "GIS"))
  }

  })
