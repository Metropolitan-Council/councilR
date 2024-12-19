testthat::skip_if_offline()

testthat::test_that("metc districts with import_gpkg", {
  districts <- import_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/bdry_mcdistricts_current/gpkg_bdry_mcdistricts_current.zip")

  testthat::expect_s3_class(districts, "sf")

  testthat::expect_equal(nrow(districts), 16)
})


testthat::test_that("EJ TPP layer", {
  ej_layer <- import_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/trans_tpp2050/gpkg_trans_tpp2050.zip", layer = "RegionalEnvironmentalJusticeByCensusTract") %>%
    suppressWarnings()

  testthat::expect_s3_class(ej_layer, "sf")

  testthat::expect_equal(nrow(ej_layer), 797)
})


testthat::test_that("park and ride layer", {
  park_and_ride <- import_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/trans_park_rides_transit_centers/gpkg_trans_park_rides_transit_centers.zip") %>%
    suppressWarnings()

  testthat::expect_s3_class(park_and_ride, "sf")

  testthat::expect_gte(nrow(park_and_ride), 190)
})
