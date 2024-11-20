testthat::skip_if_offline()


testthat::test_that("metc districts with import_gpkg", {
  districts <- import_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/bdry_mcdistricts_2013/gpkg_bdry_mcdistricts_2013.zip")

  testthat::expect_s3_class(districts, "sf")

  testthat::expect_equal(nrow(districts), 16)
})



testthat::test_that("EJ TPP layer",  {
  ej_layer <- import_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/trans_tpp2050/gpkg_trans_tpp2050.zip", layer = "RegionalEnvironmentalJusticeByCensusTract") %>%
    suppressWarnings()

  testthat::expect_s3_class(ej_layer, "sf")

  testthat::expect_equal(nrow(ej_layer), 797)
})


testthat::test_that("AADT layer",  {
  aadt <- import_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dot/trans_aadt_traffic_count_locs/gpkg_trans_aadt_traffic_count_locs.zip") %>%
    suppressWarnings()

  testthat::expect_s3_class(aadt, "sf")

  testthat::expect_gte(nrow(aadt), 39728)
})


