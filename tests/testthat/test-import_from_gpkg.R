testthat::skip_if_offline()

testthat::test_that("metc districts with import from",{
  districts <- import_from_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/bdry_mcdistricts_2013/gpkg_bdry_mcdistricts_2013.zip")

  testthat::expect_s3_class(districts, "sf")

  testthat::expect_equal(nrow(districts), 16)
})



testthat::test_that("metc districts with import_gpkg",{
  districts <- import_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/bdry_mcdistricts_2013/gpkg_bdry_mcdistricts_2013.zip")

  testthat::expect_s3_class(districts, "sf")

  testthat::expect_equal(nrow(districts), 16)
})
