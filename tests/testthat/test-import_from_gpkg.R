testthat::skip_if_offline()

if(osVersion != "windows"){

testthat::expect_warning(import_from_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/plan_parks_regional/gpkg_plan_parks_regional.zip"))

} else {
  testthat::expect_true(class(import_from_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/plan_parks_regional/gpkg_plan_parks_regional.zip"))[[1]] == "sf")
}

# testthat::expect_true(class(import_from_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/plan_parks_regional/gpkg_plan_parks_regional.zip"))[[1]] == "sf")
