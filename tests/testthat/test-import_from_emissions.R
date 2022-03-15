# basic testing

testthat::expect_error(
  import_from_emissions(
    local = FALSE,
    module = "mod_1"
  )
)


# skip on GH Actions
testthat::skip_on_ci()

# skip if not connected to VPN
testthat::skip_if(
  is.null(curl::nslookup("mc.local"))
)

# skip if "councilR.uid" option() is not set
testthat::skip_if(is.null(getOption("councilR.uid")))
testthat::skip_if(is.null(getOption("councilR.pwd")))


mod_2 <- import_from_emissions(
  module = "mod_2"
)

testthat::expect_equal(length(mod_2), 3)
