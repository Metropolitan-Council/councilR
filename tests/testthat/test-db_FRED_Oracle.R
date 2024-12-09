# skip on GH Actions
testthat::skip_on_ci()

# skip if not connected to VPN
testthat::skip_if(
  is.null(curl::nslookup("mc.local"))
)

testthat::skip_if(httr2::secret_has_key("O9i39Lhbau_YWJd97DlnWiPk912zkLhl9_mTqGBQhcRhVoGy9VZKKpE2oDcpso795QFs5dvrufm12dfo4ZYyPe043NkY69EonIg-XRfGbnVB"))

testthat::test_that("res permit type table direct", {
  res_permit_type <- import_from_FRED_oracle(
    table_name = "RESEARCH_WEB.RES_PERMIT_TYPE",
    dsn = httr2::secret_decrypt("cABpjoRCjYNrPb4MwWdDBM20DsU", "COUNCILR_KEY"),
    uid = httr2::secret_decrypt("cV0x5C6BA08qwix81ggi-8YNYpCghBpOg-tMS6o", "COUNCILR_KEY"),
    pwd = httr2::secret_decrypt("8zXftlj8U8YHQvQ1j-jlXc060eAe_-WcbCScZwIrqw", "COUNCILR_KEY"),
    url = httr2::secret_decrypt("O9i39Lhbau_YWJd97DlnWiPk912zkLhl9_mTqGBQhcRhVoGy9VZKKpE2oDcpso795QFs5dvrufm12dfo4ZYyPe043NkY69EonIg-XRfGbnVB", "COUNCILR_KEY")
  )

  # test that data is returned from gq unit table, currently 11837 rows
  testthat::expect_gte(nrow(res_permit_type), 16)

  # test that object returned is a data.frame
  testthat::expect_equal(class(res_permit_type)[[1]], "data.frame")
})


testthat::test_that("lowercase res permit type table direct", {
  res_permit_type <- import_from_fred_oracle(
    table_name = "RESEARCH_WEB.RES_PERMIT_TYPE",
    dsn = httr2::secret_decrypt("cABpjoRCjYNrPb4MwWdDBM20DsU", "COUNCILR_KEY"),
    uid = httr2::secret_decrypt("cV0x5C6BA08qwix81ggi-8YNYpCghBpOg-tMS6o", "COUNCILR_KEY"),
    pwd = httr2::secret_decrypt("8zXftlj8U8YHQvQ1j-jlXc060eAe_-WcbCScZwIrqw", "COUNCILR_KEY"),
    url = httr2::secret_decrypt("O9i39Lhbau_YWJd97DlnWiPk912zkLhl9_mTqGBQhcRhVoGy9VZKKpE2oDcpso795QFs5dvrufm12dfo4ZYyPe043NkY69EonIg-XRfGbnVB", "COUNCILR_KEY")
  )

  # test that data is returned from gq unit table, currently 11837 rows
  testthat::expect_gte(nrow(res_permit_type), 16)

  # test that object returned is a data.frame
  testthat::expect_equal(class(res_permit_type)[[1]], "data.frame")
})



testthat::test_that("lowercase res unit type table via SQL query", {
  fred <- fred_oracle_connection(
    dsn = httr2::secret_decrypt("cABpjoRCjYNrPb4MwWdDBM20DsU", "COUNCILR_KEY"),
    uid = httr2::secret_decrypt("cV0x5C6BA08qwix81ggi-8YNYpCghBpOg-tMS6o", "COUNCILR_KEY"),
    pwd = httr2::secret_decrypt("8zXftlj8U8YHQvQ1j-jlXc060eAe_-WcbCScZwIrqw", "COUNCILR_KEY"),
    url = httr2::secret_decrypt("O9i39Lhbau_YWJd97DlnWiPk912zkLhl9_mTqGBQhcRhVoGy9VZKKpE2oDcpso795QFs5dvrufm12dfo4ZYyPe043NkY69EonIg-XRfGbnVB", "COUNCILR_KEY")
  )

  testthat::expect_true(
    as.character(class(fred)) %in% c("JDBCConnection", "OdbcConnection")
  )

  permit_type <- DBI::dbGetQuery(fred, "SELECT * FROM RESEARCH_WEB.RES_PERMIT_TYPE WHERE RES_PERMIT_TYPE = 'FC'")

  # test that data is returned from gq unit table, currently 11837 rows
  testthat::expect_gte(nrow(permit_type), 1)

  # test that object returned is a data.frame
  testthat::expect_equal(class(permit_type)[[1]], "data.frame")

  DBI::dbDisconnect(fred)
})



testthat::test_that("uppercase res unit type table via SQL query", {
  fred <- FRED_oracle_connection(
    dsn = httr2::secret_decrypt("cABpjoRCjYNrPb4MwWdDBM20DsU", "COUNCILR_KEY"),
    uid = httr2::secret_decrypt("cV0x5C6BA08qwix81ggi-8YNYpCghBpOg-tMS6o", "COUNCILR_KEY"),
    pwd = httr2::secret_decrypt("8zXftlj8U8YHQvQ1j-jlXc060eAe_-WcbCScZwIrqw", "COUNCILR_KEY"),
    url = httr2::secret_decrypt("O9i39Lhbau_YWJd97DlnWiPk912zkLhl9_mTqGBQhcRhVoGy9VZKKpE2oDcpso795QFs5dvrufm12dfo4ZYyPe043NkY69EonIg-XRfGbnVB", "COUNCILR_KEY")
  )

  testthat::expect_true(
    as.character(class(fred)) %in% c("JDBCConnection", "OdbcConnection")
  )

  testthat::expect_s4_class(fred, "JDBCConnection")

  permit_type <- DBI::dbGetQuery(fred, "SELECT * FROM RESEARCH_WEB.RES_PERMIT_TYPE WHERE RES_PERMIT_TYPE = 'FC'")

  # test that data is returned from gq unit table, currently 11837 rows
  testthat::expect_gte(nrow(permit_type), 1)

  # test that object returned is a data.frame
  testthat::expect_equal(class(permit_type)[[1]], "data.frame")

  DBI::dbDisconnect(fred)
})
