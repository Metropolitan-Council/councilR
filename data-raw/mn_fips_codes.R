## code to prepare `mn_fips_codes` dataset goes here

load_all()

mn_fips_codes <- tigris::fips_codes %>%
  dplyr::filter(state_code == "27")


usethis::use_data(mn_fips_codes, overwrite = TRUE)
