db_table_names <- list(
  # mod 1
  mod_1 = list(
    "t_ctu_forecast" = "metro_sp_mod_1.vw_ctu_forecast"
  ),


  # mod 2
  mod_2 = list(
    "t_ztrax_building_sqft" = "metro_sp_mod_2.vw_ztrax_building_sqft",
    "t_ztrax_sqft_summary_ctu" =  "metro_sp_mod_2.vw_ztrax_sqft_summary_ctu",
    "t_ztrax_sqft_summary_county" = "metro_sp_mod_2.ztrax_sqft_summary_county"
  ),

  # mod 3
  mod_3 = list(
    "aeo_factor" = "metro_sp_mod_3.aeo_factor",
    "aeo_scenario" = "metro_sp_mod_3.aeo_scenario",
    "cost_factor" = "metro_sp_mod_3.cost_factor",
    "ghg_factor" = "metro_sp_mod_3.ghg_factor",
    "mode" = "metro_sp_mod_3.mode",
    "pass_transpo" = "metro_sp_mod_3.pass_transpo",
    "sources" = "metro_sp_mod_3.sources",
    "variables" = "metro_sp_mod_3.variables"
  ),

  # metro demos
  metro_demos = list(
    "t_ctu_population" = "metro_demographic.vw_ctu_population",
    "t_ctu_qcew_ctu" =  "metro_demographic.vw_qcew_ctu",
    "t_ctu_county" = "metro_demographic.vw_ctu_county",
    "t_housing_stock_ctu"  = "metro_demographic.vw_housing_stock_ctu",
    "t_emp_forecast_county" = "metro_demographic.vw_emp_forecast_county",
    "t_emp_forecast_ctu" =  "metro_demographic.vw_emp_forecast_ctu",
    "t_led_industry_county" = "metro_demographic.vw_led_industry_county"
  ),


  # state demos
  state_demos = list(
    "t_county" = "state_demographic.county",
    "t_state_qcew" = "state_demographic.vw_state_qcew"
  ),

  # metro energy
  metro_energy = list(
    "t_electricity_residential_ctu" = " metro_energy.vw_electricity_residential_ctu",
    "t_natural_gas_residential_ctu" = "metro_energy.vw_natural_gas_residential_ctu",
    "t_intersect_landuse_utility_service_area_ctu" ="metro_energy.vw_intersect_landuse_utility_service_area_ctu",
    "t_utility_electricity_by_ctu" = "metro_energy.vw_utility_electricity_by_ctu",
    "t_eia_electricity_servicewide" = "metro_energy.vw_eia_electricity_servicewide",
    "t_mndoc_electricity_county" = "metro_energy.vw_mndoc_electricity_county",
    "t_intersect_landuse_utility_service_area_county" = "metro_energy.vw_intersect_landuse_utility_service_area_county",
    "t_nrel_energy_consumption_ctu"  = "metro_energy.vw_nrel_energy_consumption_ctu",
    "t_utility_natural_gas_by_ctu" = "metro_energy.vw_utility_natural_gas_by_ctu"
  ),

  # state energy
  state_energy = list(
    "t_eia_energy_consumption_state" = "state_energy.eia_energy_consumption_state"
  )
)

usethis::use_data(db_table_names, overwrite = T)


# db tables-----

db_tables <- fetch_db_tables(module = "all")

usethis::use_data(db_tables, overwrite = T)
