options(
  tigris_use_cache = TRUE,
  progress_bar = FALSE,
  tigris_refresh = ifelse(testthat:::on_ci(), TRUE, FALSE)
)
