x <- lapply(state.abb, \(s) tidycensus::get_decennial('state', state = s, variables = c(pop = 'P2_001N'), year = 2020))
state_2020 <- do.call('rbind', x) |>
  dplyr::transmute(
    GEOID = GEOID,
    name = NAME,
    pop = value,
    abb = state.abb
  )

usethis::use_data(state_2020, overwrite = TRUE)
