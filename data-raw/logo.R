library(tidyverse)
library(tinytiger)
library(sf)
devtools::load_all()

source('data-raw/02_move_states.R')

st <- tt_states() |>
  left_join(
    state_2020,
    by = 'GEOID'
  ) |>
  filter(
    STUSPS %in% c(state.abb)
  ) |>
  mutate(seats = app_hamilton_vinton(435, pop)) |>
  mutate(state = STUSPS) |>
  st_transform(3857)

st <- st |>
  mutate(scale = sqrt(seats / max(seats)))

st <- st |> move_states(trim_hi = TRUE)

tiny <- lapply(seq_len(nrow(st)),
       function(i) place_geometry(geometry = st |> slice(i) |> st_geometry(),
                                  scale = st$scale[i])) |>
  do.call(what = 'rbind', args = _) |>
  st_sfc() |>
  `st_crs<-`('ESRI:102003')

tiny |>
  ggplot() +
  geom_sf(fill = NA, linewidth = 0.7, color = 'white') +
  ggredist::theme_map()
ggsave('data-raw/logo-states.png')
