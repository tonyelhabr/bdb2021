

# TODO: Need to combine this with the prior script...
library(tidyverse)
data('pick_plays', package = 'bdb2021')
data('receiver_intersections_adj', package = 'bdb2021')
features <- file.path('inst', 'features.parquet') %>% arrow::read_parquet()

plays <- import_plays()
pbp <- import_nflfastr_pbp()
.sec_cutoff <- 2

# Only want the seconds frames, not the other event frames.
events_end_rush <- .get_events_end_rush()
features_min <-
  features %>%
  filter(event %in% c('0.0 sec', events_end_rush)) %>%
  select(week, game_id, play_id, event, frame_id, sec, nfl_id, nfl_id_d_robust) %>%
  # If there are multiple of these `events_end_rush` on the same play, then just pick the first
  group_by(game_id, play_id, nfl_id, event) %>%
  # filter(frame_id == min(frame_id)) %>%
  filter(row_number() == 1L) %>%
  ungroup()
features_min

features_min %>%
  count(game_id, play_id, nfl_id, event) %>%
  count(n, name = 'nn')


# Adding the `_defender` and `_intersect` columns. Need to join on itself in order to get `had_intersect`, hence the `_init` added to the variable name.
features_lag_init <-
  features_min %>%
  group_by(game_id, play_id, nfl_id) %>%
  mutate(
    has_same_init_defender = if_else(nfl_id_d_robust == dplyr::first(nfl_id_d_robust), TRUE, FALSE)
  ) %>%
  ungroup() %>%
  mutate(across(has_same_init_defender, ~if_else(sec == 0, NA, .x))) %>%
  left_join(
    receiver_intersections_adj %>%
      select(week, game_id, play_id, nfl_id, sec, is_lo) %>%
      mutate(has_intersect = TRUE)
  ) %>%
  mutate(
    across(has_intersect, ~coalesce(.x, FALSE)),
    across(has_intersect, ~if_else(sec == 0, NA, .x))
  )
features_lag_init
features_lag_init %>% filter(sec != 0)
# # Not using this, although it would be interesting for quantifying double teams
# features_lag_n_def <-
#   features_lag_init %>%
#   group_by(week, game_id, play_id, nfl_id) %>%
#   summarize(n_def = n_distinct(nfl_id_d_robust)) %>%
#   ungroup()
# features_lag_n_def

# .join_epa <- function(data) {
#   data %>%
#     inner_join(
#       plays %>%
#         select(game_id, play_id, pass_result, epa),
#       by = c('game_id', 'play_id')
#     ) %>%
#     left_join(
#       pbp %>%
#         select(game_id, play_id, wpa_nflfastr = wpa, epa_nflfastr = epa),
#       by = c('game_id', 'play_id')
#     )
# }

# This is to include all seconds distinctly. Adding epa in case it is useful at some point.
# `pick_plays` has epa
features_lag <-
  features_lag_init %>%
  # filter(game_id == 2018090600, play_id == 521, nfl_id == 2507828) %>%
  left_join(
    features_lag_init %>%
      filter(has_intersect) %>%
      select(week, game_id, play_id, frame_id, nfl_id, nfl_id_d_robust) %>%
      mutate(had_intersect = TRUE)
  ) %>%
  group_by(game_id, play_id, nfl_id) %>%
  arrange(frame_id, .by_group = TRUE) %>%
  fill(had_intersect) %>%
  ungroup() %>%
  # .join_epa()
  inner_join(
    plays %>%
      select(game_id, play_id, pass_result, epa),
    by = c('game_id', 'play_id')
  ) %>%
  left_join(
    pbp %>%
      select(game_id, play_id, wpa_nflfastr = wpa, epa_nflfastr = epa),
    by = c('game_id', 'play_id')
  )
features_lag

# Aggregating over seconds
features_w_pick_info_agg <-
  features_lag %>%
  filter(sec > 0) %>%
  group_by(week, game_id, play_id, nfl_id) %>%
  summarize(
    # This isn't really the last frame unless it coincides with the pass throw
    frame_id_last = max(frame_id),
    sec_last = max(sec),
    dist_d_robust = last(dist_d_robust),
    n_defender = n_distinct(nfl_id_d_robust),
    has_intersect = any(has_intersect, na.rm = TRUE),
    is_lo = any(is_lo, na.rm = TRUE),
    has_same_init_defender = any(has_same_init_defender, na.rm = TRUE)
  ) %>%
  ungroup()
features_w_pick_info_agg # %>% left_join(plays_w_pick_info)
usethis::use_data(features_w_pick_info_agg, overwrite = TRUE)
