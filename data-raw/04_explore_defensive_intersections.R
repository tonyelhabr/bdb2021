
library(tidyverse)
data('pick_plays', package = 'bdb2021')
data('receiver_intersections_adj', package = 'bdb2021')
features <- file.path('inst', 'features.parquet') %>% arrow::read_parquet()

plays <- import_plays()
# pbp <- import_nflfastr_pbp()
.sec_cutoff <- 2

# Not sure why I have stuff beyond 3.5 seconds into the play. I thought I explicitly cut that out (with `n_halfseconds`)?
features_min <-
  features %>%
  filter(event %>% str_detect('sec$') & sec <= .sec_cutoff) %>%
  select(week, game_id, play_id, frame_id, sec, nfl_id, nfl_id_d_robust, dist_d_robust)
features_min

# # For "expanding" plays less than `sec_cutoff` seconds.
# id_grid <-
#   features_min %>%
#   distinct(week, game_id, play_id, nfl_id) %>%
#   crossing(sec = seq(0, .sec_cutoff, by = 0.5))
# id_grid

features_lag_init <-
  features_min %>%
  group_by(game_id, play_id, nfl_id) %>%
  mutate(
    has_same_prev_defender = if_else(nfl_id_d_robust == dplyr::lag(nfl_id_d_robust), TRUE, FALSE),
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

# # Not using this, although it would be interesting for quantifying double teams
# features_lag_n_def <-
#   features_lag_init %>%
#   group_by(week, game_id, play_id, nfl_id) %>%
#   summarize(n_def = n_distinct(nfl_id_d_robust)) %>%
#   ungroup()
# features_lag_n_def

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

features_lag %>% filter(had_intersect)

features_lag %>%
  filter(has_same_prev_defender != has_same_init_defender)
features_lag %>%
  filter(has_intersect != had_intersect)
features_lag

# # Think i'm only going with has_same_init_defender and had_intersect
# features_lag_long <-
#   features_lag %>%
#   rename(prev = has_same_prev_defender, init = has_same_init_defender) %>%
#   pivot_longer(
#     c(prev, init),
#     names_to = 'defender_cnd',
#     values_to = 'defender_lgl'
#   ) %>%
#   rename(current = has_intersect, past = had_intersect) %>%
#   pivot_longer(
#     c(current, past),
#     names_to = 'intersect_cnd',
#     values_to = 'intersect_lgl'
#   )
#
# features_lag_long %>%
#   filter(game_id == 2018090600, play_id == 146, frame_id == 31, nfl_id == 2555415)
# # features_lag_long <-
# #   features_lag %>%
# #   rename(defender_lgl = has_same_init_defender, intersect_lgl = has_intersect) %>%
# #   select(-has_same_prev_defender, -had_intersect) %>%
# #   mutate(defender_cnd = 'init', intersect_cnd = 'current')
# # features_lag_long
#
# features_lag_long %>%
#   filter(sec > 1, intersect_cnd == 'current', defender_cnd == 'init') %>%
#   filter(game_id == first(game_id), play_id == first(play_id)) %>%
#   head(20) %>%
#   # slice(c(6:25)) %>%
#   arrange(nfl_id, frame_id) # , nfl_id == first(nfl_id))

# features_lag_long_w_pick_info <-
#   features_lag_long %>%
#   # filter(sec > 0) %>%
#   inner_join(pick_plays %>% rename(sec_pick = sec, is_lo_pick = is_lo)) %>%
#   relocate(matches('^sec'), matches('is_lo'))

# features_lag_long %>%
#   filter(sec == 2) %>%
#   # filter(game_id == 2018090600, play_id == 146, frame_id == 31, nfl_id == 2555415)
#   filter(intersect_cnd == 'past') %>%
#   # count(sec, is_lo, defender_lgl, intersect_lgl)
#   group_by(sec, is_lo, defender_cnd, intersect_cnd) %>%
#   summarize(
#     n_true_defender = sum(defender_lgl),
#     n_false_defender = sum(!defender_lgl),
#     n_true_intersect = sum(intersect_lgl),
#     n_false_intersect = sum(!intersect_lgl)
#   ) %>%
#   ungroup()

# Checking frequencies
features_lag_n <-
  features_lag %>%
  filter(sec > 0) %>%
  # count(sec, is_lo, defender_cnd, defender_lgl, intersect_cnd, intersect_lgl) %>%
  # group_by(sec, is_lo, defender_cnd, intersect_cnd) %>%
  count(sec, is_lo, has_same_prev_defender, has_same_init_defender, has_intersect, had_intersect)
features_lag_n
features_lag_n %>% filter(is_lo, sec == 2)

features_lag_n_long <-
  features_lag_n %>%
  rename(prev = has_same_prev_defender, init = has_same_init_defender) %>%
  pivot_longer(
    c(prev, init),
    names_to = 'defender_cnd',
    values_to = 'defender_lgl'
  ) %>%
  rename(current = has_intersect, past = had_intersect) %>%
  pivot_longer(
    c(current, past),
    names_to = 'intersect_cnd',
    values_to = 'intersect_lgl'
  )
features_lag_n_long
fea
features_lag_n %>%
  filter(is_lo != FALSE) %>%
  # filter(intersect_cnd == 'past', defender_cnd == 'init') %>%
  mutate(grp = sprintf('%s pick? %s. Is %s frames\' defender? %s.', ifelse(intersect_cnd == 'past', 'Had', 'Is'), ifelse(intersect_lgl, 'Y', 'N'), ifelse(defender_cnd == 'init', 'initial', 'previous'), ifelse(defender_lgl, 'Y', 'N'))) %>%
  # group_by(sec) %>%
  # summarize(across(frac, sum))
  ggplot() +
  aes(x = sec, y = n) +
  geom_col(aes(fill = defender_lgl), position = 'dodge') +
  facet_grid(defender_cnd~intersect_cnd, scales = 'fixed') +
  theme_minimal()


features_lag_nn <-
  features_lag %>%
  count(sec, has_same_prev_defender, has_same_init_defender, has_intersect) %>%
  filter(sec > 0) %>%
  group_by(sec, has_same_prev_defender) %>%
  mutate(frac_prev = n / sum(n)) %>%
  ungroup() %>%
  group_by(sec, has_same_init_defender) %>%
  mutate(frac_init = n / sum(n)) %>%
  ungroup() %>%
  group_by(sec, has_intersect) %>%
  mutate(frac_intersect = n / sum(n)) %>%
  ungroup()
features_lag_nn

features_lag_n %>%
  group_by(sec, case) %>%
  summarize(n = sum(n)) %>%
  ungroup() %>%
  filter(case == first(case)) %>%
  ggplot() +
  aes(x = sec, y = n) +
  geom_col()
