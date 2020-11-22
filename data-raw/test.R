
library(tidyverse)
data('receiver_intersections_relaxed_adj', package = 'bdb2021')
features <-
  arrow::read_parquet(file.path('inst', 'features.parquet'))
features

features_n <-
  features %>%
  count(week, game_id, play_id, frame_id, sec)
bad_features <- features_n %>% filter(n != 5L)
# tracking <- import_tracking(3) # Some weird stuff in week 3 (game_id == 2018092300, play_id == 740; game_id == 2018092301, play_id == 477)

# Not sure why I have stuff beyond 3.5 seconds into the play. I thought I explicitly cut that out (with `n_halfseconds`)?
features_min <-
  features %>%
  anti_join(bad_features %>% distinct(game_id, play_id)) %>%
  filter(sec < 3.5) %>%
  select(week, game_id, play_id, frame_id, sec, nfl_id, nfl_id_d, dist_d)
features_min

# For "expanding" plays less than 3 seconds.
id_grid <-
  features_min %>%
  distinct(week, game_id, play_id, nfl_id) %>%
  crossing(sec = seq(0, 3, by = 0.5))
id_grid

res <-
  features_min %>%
  full_join(
    id_grid
  ) %>%
  inner_join(
    receiver_intersections_relaxed_adj
  )
res

features_min_wide <-
  features_min %>%
  # full_join(id_grid) %>%
  select(-frame_id) %>%
  arrange(week, game_id, play_id, nfl_id) %>%
  fill(nfl_id_d, dist_d, .direction = 'down') %>%
  # crossing(sec = seq(0, 3, by = 0.5)) %>%
  # full_join(tibble(sec = seq(0, 3, by = 0.5)))
  pivot_wider(
    names_from = c('sec'),
    values_from = c('nfl_id_d', 'dist_d')
  )

features_lag <-
  features_min %>%
  group_by(game_id, play_id, nfl_id) %>%
  mutate(
    has_same_prev_defender = if_else(nfl_id_d == dplyr::lag(nfl_id_d), 1L, 0L),
    has_same_init_defender = if_else(nfl_id_d == dplyr::first(nfl_id_d), 1L, 0L)
  ) %>%
  ungroup() %>%
  left_join(
    receiver_intersections_relaxed_adj %>%
      select(week, game_id, play_id, nfl_id, nfl_id_intersect, sec = sec_end)
  ) %>%
  mutate(has_intersect = if_else(is.na(nfl_id_intersect), 0L, 1L))
features_lag

features_lag_n <-
  bind_rows(
    features_lag %>%
      count(sec, cnd = has_same_prev_defender) %>%
      mutate(case = 'has_same_prev_defender'),
    features_lag %>%
      count(sec, cnd = has_same_init_defender) %>%
      mutate(case = 'has_same_inital_defender'),
  ) %>%
  group_by(sec, case) %>%
  mutate(
    frac = n / sum(n)
  ) %>%
  ungroup() %>%
  filter(sec > 0)
features_lag_n

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
  filter(cnd == 1L) %>%
  ggplot() +
  aes(x = sec, y = frac) +
  geom_col() +
  facet_wrap(~case)

features_lag_n %>%
  group_by(sec, case) %>%
  summarize(n = sum(n)) %>%
  ungroup() %>%
  filter(case == first(case)) %>%
  ggplot() +
  aes(x = sec, y = n) +
  geom_col()

features_lag %>%
  filter(sec > 0, is.na(has_same_defender)) %>%
  select(week, game_id, play_id) %>%
  inner_join(features_min)

res_lag <-
  res %>%
  group_by(nfl_id) %>%
  mutate(
    across(nfl_id_d, list(prev = ~dplyr::lag(.x))),
    # nfl_id_d_prev = nfl_id_d %>% dplyr::lag(),
    has_same_defender = if_else(nfl_id_d == nfl_id_d_prev, 1L, 0L)
  ) %>%
  ungroup()
res
res %>% filter(sec == sec_start) %>% count(has_same_defender)

nms <- features %>% names()
cols_ball <- nms %>% str_subset('^ball_')
cols_qb <- nms %>% str_subset('^qb_')
cols_rusher <- nms %>% str_subset('rusher')
col_y <- 'idx_o_target'
cols_id <-
  c(
    'week',
    'game_id',
    'play_id',
    'frame_id'
  )
cols_static <-
  c(
    cols_id,
    'sec',
    'los',
    'yards_to_go',
    'idx_o_target',
    cols_ball,
    cols_qb,
    cols_rusher
  )


features_wide <-
  features %>%
  anti_join(bad_features) %>%
  pivot_wider(
    names_from = c('idx_o'),
    values_from  = setdiff(nms, cols_static)
  ) %>%
  mutate(across(idx_o_target, ~coalesce(.x, 101) %>% factor())) %>%
  drop_na() %>%
  mutate(idx = row_number()) %>%
  relocate(idx)
features_wide # %>% count(idx_o_target)

features_wide_min <-
  features_wide %>%
  select(
    -all_of(cols_id),
    -dplyr::matches('^is_target_'),
    -dplyr::matches('^(a|s|dir|x|y)_'),
    -dplyr::matches('^nfl_id_'),
    -dplyr::matches('^idx_o_[1-5]$'),
    -dplyr::matches('^dist_qb')
  )
