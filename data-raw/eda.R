
library(tidyverse)
data('receiver_intersections_relaxed', package = 'bdb2021')
data('closest_defenders', package = 'bdb2021')
data('personnel_and_rushers', package = 'bdb2021')
receiver_intersections_relaxed
closest_defenders
personnel_and_rushers
features <- arrow::read_parquet(file.path('inst', 'features.parquet'))
features

plays <- import_plays()
# pbp <-
#   import_nflfastr_pbp() # %>%
#   # select(game_id = old_game_id, play_id, wpa, epa) %>%
#   # mutate(across(c(game_id, play_id), as.integer))
# pbp

pick_play_meta_init <-
  receiver_intersections_relaxed %>%
  filter(has_intersection) %>%
  unnest(intersection) %>%
  distinct(week, game_id, play_id, sec_end = sec, nfl_id, nfl_id_intersect) %>%
  # Data should be dropped with this join because we are only joining on one `sec` interval per play and `y_side`.
  inner_join(
    pick_play_ids_adj,
    by = c('game_id', 'play_id', 'nfl_id', 'nfl_id_intersect', 'sec_end')
  ) %>%
  relocate(.data$sec_start, .data$sec_end, .after = dplyr::last_col()) %>%
  # Just get the `target_nfl_id` first.
  inner_join(
    plays %>%
      select(game_id, play_id, target_nfl_id),
    by = c('game_id', 'play_id')
  ) %>%
  group_by(game_id, play_id, nfl_id, nfl_id_intersect, sec_start, sec_end) %>%
  # There will be some NAs here due to missing `target_nfl_id`s.
  # I think it's best to leave these in.
  summarize(
    target_is_intersect = sum(target_nfl_id == nfl_id)
  ) %>%
  ungroup()
pick_play_meta_init

plays_w_pick_info <-
  plays %>%
  left_join(
    pick_play_meta_init %>%
      nest(pick_play = -c(game_id, play_id)),
    by = c('game_id', 'play_id')
  ) %>%
  left_join(
    personnel_and_rushers %>%
      # Remove unneeded columns.
      # mutate(
      #   rushers = map(rushers, ~select(.x, nfl_id, dist_to_ball_abs, idx_closest_to_ball))
      # ),
      select(-rushers) %>%
      rename(n_qb_rusher = n_qb),
    by = c('game_id', 'play_id')
  ) %>%
  mutate(
    is_pick_play = map_lgl(pick_play, ~!is.null(.x)),
    across(n_rusher, ~coalesce(.x, 0L)),
    # pick_play = map_if(pick_play, is.null, ~tibble())
  ) %>%
  relocate(week, game_id, play_id, is_pick_play, pick_play)
plays_w_pick_info
plays_w_pick_info %>% filter(!is_pick_play)

pick_play_meta <-
  pick_play_meta_init %>%
  # Now get other numbers from `plays`.
  inner_join(
    plays %>%
      select(game_id, play_id, pass_result, epa, possession_team, quarter, down, yards_to_go),
    by = c('game_id', 'play_id')
  ) %>%
  left_join(
    pbp %>% rename(wpa_nflfastr = wpa, epa_nflfastr = epa),
    by = c('game_id', 'play_id')
  )
pick_play_meta

# TODO: Add this removal of bad plays to the generate features script.
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

features_lag <-
  features_min %>%
  group_by(game_id, play_id, nfl_id) %>%
  mutate(
    has_same_prev_defender = if_else(nfl_id_d == dplyr::lag(nfl_id_d), TRUE, FALSE),
    has_same_init_defender = if_else(nfl_id_d == dplyr::first(nfl_id_d), TRUE, FALSE)
  ) %>%
  ungroup() %>%
  mutate(across(has_same_init_defender, ~if_else(sec == 0, NA, .x))) %>%
  left_join(
    bind_rows(
      receiver_intersections_relaxed_adj %>%
        select(week, game_id, play_id, nfl_id, sec = sec_end),
      receiver_intersections_relaxed_adj %>%
        select(week, game_id, play_id, nfl_id = nfl_id_intersect, sec = sec_end)
    ) %>%
      mutate(has_intersect = TRUE) %>%
      arrange(week, game_id, play_id, nfl_id, sec)
  ) %>%
  mutate(
    across(has_intersect, ~coalesce(.x, FALSE)),
    across(has_intersect, ~if_else(sec == 0, NA, .x))
  )
features_lag

features_lag <-
  features_lag %>%
  left_join(
    features_lag %>%
      filter(has_intersect) %>%
      select(-sec) %>%
      mutate(had_intersect = TRUE)
  ) %>%
  mutate(
    across(
      had_intersect,
      ~case_when(
        is.na(has_intersect) ~ NA,
        !has_intersect ~ FALSE,
        is.na(had_intersect) ~ FALSE,
        TRUE ~ .x)
    )
  )
features_lag

features_lag_long <-
  features_lag %>%
  select(-nfl_id_intersect) %>%
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
features_lag_long %>% filter(sec > 0) %>% filter(intersect_cnd == 'past')

features_lag_n <-
  features_lag_long %>%
  filter(sec > 0) %>%
  count(sec, defender_cnd, defender_lgl, intersect_cnd, intersect_lgl) %>%
  group_by(sec, defender_cnd, intersect_cnd) %>%
  mutate(
    total = sum(n),
    frac = n / total
  ) %>%
  ungroup()
  # mutate(
  #   frac_defender = n / sum(n)
  # ) %>%
  # group_by(sec, intersect_cnd) %>%
  # mutate(
  #   frac_intersect = n / sum(n)
  # ) %>%
  # ungroup()
features_lag_n %>% filter(intersect_cnd == 'past')
features_lag_n %>%
  filter(intersect_cnd == 'past', defender_cnd == 'init') %>%
  mutate(grp = sprintf('Is pick? %s. Is initial defender? %s.', ifelse(intersect_lgl, 'Y', 'N'), ifelse(defender_lgl, 'Y', 'N'))) %>%
  # group_by(sec) %>%
  # summarize(across(frac, sum))
  ggplot() +
  aes(x = sec, y = n) +
  geom_col(aes(fill = grp), show.legend = FALSE) +
  facet_wrap(~grp, scales = 'free')


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
  filter(cnd == TRUE) %>%
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
    has_same_defender = if_else(nfl_id_d == nfl_id_d_prev, TRUE, FALSE)
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
