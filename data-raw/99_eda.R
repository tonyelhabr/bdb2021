
library(tidyverse)
data('receiver_intersections_relaxed', package = 'bdb2021')
data('personnel_and_rushers', package = 'bdb2021')
data('players_from_tracking', package = 'bdb2021')
features <- arrow::read_parquet(file.path('inst', 'features.parquet'))
plays <- import_plays()
pbp <- import_nflfastr_pbp()

pick_play_meta_init <-
  receiver_intersections_relaxed_adj %>%
  # Just get the `target_nfl_id` first.
  inner_join(
    plays %>%
      select(game_id, play_id, target_nfl_id),
    by = c('game_id', 'play_id')
  ) %>%
  group_by(game_id, play_id, nfl_id, nfl_id_intersect, is_lo, sec) %>%
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
      # Keep the pick play to one row per play at maximum
      nest(pick_data = -c(game_id, play_id)),
    by = c('game_id', 'play_id')
  ) %>%
  left_join(
    personnel_and_rushers %>%
      select(-rushers) %>%
      rename(n_qb_rusher = n_qb),
    by = c('game_id', 'play_id')
  ) %>%
  mutate(
    is_pick_play = map_lgl(pick_data, ~!is.null(.x)),
    across(n_rusher, ~coalesce(.x, 0L)),
    # pick_data = map_if(pick_data, is.null, ~tibble())
  ) %>%
  relocate(week, game_id, play_id, is_pick_play, pick_data)
plays_w_pick_info
plays_w_pick_info %>% filter(!is_pick_play)

pick_play_meta <-
  pick_play_meta_init %>%
  # Now get other numbers from `plays`.
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
pick_play_meta

# TODO: Need a data set describing how defenders played it

pick_play_meta_viz <-
  pick_play_meta %>%
  # filter(pass_result == 'C') %>%
  mutate(pass_complete = if_else(pass_result == 'C', TRUE, FALSE)) %>%
  filter(!is.na(target_is_intersect)) %>%
  group_by(sec, pass_complete, is_lo, target_is_intersect) %>%
  mutate(prnk = percent_rank(epa)) %>%
  filter(prnk == min(prnk) | prnk == max(prnk)) %>%
  ungroup() %>%
  # filter(prnk == 0 | prnk == 1) %>%
  mutate(high_epa = if_else(prnk == 1, TRUE, FALSE)) %>%
  filter(high_epa == pass_complete) %>%
  arrange(sec, pass_complete, is_lo, target_is_intersect) %>%
  filter(is_lo) %>%
  inner_join(plays %>% select(game_id, play_id, nfl_id_target = target_nfl_id, play_result)) %>%
  inner_join(players_from_tracking) %>%
  inner_join(players_from_tracking %>% rename_with(~sprintf('%s_intersect', .x), c(nfl_id, display_name, jersey_number, position))) %>%
  left_join(players_from_tracking %>% rename_with(~sprintf('%s_target', .x), c(nfl_id, display_name, jersey_number, position))) %>%
  mutate(
    across(c(nfl_id_target, jersey_number), ~coalesce(.x, -1L)),
    across(c(display_name, position), ~coalesce(.x, '?'))
  ) %>%
  mutate(
    lab = glue::glue('Pick between {display_name} ({jersey_number}, {position}) and {display_name_intersect} ({jersey_number_intersect}, {position_intersect}) between {sec-0.5} and {sec} seconds.
                     Pick to underneath route: {is_lo}, Target: {display_name_target} ({jersey_number_target}, {position_target}). Play result: {pass_result}. Yards gained: {play_result}.
                     BDB EPA: {scales::number(epa, accuracy = 0.01)}, nflfastR EPA: {scales::number(epa_nflfastr, accuracy = 0.01)}, nflfastR WPA: {scales::number(wpa_nflfastr, accuracy = 0.01)}'),
    path = file.path('inst', sprintf('is_pick_play=%s-sec=%1.1f-pass_complete=%s-is_lo=%s-target_is_intersect=%s-high_epa=%s-%s-%s.png', 'Y', sec, ifelse(pass_result == 'Good', 'Y', 'N'), ifelse(is_lo, 'Y', 'N'), ifelse(target_is_intersect, 'Y', 'N'), ifelse(high_epa, 'Y', 'N'), game_id, play_id))
  )
pick_play_meta_viz %>% arrange(desc(epa))

res_viz <-
  pick_play_meta_viz %>%
  # slice(c(21:28)) %>%
  filter(sec >= 1, sec <= 2) %>%
  mutate(
    viz = pmap(
      list(game_id, play_id, lab),
      ~plot_play(game_id = ..1, play_id = ..2, save = FALSE) +
        labs(subtitle = ..3),
    ),
    res = map2(viz, path, ~ggsave(filename = ..2, plot = ..1, unit = 'in', height = 10, width = 10))
  )

pick_play_agg <-
  pick_play_meta %>%
  # Drop the plays where the targeted receiver is NA.
  # drop_na() %>%
  # filter(!is.na(target_is_intersect)) %>%
  mutate(across(pass_result, ~if_else(.x != 'C', 'I', .x))) %>%
  group_by(target_is_intersect, sec, pass_result) %>%
  summarize(
    n = n(),
    across(c(ends_with('_nflfastr'), epa), mean, na.rm = TRUE)
  ) %>%
  ungroup() # %>%
# group_by(target_is_intersect, sec_start, sec_end) %>%
# mutate(frac = n / sum(n)) %>%
# ungroup()
pick_play_agg


# ----
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

features_lag_init <-
  features_min %>%
  group_by(game_id, play_id, nfl_id) %>%
  mutate(
    has_same_prev_defender = if_else(nfl_id_d == dplyr::lag(nfl_id_d), TRUE, FALSE),
    has_same_init_defender = if_else(nfl_id_d == dplyr::first(nfl_id_d), TRUE, FALSE)
  ) %>%
  ungroup() %>%
  mutate(across(has_same_init_defender, ~if_else(sec == 0, NA, .x))) %>%
  left_join(
    receiver_intersections_relaxed_adj %>%
      select(week, game_id, play_id, nfl_id, sec, is_lo) %>%
      mutate(has_intersect = TRUE)
  ) %>%
  mutate(
    across(has_intersect, ~coalesce(.x, FALSE)),
    across(has_intersect, ~if_else(sec == 0, NA, .x))
  )
features_lag_init

features_lag_n_def <-
  features_lag_init %>%
  group_by(week, game_id, play_id, nfl_id) %>%
  summarize(n_def = n_distinct(nfl_id_d)) %>%
  ungroup()
features_lag_n_def %>% count(n_def)
features_lag_n_def %>% count(n_def)

features_lag <-
  features_lag_init %>%
  left_join(
    features_lag_init %>%
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
features_lag_long

# features_lag_long %>% filter(sec > 0) %>% filter(intersect_cnd == 'past')
features_lag_long %>%
  filter(sec > 0) %>%
  filter(intersect_cnd == 'current' & defender_cnd == 'prev') %>%
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
  ggplot() +
  aes(x = sec, y = epa) +
  geom_point()



features_lag_n <-
  features_lag_long %>%
  filter(sec > 0) %>%
  count(sec, is_lo, defender_cnd, defender_lgl, intersect_cnd, intersect_lgl) %>%
  group_by(sec, is_lo, defender_cnd, intersect_cnd) %>%
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
  filter(is_lo != FALSE) %>%
  filter(intersect_cnd == 'past', defender_cnd == 'init') %>%
  mutate(grp = sprintf('Is pick? %s. Is initial defender? %s.', ifelse(intersect_lgl, 'Y', 'N'), ifelse(defender_lgl, 'Y', 'N'))) %>%
  # group_by(sec) %>%
  # summarize(across(frac, sum))
  ggplot() +
  aes(x = sec, y = n) +
  geom_col(aes(fill = grp), show.legend = FALSE) +
  facet_wrap(~grp, scales = 'fixed')


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
