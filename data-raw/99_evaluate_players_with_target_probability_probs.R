
library(tidyverse)
# jersey_numbers
data('players_from_tracking', package = 'bdb2021')

features <-
  file.path('inst', 'features.parquet') %>%
  arrow::read_parquet()
features

min_dists <-
  file.path('inst', 'min_dists_naive_target.parquet') %>%
  arrow::read_parquet() %>%
  select(game_id, play_id, frame_id, event, idx_o, nfl_id, nfl_id_d, dist_d) %>%
  group_by(game_id, play_id, frame_id, event, idx_o, nfl_id) %>%
  # Inverse distance weighting. Using the squared power is sort of an arbitrary choice. One could use whatever power seems reasonable.
  mutate(
    dist_d_total_o = sum(1 / dist_d^2),
    wt_o = (1 / dist_d^2) / dist_d_total_o
  ) %>%
  ungroup() %>%
  group_by(game_id, play_id, frame_id, event, nfl_id_d) %>%
  # Inverse distance weighting. Using the squared power is sort of an arbitrary choice. One could use whatever power seems reasonable.
  mutate(
    dist_d_total_d = sum(1 / dist_d^2),
    wt_d = (1 / dist_d^2) / dist_d_total_d
  ) %>%
  ungroup()
min_dists
# min_dists %>% filter(game_id == 2018090600, play_id == 75, frame_id1, nfl_id_d == 79848) %>% mutate(across(wt_d, list(total = sum)))
# min_dists %>% filter(game_id == 2018090600, play_id == 75, frame_id1, nfl_id == 2495454) %>% mutate(across(wt_o, list(total = sum)))
#
min_dists %>% arrange(wt)
# min_dists %>% ggplot() + aes(x = wt) + geom_histogram(binwidth = 0.01)
# Will get some warnings on creating the seconds column.
# The `+ 0.25` is to identify when passes occur between the half second frames.
# (For example, a pass that occurs between frames 26 and 30 (`sec=2.5` and `sec=3.0`) after the snap will get a `sec=2.75`.
probs <-
  file.path('inst', 'probs-tp-final-folds.parquet') %>%
  arrow::read_parquet() %>%
  filter(.set == 'tst') %>%
  select(-c(.set, .pred_class, idx, week)) %>%
  mutate(
    sec = case_when(
      event %>% str_detect('sec$') ~ event %>% str_remove(' sec') %>% as.double(),
      TRUE ~ event_lag1 %>% str_remove(' sec') %>% as.double() %>% {. + 0.25}
    )
  )
probs

# Prep for data filtering.
snap_frames <-
  probs %>%
  group_by(game_id, play_id) %>%
  filter(frame_id == min(frame_id)) %>%
  ungroup() %>%
  select(game_id, play_id, frame_id_min = frame_id)

# More prep.
minmax_frames <-
  probs %>%
  left_join(
    snap_frames
  ) %>%
  mutate(frame_id_diff = frame_id - frame_id_min) %>%
  filter(frame_id_diff < 35) %>%
  group_by(game_id, play_id) %>%
  summarize(
    across(c(frame_id), list(min = min, max = max))
  ) %>%
  ungroup() %>%
  mutate(frame_id_diff = frame_id_max - frame_id_min) %>%
  inner_join(probs %>% distinct(game_id, play_id, frame_id_min = frame_id, event_min = event)) %>%
  inner_join(probs %>% distinct(game_id, play_id, frame_id_max = frame_id, event_max = event))

# If the pass is after 3 seconds after the snap, then take the 3 second frame.
minmax_frames_long <-
  minmax_frames %>%
  pivot_longer(
    matches('frame_id_(min|max)$'),
    names_to = 'frame_id_stat',
    values_to = 'frame_id'
  ) %>%
  pivot_longer(
    matches('event_(min|max)$'),
    names_to = 'event_stat',
    values_to = 'event'
  ) %>%
  mutate(
    across(matches('_stat$'), ~str_replace(.x, '(^.*_)(min|max)', '\\2'))
  ) %>%
  filter(frame_id_stat == event_stat)

probs_long <-
  probs %>%
  # Old way
  # filter((event == '0.0 sec' & event_lag1 == 'none') | (event %in% c('pass_forward', 'pass_shovel') & event_lag1 %>% str_detect('sec$'))) %>%
  # group(game_id, play_id) %>%
  # filter(n() == 2L) %>%
  # ungroup()
  # New way
  # inner_join(
  #   minmax_frames_long %>% select(game_id, play_id, frame_id)
  # ) %>%
  mutate(across(c(idx_o_target), as.integer)) %>%
  pivot_longer(
    matches('[.]pred_[1-9]'),
    names_to = 'name',
    values_to = 'prob'
  ) %>%
  separate(name, into = c('dummy', 'idx_o_target_pred'), sep = '_') %>%
  select(-dummy) %>%
  mutate(across(idx_o_target_pred, as.integer)) %>%
  arrange(game_id, play_id, frame_id)
probs_long

.select_rename <- function(data, suffix = '') {
  features %>%
    select(game_id, play_id, frame_id, event, nfl_id, idx_o) %>%
    rename_with(~sprintf('%s_target%s', .x, suffix), c(nfl_id, idx_o))
}

diffs <-
  probs_long %>%
  # Probably should adapt model to be handle these cases (treat them like the `NA` case), as well as with plays that have less than 5 route runners.
  filter(idx_o_target != 6) %>%
  left_join(
    features %>% .select_rename('_pred')
  ) %>%
  left_join(
    features %>% .select_rename('')
  )
diffs

diffs_ex <-
  diffs %>%
  filter(game_id == first(game_id), play_id <= 300) %>%
  select(-idx_o_target, -idx_o_target_pred)
write_csv(diffs_ex, 'inst/example_target_probs.csv')

# diffs_n <-
#   diffs %>%
#   count(game_id, play_id, frame_id, idx_o_target_pred, nfl_id_d_robust)
# diffs_n %>% count(n)

diffs_ex <-
  diffs %>%
  filter(game_id == first(game_id), play_id <= 300) %>%
  # arrange(game_id, play_id, frame_id) %>%
  # filter(game_id == first(game_id), play_id == first(play_id)) %>%  # , idx_o_target_pred == 5) %>%
  # select(game_id, play_id, frame_id, event, idx_o_target, idx_o_target_pred, nfl_id_target, nfl_id_target_pred) %>%
  # head(5) %>%
  # left_join(min_dists %>% filter(nfl_id_d == 79848)) %>%
  left_join(min_dists %>% rename(nfl_id_target_pred = nfl_id)) %>%
  # arrange(game_id, play_id, frame_id, idx_o_target_pred, nfl_id_d) %>%
  mutate(
    is_target = if_else(idx_o_target_pred == idx_o_target, TRUE, FALSE)
  ) %>%
  select(
    game_id,
    play_id,
    frame_id,
    sec,
    event,
    event_lag1,
    # idx_o,
    # idx_o_target,
    # idx_o_target_pred, # Same as idx_o
    nfl_id_o = nfl_id_target_pred,
    nfl_id_target,
    is_target,
    nfl_id_d,
    prob,
    dist_d,
    wt_d
  ) %>%
  left_join(
    players_from_tracking %>%
      select(-week) %>%
      rename_with(~sprintf('%s_o', .x), c(nfl_id, position, display_name, jersey_number))
  ) %>%
  left_join(
    positions %>%
      select(position, position_label) %>%
      rename_with(~sprintf('%s_o', .x), c(position, position_label))
  ) %>%
  left_join(
    players_from_tracking %>%
      select(-week) %>%
      rename_with(~sprintf('%s_d', .x), c(nfl_id, position, display_name, jersey_number))
  ) %>%
  left_join(
    positions %>%
      select(position, position_label) %>%
      rename_with(~sprintf('%s_d', .x), c(position, position_label))
  )
diffs_ex
write_csv(diffs_ex, 'inst/example_target_probs.csv')

diffs_wide <-
  diffs %>%
  filter(game_id == first(game_id), play_id <= 300) %>%
  # arrange(game_id, play_id, frame_id) %>%
  # filter(game_id == first(game_id), play_id == first(play_id)) %>%  # , idx_o_target_pred == 5) %>%
  # select(game_id, play_id, frame_id, event, idx_o_target, idx_o_target_pred, nfl_id_target, nfl_id_target_pred) %>%
  # head(5) %>%
  # left_join(min_dists %>% filter(nfl_id_d == 79848)) %>%
  left_join(min_dists %>% rename(nfl_id_target_pred = nfl_id)) %>%
  # arrange(game_id, play_id, frame_id, idx_o_target_pred, nfl_id_d) %>%
  mutate(
    is_target = if_else(idx_o_target_pred == idx_o_target, TRUE, FALSE)
  ) %>%
  select(
    game_id,
    play_id,
    frame_id,
    # idx_o,
    idx_o_target,
    idx_o_target_pred, # Same as idx_o
    nfl_id_target_pred,
    nfl_id_d,
    event,
    prob,
    wt_o,
    wt_d
  ) %>%
  mutate(
    across(
      event,
      ~case_when(
        .x == '0.0 sec' ~ 'start',
        TRUE ~ 'end'
      )
    )
  ) %>%
  pivot_wider(
    names_from = event,
    values_from = c(prob, wt_o, wt_d)
  ) %>%
  drop_na() %>%
  mutate(
    is_target = if_else(idx_o_target_pred == idx_o_target, TRUE, FALSE),
    prob_diff = prob_end - prob_start,
    prob_diff_wt_o = (wt_o_end * prob_end) - (wt_o_start * prob_start),
    prob_diff_wt_d = (wt_d_end * prob_end) - (wt_d_start * prob_start)
  )
diffs_wide

diffs_wide %>%
  filter(game_id == first(game_id), play_id <= 300) %>%
  select(-idx_o_target, -idx_o_target_pred)
write_csv(diffs_ex, 'inst/example_target_probs.csv')


diffs_wide %>%
  filter(game_id == first(game_id), play_id <= 300)

p1 <- 0.99
w1 <- 0.94
p2 <- 0.01
w2 <- 0.99
+ p2 * w2 - p1 * w1
- p2 * w2 - p1 * w1
- p2 * w2 + p1 * w1
- p2 * w2 + p1 * w1

positions <- import_positions()
diffs_by_play <-
  diffs_wide %>%
  # filter(idx_o_target != idx_o_target_pred) %>%
  left_join(
    players_from_tracking %>%
      rename_with(~sprintf('%s_d', .x), c(nfl_id, position, display_name, jersey_number))
  ) %>%
  left_join(
    positions %>%
      select(position, position_label) %>%
      rename_with(~sprintf('%s_d', .x), c(position, position_label))
  ) %>%
  # arrange(prob_diff) %>%
  # select(week, game_id, play_id, is_target, nfl_id_d, position_label_d, display_name_d, jersey_number_d, matches('^prob'), matches('^wt')) %>%
  group_by(week, game_id, play_id, is_target, nfl_id_d, position_label_d, display_name_d, jersey_number_d) %>%
  summarize(
    across(matches('^(prob|wt)_'), sum)
  ) %>%
  ungroup()

diffs_by_play %>%
  filter(is_target) %>%
  arrange(prob_diff_wt_d)
0.086 * 0.873 - 0.788 * 0.95
diffs_by_play %>%
  filter(is_target) %>%
  arrange(prob_diff_wt_d) %>%
  filter(wt_d_start > 0.1 & wt_d_end > 0.1)

diffs_by_play %>%
  filter(is_target) %>%
  arrange(prob_diff_wt_d) %>%
  filter(wt_d_start > 0.1 & wt_d_end > 0.1) %>%
  slice(c(1:2)) %
>% mutate(diffs = map2(game_id, play_id, ~plot_play(game_id = ..1, play_id = ..2)))
diffs_by_play %>% filter(is_target) %>% arrange(prob_diff_wt_d) %>% slice(c(1:10)) %>% mutate(diffs = map2(game_id, play_id, ~plot_play(game_id = ..1, play_id = ..2)))


diffs_by_player <-
  diffs_by_play %>%
  group_by(is_target, nfl_id_d, position_label_d, display_name_d, jersey_number_d) %>%
  summarize(
    n = n(),
    across(matches('^prob_diff_wt'), list(mean = mean, sum = sum))
  ) %>%
  ungroup() %>%
  group_by(is_target) %>%
  mutate(
    rnk_o = row_number(prob_diff_wt_o_sum),
    rnk_d = row_number(prob_diff_wt_d_sum)
  ) %>%
  ungroup() %>%
  group_by(is_target, position_label_d) %>%
  mutate(
    rnk_o_pos = row_number(prob_diff_wt_o_sum),
    rnk_d_pos = row_number(prob_diff_wt_d_sum)
  ) %>%
  ungroup()
diffs_by_player

diffs_by_player %>%
  filter(!is_target) %>%
  filter(position_label_d == 'CB') %>%
  select(display_name_d, jersey_number_d, rnk_d) %>%
  arrange(rnk_d)

diffs_by_player %>%
  filter(!is_target) %>%
  arrange(rnk_d) %>%
  # filter(position_label_d == 'CB') %>%
  select(position_label_d, display_name_d, rnk_d) %>%
  head(20) %>%
  count(position_label_d)

diffs_by_player %>%
  filter(!is_target) %>%
  ggplot() +
  aes(x = prob_diff_wt_d_sum) +
  geom_histogram(aes(fill = position_label_d), binwidth = 0.5)

diffs_by_player %>%
  filter(is_targetL) %>%
  ggplot() +
  aes(x = rnk_d, y = n) +
  geom_point()

diffs_by_player %>%
  filter(is_targetL) %>%
  ggplot() +
  aes(y = prob_diff_wt_d_sum, x = n) +
  geom_point()

diffs_by_player %>%
  filter(!is_target) %>%
  filter(position_label_d == 'CB') %>%
  arrange(rnk_o)

top_cbs_when_targeted <-
  diffs_by_player %>%
  filter(is_targeted) %>%
  filter(position_d_robust_end == 'CB') %>%
  filter(n > 50) %>%
  arrange(prob_diff_mean) # %>%
  # select(display_name_d_robust_end, n, prob_diff_mean, prob_diff_sum) %>%
  # mutate(across(matches('^prob_'), ~scales::percent(.x, accuracy = 0.01)))
top_cbs_when_targeted

top_cbs_when_not_targeted <-
  diffs_by_player %>%
  filter(!is_targeted) %>%
  filter(position_d_robust_end == 'CB') %>%
  filter(n > 250) %>%
  arrange(prob_diff_mean)

top_cbs_when_not_targeted %>% filter(display_name_d_robust_end == 'Stephon Gilmore')

diffs_by_player %>%
  left_join(players_from_tracking)

probs_wide_filt %>% arrange(diff)
probs_wide_filt %>% arrange(-diff)
probs_wide_filt %>% filter(is.na(end)) %>% inner_join(probs)

probs_wide_filt %>% summarize(across(c(start, end, diff), mean, na.rm = TRUE))
probs_wide_filt %>%
  ggplot() +
  aes(x = diff) +
  geom_histogram(binwidth = 0.01)
probs %>%
  filter(event == 'pass_forward') %>%
  count(event_lag1)
plays <- import_plays()
plays %>%
  filter(is.na(target_nfl_id)) %>%
  select(play_description)
pbp <- import_nflfastr_pbp()
pbp %>% count(play_type_nfl)


