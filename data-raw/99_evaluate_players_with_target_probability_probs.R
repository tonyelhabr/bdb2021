
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
    dist_d_total = sum(1 / dist_d^2),
    wt = (1 / dist_d^2) / dist_d_total
  ) %>%
  ungroup() %>%
  select(-dist_d_total)
min_dists

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
  # filter((event == '0.0 sec' & event_lag1 == 'none') | (event %in% c('pass_forward', 'pass_shovel') & event_lag1 %>% str_detect('sec$'))) %>%
  inner_join(
    minmax_frames_long %>% select(game_id, play_id, frame_id)
  ) %>%
  # group(game_id, play_id) %>%
  # filter(n() == 2L) %>%
  # ungroup()
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

res <-
  probs_long %>%
  # Probably should adapt model to be handle these cases (treat them like the `NA` case), as well as with plays that have less than 5 route runners.
  filter(idx_o_target != 6) %>%
  left_join(
    features %>%
      select(game_id, play_id, frame_id, event, nfl_id_target_pred = nfl_id, idx_o_target_pred = idx_o)
  ) %>%
  left_join(
    features %>%
      select(game_id, play_id, frame_id, event, nfl_id_target = nfl_id, idx_o_target = idx_o)
  )
res

# res_n <-
#   res %>%
#   count(game_id, play_id, frame_id, idx_o_target_pred, nfl_id_d_robust)
# res_n %>% count(n)

res_wide <-
  res %>%
  arrange(game_id, play_id, frame_id) %>%
  filter(game_id == first(game_id), play_id == first(play_id)) %>%  # , idx_o_target_pred == 5) %>%
  # select(game_id, play_id, frame_id, event, idx_o_target, idx_o_target_pred, nfl_id_target, nfl_id_target_pred) %>%
  # head(5) %>%
  # left_join(min_dists %>% filter(nfl_id_d == 79848)) %>%
  left_join(min_dists %>% rename(nfl_id_target_pred = nfl_id)) %>%
  # arrange(game_id, play_id, frame_id, idx_o_target_pred, nfl_id_d) %>%
  select(game_id, play_id, idx_o, idx_o_target, idx_o_target_pred, nfl_id_target_pred, nfl_id_d, event, prob, wt) %>%
  mutate(
    across(
      event,
      ~case_when(
        .x == '0.0 sec' ~ 'start',
        TRUE ~ 'end'
      )
    )
  ) %>%
  # left_join(min_dists %>% select(-event)) %>%
  pivot_wider(
    names_from = event,
    values_from = c(prob, wt)
  ) %>%
  drop_na() %>%
  mutate(
    is_target = if_else(idx_o == idx_o_target, 1L, 0L),
    prob_diff = prob_end - prob_start,
    prob_diff_wt = (wt_end * prob_end) - (wt_start * prob_start)
  )
res_wide

res_wide %>%
  group_by(nfl_id_d, is_target) %>%
  summarize(
    across(matches('prob'), mean)
  )

top_plays <-
  res_wide %>%
  # filter(idx_o_target != idx_o_target_pred) %>%
  left_join(
    players_from_tracking %>%
      rename_with(~sprintf('%s_d_robust_end', .x), c(nfl_id, position, display_name))
  ) %>%
  arrange(prob_diff) %>%
  select(week, game_id, play_id, position_d_robust_end, display_name_d_robust_end, prob_start, prob_end, prob_diff)
top_plays
plot_play(game_id = 2018090901, play_id = 1199)
plot_play(game_id = 2018102109, play_id = 2366)

res_agg <-
  res_wide %>%
  # filter(idx_o_target == idx_o_target_pred) %>%
  left_join(players_from_tracking %>% rename_with(~sprintf('%s_d_robust_end', .x), c(nfl_id, position, display_name))) %>%
  mutate(is_targeted = idx_o_target == idx_o_target_pred) %>%
  group_by_at(vars(matches('d_robust_end'), is_targeted)) %>%
  summarize(
    n = n(),
    across(prob_diff, list(mean = mean, sum = sum))
  ) %>%
  ungroup() %>%
  group_by(is_targeted) %>%
  mutate(
    rnk = row_number(prob_diff_mean)
  ) %>%
  ungroup() %>%
  group_by(is_targeted, position_d_robust_end) %>%
  mutate(
    rnk = row_number(prob_diff_mean)
  ) %>%
  ungroup() %>%
  arrange(prob_diff_sum)

top_cbs_when_targeted <-
  res_agg %>%
  filter(is_targeted) %>%
  filter(position_d_robust_end == 'CB') %>%
  filter(n > 50) %>%
  arrange(prob_diff_mean) # %>%
  # select(display_name_d_robust_end, n, prob_diff_mean, prob_diff_sum) %>%
  # mutate(across(matches('^prob_'), ~scales::percent(.x, accuracy = 0.01)))
top_cbs_when_targeted

top_cbs_when_not_targeted <-
  res_agg %>%
  filter(!is_targeted) %>%
  filter(position_d_robust_end == 'CB') %>%
  filter(n > 250) %>%
  arrange(prob_diff_mean)

top_cbs_when_not_targeted %>% filter(display_name_d_robust_end == 'Stephon Gilmore')

res_agg %>%
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


