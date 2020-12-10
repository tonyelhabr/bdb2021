
library(tidyverse)
library(bdb2021)
# data('routes', package = 'bdb2021')
new_features <- import_new_features()
plays <- import_plays()

events_end_rush <- .get_events_end_rush()
features_min_init <-
  new_features %>%
  filter(event %in% c('0.0 sec', events_end_rush)) %>%
  select(week, game_id, play_id, event, frame_id, sec, nfl_id, nfl_id_d_robust) %>%
  # If there are multiple of these `events_end_rush` on the same play, then just pick the first
  group_by(game_id, play_id, nfl_id, event) %>%
  # filter(frame_id == min(frame_id)) %>%
  filter(row_number() == 1L) %>%
  ungroup()
features_min_init

features_min_end_drop <-
  features_min_init %>% 
  filter(event %in% events_end_rush) %>% 
  group_by(game_id, play_id, nfl_id) %>% 
  # slice_min(frame_id, with_ties = FALSE) %>% 
  filter(row_number(frame_id) > 1L) %>% 
  ungroup()
features_min_end_drop

features <-
  features_min_init %>% 
  anti_join(features_min_end_drop)
features

features_events <-
  features %>%
  mutate(across(event, ~if_else(.x == '0.0 sec', 'ball_snap', .x))) %>%
  filter(event %>% str_detect('sec$', negate = TRUE)) %>%
  distinct(game_id, play_id, event, frame_id) %>%
  group_by(game_id, play_id, event) %>%
  filter(frame_id == min(frame_id)) %>%
  ungroup()
features_events

snap_frames <-
  features %>%
  group_by(game_id, play_id) %>%
  filter(frame_id == min(frame_id)) %>%
  ungroup() %>%
  select(game_id, play_id, frame_id)
snap_frames

# snap_frames %>% 
#   distinct(game_id, play_id) %>% 
#   anti_join(plays %>% select(game_id, play_id, play_description))

features_wide <-
  new_features %>%
  semi_join(snap_frames) %>% 
  # count(game_id, play_id, frame_id, nfl_id) %>%
  # distinct(game_id, play_id, frame_id, nfl_id, .keep_all = TRUE) %>%
  group_by(game_id, play_id, frame_id, nfl_id) %>%
  mutate(rn = row_number()) %>%
  filter(rn == min(rn)) %>%
  ungroup() %>% 
  select(-rn)
features_wide

cols_id <-
  c(
    'week',
    'game_id',
    'play_id',
    'frame_id'
  )
cols_id_model <-
  c(
    'idx'
  )
cols_static <-
  c(
    cols_id,
    cols_id_model
  )
cols_pivot_name <- 'idx_o'
cols_pivot_value <-
  c(
    'x', 'y', 'dist_ball'
  )
cols_keep <-
  c(
    cols_static,
    cols_pivot_name,
    cols_pivot_value
  )
cols_keep

features_wide <-
  features_wide %>%
  # Only a few plays like this
  filter(!is.na(idx_o)) %>% 
  # left_join(
  #   routes,
  #   by = c('game_id', 'play_id', 'nfl_id')# 
  # ) %>%
  # mutate(
  #   across(c(cols_pivot_value), ~case_when(is.na(route) ~ 9999, TRUE ~ .x))
  # ) %>% 
  arrange(game_id, play_id, frame_id, idx_o) %>% 
  filter(idx_o <= 5L) %>% 
  select(any_of(cols_keep)) %>%
  pivot_wider(
    names_from = all_of(cols_pivot_name),
    # values_from = setdiff(nms, cols_keep)
    values_from = all_of(cols_pivot_value)
    # values_from = cols_features
  ) %>%
  mutate(idx = row_number()) %>%
  mutate(
    across(where(is.integer), ~case_when(is.na(.x) ~ -1L, TRUE ~ .x)),
    across(where(is.double), ~case_when(is.na(.x) ~ 9999, TRUE ~ .x))
  ) %>% 
  relocate(idx)
features_wide

features_w_pick_ind <-
  features_wide %>% 
  left_join(
    pick_plays %>% 
      distinct(game_id, play_id) %>% 
      mutate(has_intersect = TRUE)
  ) %>% 
  mutate(across(has_intersect, ~coalesce(.x, FALSE))) %>% # %>% as.integer() %>% factor())) %>% 
  inner_join(
    plays %>% 
      select(game_id, play_id, epa)
  )
features_w_pick_ind

fmla <-
  generate_formula(
    data = features_w_pick_ind,
    y = 'has_intersect',
    x_exclude = c('week', 'game_id', 'play_id', 'frame_id', 'idx', 'epa') # , sprintf('y_%d', c(1:5)))
  )
fmla
features_w_pick_ind %>% filter(is.na(epa))

fit_ind <-
  glm(fmla, data = features_w_pick_ind, family = stats::binomial)
fit_ind

matchit_res <-
  MatchIt::matchit(
    X = fit_ind %>% fitted(),
    # X = model.matrix(fmla, features_w_pick_ind),
    Y = features_w_pick_ind$epa,
    Tr = features_w_pick_ind$has_intersect
  )

match_res <-
  Matching::Match(
    # X = fit_ind %>% fitted(),
    fmla
    data = features_w_pick_ind,
  )
features_w_pick_ind[match_res$index.treated, ] %>% count(has_intersect) # count(idx, sort = TRUE)
features_w_pick_ind[match_res$index.control, ] %>% count(has_intersect)

features_w_pick_ind[match_res$index.control, ] %>% 
  head(1) %>% 
  relocate(has_intersect)
  mutate(viz = map2(game_id, play_id, plot_play))

coefs_ind <-
  fit_ind %>%
  broom::tidy() %>%
  mutate(
    # across(term, ~str_replace_all(.x, '(^.*)([<>2-8]+)', '\\1_\\2')),
    across(term, ~fct_reorder(.x, estimate)),
    lo = estimate - 1.96 * std.error,
    hi = estimate + 1.96 * std.error,
    is_signif = if_else(p.value < 0.05, TRUE, FALSE)
  )
coefs_ind

coefs_ind %>%
  ggplot() +
  aes(y = term, x = estimate, color = is_signif) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), size = 1) +
  scale_color_manual(values = c(`TRUE` = 'red', `FALSE` = 'black')) +
  geom_vline(data = tibble(), aes(xintercept = 0), linetype = 2) +
  guides(color = guide_legend('Is Statistically Significant?')) +
  theme(
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = 'top'
  )
