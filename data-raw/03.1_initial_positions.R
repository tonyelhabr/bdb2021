
library(tidyverse)
library(bdb2021)
# data('routes', package = 'bdb2021')
features <- import_new_features()

snap_frames <-
  features %>%
  group_by(game_id, play_id) %>%
  filter(frame_id == min(frame_id)) %>%
  ungroup() %>%
  select(game_id, play_id, frame_id)
snap_frames

features_filt <-
  features %>%
  semi_join(snap_frames) %>% 
  # count(game_id, play_id, frame_id, nfl_id) %>%
  # distinct(game_id, play_id, frame_id, nfl_id, .keep_all = TRUE) %>%
  group_by(game_id, play_id, frame_id, nfl_id) %>%
  mutate(rn = row_number()) %>%
  filter(rn == min(rn)) %>%
  ungroup() %>% 
  select(-rn)
features_filt

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
    cols_id_model,
    'nfl_id_target'
  )
cols_pivot_name <- 'idx_o'
cols_pivot_value <-
  c(
    'x', 'dist_ball', 'dist_d1_naive'
  )
cols_keep <-
  c(
    cols_static,
    cols_pivot_name,
    cols_pivot_value
  )
cols_keep

# The big difference here compared to the target prob stuff is that we don't fill in for NA routes.
features_wide <-
  features_filt %>%
  # Only a few plays like this
  filter(!is.na(idx_o)) %>% 
  arrange(game_id, play_id, frame_id, idx_o) %>% 
  filter(idx_o <= 5L) %>% 
  select(any_of(cols_keep)) %>%
  pivot_wider(
    names_from = all_of(cols_pivot_name),
    values_from = all_of(cols_pivot_value)
  ) %>%
  mutate(idx = row_number()) %>%
  mutate(
    across(where(is.integer), ~case_when(is.na(.x) ~ -1L, TRUE ~ .x)),
    across(where(is.double), ~case_when(is.na(.x) ~ 9999, TRUE ~ .x))
  ) %>% 
  relocate(idx) %>% 
  left_join(
    plays_w_pick_info
  ) %>%
  filter(!is.na(is_pick_play))
features_wide$targe

features_wide %>% 
  filter(is_pick_play == '1') %>% 
  rename(nfl_id_target = target_nfl_id) %>% 
  mutate(
    is_target = if_else(nfl_id == nfl_id_target, TRUE, FALSE)
  )

min_dists_naive_target <- import_min_dists_naive_target()
min_dists <-
  import_min_dists_naive_target() %>%
  select(game_id, play_id, frame_id, event, idx_o, nfl_id, nfl_id_d, dist_d) %>%
  group_by(game_id, play_id, frame_id, event, idx_o, nfl_id) %>%
  # Inverse distance weighting. Using the squared power is sort of an arbitrary choice. One could use whatever power seems reasonable.
  mutate(
    dist_d_total_o = sum(1 / dist_o^2),
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

# others: 'type_dropback', 'is_defensive_pi', 'personnel_o', 'personnel_d',  'possession_team',
cols_d <-
  c(
    # 'defenders_in_the_box',
    # 'number_of_pass_rushers',
    sprintf('n_%s_fct', c('wr', 'te')), # , 'dl', 'lb', 'db')),
    'quarter',
    'down'
  )

# cols_c_i_added_nflfastr <- 'wp'
cols_c_i_added <- 
  c(
    # cols_c_i_added_nflfastr, 
    sprintf('x_%d', 1:5),
    sprintf('dist_ball_%d', 1:5),
    sprintf('dist_d1_naive_%d', 1:5),
    'pre_snap_score_diff'
  )

# don't need  'pre_snap_visitor_score' if have home and differential
cols_c_i <-
  c(
    cols_c_i_added,
    # sprintf('n_%s', c('rb', 'wr', 'te')), # , 'dl', 'lb', 'db')),
    'yards_to_go',
    'absolute_yardline_number',
    'pre_snap_home_score'
  )
cols_features <- c(cols_d, cols_c_i)

col_y <- 'is_pick_play'
fmla_pick_play_prob <-
  generate_formula(
    # intercept = TRUE,
    intercept = FALSE,
    data = features_wide, 
    y = col_y, 
    x_include = cols_features
  )
fmla_pick_play_prob

fit_pick_play_prob_ind <-
  features_wide %>% 
  glm(fmla_pick_play_prob, data = ., family = stats::binomial)
fit_pick_play_prob_ind

res_match <-
  Matching::Match(
    # caliper = 0.25,
    ties = FALSE,
    X = fit_pick_play_prob_ind %>% fitted(),
    # X = model.matrix(fmla, features_wide),
    Y = features_wide[['epa']],
    Tr = features_wide[['is_pick_play']] %>% as.integer() %>% {. - 1L}
  )
res_match

control_match <- features_wide[res_match[['index.control']], ]
treatment_match <- features_wide[res_match[['index.treated']], ]
features_match <- 
  bind_rows(
    control_match %>% mutate(grp = 'control'), 
    treatment_match %>% mutate(grp = 'treatment')
  )
features_match

.str_replace_f <- function(x, i) {
  x %>% str_replace('(^.*)_(mean|sd)$', sprintf('\\%d', i))
}

.aggregate_to_sd_diffs <- function(data) {
  agg <-
    data %>% 
    # Grab the original `n_wr` instead of `n_wr_fct`.
    select(one_of(str_remove(cols_features, '_fct')), is_pick_play) %>% 
    mutate(
      across(where(is.double), ~if_else(.x == 9999, NA_real_, .x)),
      across(where(is.factor), as.integer),
      across(is_pick_play, ~.x - 1L)
    ) %>% 
    group_by(is_pick_play) %>% 
    summarise(
      across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    pivot_longer(
      -is_pick_play
    ) %>% 
    mutate(
      across(
        name,
        list(
          col = ~.str_replace_f(.x, 1),
          stat = ~.str_replace_f(.x, 2)
        ),
        .names = '{fn}'
      )
    ) %>% 
    select(-name)
  
  res <-
    agg %>% 
    pivot_wider(
      names_from = c(stat, is_pick_play),
      values_from = value
    ) %>% 
    mutate(sd_diff = (mean_1 - mean_0) / sd_1)
  res
}

.toupper1 <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

sd_diffs <-
  bind_rows(
    features_wide %>% .aggregate_to_sd_diffs() %>% mutate(grp = 'before'),
    features_match %>% .aggregate_to_sd_diffs() %>% mutate(grp = 'after')
  ) %>% 
  mutate(
    across(grp, ~.x %>% .toupper1() %>% fct_inorder())
  ) # ordered(.x, levels = sprintf('%s Matching', .x
sd_diffs

sd_diffs %>% 
  arrange(col) %>% 
  ggplot() +
  aes(y = col, x = abs(sd_diff), color = grp) +
  geom_point() +
  geom_path(aes(group = grp))

features_wide <-
  features_wide %>% 
  left_join(features_match %>% distinct(game_id, play_id, grp))
features_wide %>% 
  filter(!is.na(grp))

res_matchit <-
  MatchIt::matchit(
    fmla_pick_play_prob,
    method = 'nearest', 
    data = features_wide
  )
res_matchit
matched_matchit <- res_matchit %>% MatchIt::match.data() %>% arrange(subclass)
matched_matchit
matched %>% 
  lm(formula(epa ~ is_pick_play), data = ., weights = 1 / distance) %>% 
  summary()

res_match <-
  Matching::Match(
    # X = fit_ind %>% fitted(),
    fmla
    data = features_wide,
  )
features_wide[res_match$index.treated, ] %>% count(has_intersect) # count(idx, sort = TRUE)
features_wide[res_match$index.control, ] %>% count(has_intersect)

features_wide[res_match$index.control, ] %>% 
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
