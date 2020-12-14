
library(tidyverse)
library(bdb2021)
data('plays_w_pick_info', package = 'bdb2021')
features <- import_new_features()
min_dists_naive_od_target <- file.path(get_bdb_dir_data(), 'min_dists_naive_od_target.parquet') %>% arrow::read_parquet()
min_dists_naive_od_target

snap_frames <-
  min_dists_naive_od_target %>%
  group_by(game_id, play_id) %>%
  filter(frame_id == min(frame_id)) %>%
  ungroup() %>%
  select(game_id, play_id, frame_id)
snap_frames

min_dists_naive_od_target_filt <-
  min_dists_naive_od_target %>%
  semi_join(snap_frames)
min_dists_naive_od_target_filt

# others: 'type_dropback', 'is_defensive_pi', 'personnel_o', 'personnel_d',  'possession_team',
cols_extra <- c('epa', 'pass_complete')
cols_id <- c('game_id', 'play_id')
cols_d <-
  c(
    # 'defenders_in_the_box',
    # 'number_of_pass_rushers',
    # sprintf('n_%s_fct', c('wr', 'te')), # , 'dl', 'lb', 'db')),
    # 'quarter_fct',
    'game_half_fct',
    'down_fct'
  )

# cols_c_i_added_nflfastr <- 'wp'
cols_c_i_added <- 
  c(
    # cols_c_i_added_nflfastr, 
    # sprintf('x_%d', 1:5),
    # sprintf('dist_ball_%d', 1:5),
    # sprintf('dist_d1_naive_%d', 1:5),
    sprintf('x_%s', c('o', 'd')),
    sprintf('dist_%s', c('o', 'd')),
    'yardline_100',
    'pre_snap_score_diff'
  )

# don't need  'pre_snap_visitor_score' if have home and differential
cols_c_i <-
  c(
    cols_c_i_added,
    # 'quarter',
    # 'game_half',
    # 'down',
    # sprintf('n_%s', c('wr', 'te')), # , 'dl', 'lb', 'db')),
    'yards_to_go',
    'pre_snap_home_score'
  )
cols_features <- c(cols_d, cols_c_i)
col_trt <- 'target_is_intersect'
col_y <- 'epa_abs'

features_wide <-
  min_dists_naive_od_target_filt %>% 
  select(-target_nfl_id) %>% 
  inner_join(plays_w_pick_info) %>% 
  filter(pass_result %in% c('C', 'I', 'IN')) %>%  # no 'S'
  mutate(
    epa_abs = epa %>% abs(),
  ) %>%
  select(one_of(c(cols_id, col_y, col_trt, cols_features, cols_extra)))
features_wide

# Check to see that the epa numbers don't change much after the inner_join
.f_debug <- function(data) {
  data %>% 
    group_by(target_is_intersect, pass_complete) %>% 
    summarize(n = n(), across(epa, mean)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = pass_complete, values_from = c(epa, n))
}

plays_w_pick_info %>% .f_debug()
features_wide %>% .f_debug()

fmla_pick_play_prob_prop <-
  generate_formula(
    intercept = TRUE,
    # intercept = FALSE,
    data = features_wide, 
    y = col_trt, 
    x_include = cols_features
  )
fmla_pick_play_prob_prop
fmla_pick_play_prob_prop <- formula(target_is_intersect ~ game_half_fct + x_o + x_d + dist_o + dist_d + yardline_100 + pre_snap_score_diff*pre_snap_home_score + down_fct*yards_to_go + 1)
fit_pick_play_prob_prop <-
  features_wide %>% 
  glm(fmla_pick_play_prob_prop, data = ., family = stats::binomial)
fit_pick_play_prob_prop

# Quick check for significance.
fit_pick_play_prob_prop %>% 
  broom::tidy() %>% 
  arrange(p.value)

res_match <-
  Matching::Match(
    caliper = 0.25,
    ties = FALSE,
    X = fit_pick_play_prob_prop %>% fitted(),
    # X = model.matrix(fmla, features_wide),
    Y = features_wide[[col_y]],
    Tr = features_wide[[col_trt]] %>% as.integer() %>% {. - 1L}
  )
# # Lots of unnecesary stuff.
# res_match

control_match <- features_wide[res_match[['index.control']], ]
treatment_match <- features_wide[res_match[['index.treated']], ]
features_match <- 
  bind_rows(
    control_match %>% mutate(grp = 'control'), 
    treatment_match %>% mutate(grp = 'treatment')
  )
features_match
features_match %>% 
  .f_debug()

.str_replace_f <- function(x, i) {
  x %>% str_replace('(^.*)_(mean|sd)$', sprintf('\\%d', i))
}

.aggregate_to_sd_diffs <- function(data) {
  col_trt_sym <- col_trt %>% sym()
  agg <-
    data %>% 
    # Grab the original `n_wr` instead of `n_wr_fct`.
    select(one_of(c(cols_features, col_trt))) %>% 
    mutate(
      across(where(is.double), ~if_else(.x == 9999, NA_real_, .x)),
      across(where(is.factor), as.integer),
      across(!!col_trt_sym, ~.x - 1L)
    ) %>% 
    group_by(!!col_trt_sym) %>% 
    summarise(
      across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    pivot_longer(
      -one_of(col_trt)
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
      names_from = c('stat', col_trt),
      values_from = 'value'
    ) %>% 
    mutate(sd_diff = (mean_1 - mean_0) / sd_1)
  res
}

# .toupper1 <- function(x) {
#   x <- tolower(x)
#   substr(x, 1, 1) <- toupper(substr(x, 1, 1))
#   x
# }

sd_diffs <-
  bind_rows(
    features_wide %>% .aggregate_to_sd_diffs() %>% mutate(grp = 'Un-adjusted'),
    features_match %>% .aggregate_to_sd_diffs() %>% mutate(grp = 'Adjusted')
  ) %>% 
  mutate(
    across(grp, ~.x %>% fct_inorder())
  )
sd_diffs

sd_diffs_rnk <-
  sd_diffs %>% 
  filter(grp == 'Un-adjusted') %>% 
  # arrange(desc(abs_diff)) %>% 
  mutate(rnk = row_number(desc(abs(sd_diff)))) %>% 
  select(col, rnk) %>% 
  arrange(rnk)
sd_diffs_rnk

viz_love_pick_play_prob <-
  sd_diffs %>% 
  left_join(sd_diffs_rnk) %>% 
  mutate(across(col, ~fct_reorder(.x, -rnk))) %>% 
  arrange(grp, rnk) %>% 
  ggplot() +
  aes(y = col, x = abs(sd_diff), color = grp) +
  geom_vline(aes(xintercept = 0.1), linetype = 2) +
  geom_point() +
  geom_path(aes(group = grp)) +
  scale_color_manual(values = c(`Un-adjusted` = 'dodgerblue', `Adjusted` = 'darkorange')) +
  guides(
    color = guide_legend('', override.aes = list(size = 3))
  ) +
  theme(
    legend.position = 'top'
  ) +
  labs(
    title = 'Bias among coefficients for pick play model after matching',
    x = 'Absolute standardized mean difference',
    y = NULL
  )
viz_love_pick_play_prob
# do_save_plot(viz_love_pick_play_prob)

fmla_pick_play_prob <-
  generate_formula(
    intercept = TRUE,
    # intercept = FALSE,
    data = features_match,
    y = col_y, 
    x_include = c(cols_features, col_trt)
  )
fmla_pick_play_prob
fmla_pick_play_prob <- formula(epa_abs ~ target_is_intersect + game_half_fct + x_o + x_d + dist_o + dist_d + yardline_100 + pre_snap_score_diff*pre_snap_home_score + down_fct*yards_to_go + 1)
fit_pick_play_prob <-
  features_match %>% 
  lm(fmla_pick_play_prob, data = .)
fit_pick_play_prob %>% broom::tidy()

coefs_pick_play_prob <-
  fit_pick_play_prob %>%
  broom::tidy() %>%
  mutate(
    # across(term, ~str_replace_all(.x, '(^.*)([<>2-8]+)', '\\1_\\2')),
    across(term, ~fct_reorder(.x, estimate)),
    lo = estimate - 1.96 * std.error,
    hi = estimate + 1.96 * std.error,
    is_signif = if_else(p.value < 0.05, TRUE, FALSE)
  )
coefs_pick_play_prob

fit_pick_play_prob_simple <-
  features_match %>%
  select(epa_abs, target_is_intersect) %>% 
  mutate(
    x1 = if_else(target_is_intersect == '1', 1L, 0L),
    x0 = if_else(x1 == 1L, 0L, 1L)
  ) %>% 
  # lm(formula(epa_abs ~ x1 + x0 + 0), data = .)
  lm(formula(epa_abs ~ x1), data = .)
fit_pick_play_prob_simple %>% broom::tidy()

fit_pick_play_prob_simple <-
  features_match %>% 
  lm(formula(abs(epa) ~ target_is_intersect + 0), data = .)
fit_pick_play_prob_simple %>% broom::tidy()

fit_pick_play_prob_simple <-
  features_match %>% 
  lm(formula(epa ~ target_is_intersect + pass_complete + 0), data = .)
fit_pick_play_prob_simple %>% broom::tidy()

fit_pick_play_prob_prop %>%
  broom::augment() %>% 
  bind_cols(features_wide %>% select(game_id, play_id, epa, pass_result)) %>% 
  .f_debug()

fit_pick_play_prob %>%
  broom::augment() %>% 
  bind_cols(features_match %>% select(game_id, play_id, pass_result)) %>% 
  .f_debug()

fit_pick_play_prob_simple %>%
  broom::augment() %>% 
  bind_cols(features_match %>% select(game_id, play_id, pass_result)) %>% 
  .f_debug()

viz_pick_play_prob_coefs <-
  coefs_pick_play_prob %>%
  ggplot() +
  aes(y = term, x = estimate, color = is_signif) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), size = 1) +
  scale_color_manual(values = c(`TRUE` = 'red', `FALSE` = 'black')) +
  geom_vline(data = tibble(), aes(xintercept = 0), linetype = 2) +
  guides(color = guide_legend('Is statistically significant?')) +
  theme(
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = 'top' # ,
    # legend.box = element_rec()
  ) +
  labs(
    title = 'Adjusted targeted pick play probability model coefficients',
    y = NULL, x = 'Estimate +- 1.96 standard error'
  )
viz_pick_play_prob_coefs

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
  lm(formula(epa ~ target_is_intersect), data = ., weights = 1 / distance) %>% 
  summary()

res_match <-
  Matching::Match(
    # X = fit_prop %>% fitted(),
    fmla
    data = features_wide,
  )
features_wide[res_match$index.treated, ] %>% count(has_intersect) # count(idx, sort = TRUE)
features_wide[res_match$index.control, ] %>% count(has_intersect)

features_wide[res_match$index.control, ] %>% 
  head(1) %>% 
  relocate(has_intersect)
  mutate(viz = map2(game_id, play_id, plot_play))

coefs_prop <-
  fit_prop %>%
  broom::tidy() %>%
  mutate(
    # across(term, ~str_replace_all(.x, '(^.*)([<>2-8]+)', '\\1_\\2')),
    across(term, ~fct_reorder(.x, estimate)),
    lo = estimate - 1.96 * std.error,
    hi = estimate + 1.96 * std.error,
    is_signif = if_else(p.value < 0.05, TRUE, FALSE)
  )
coefs_prop

coefs_prop %>%
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
