
setwd('C:\\Users\\aelhabr\\Documents\\projects\\bdb2021')

# setup ----
# extrafont::loadfonts(device = 'win', quiet = TRUE)
library(tidyverse)
library(bdb2021)
theme_set_and_update_bdb()
# data('receiver_intersections_adj', package = 'bdb2021')
data('personnel_and_rushers', package = 'bdb2021')
data('players_from_tracking', package = 'bdb2021')
data('routes', package = 'bdb2021')
players_from_tracking <- players_from_tracking %>% select(-week)
routes <- routes %>% select(-week)

do_save_plot <- function(...) {
  if(TRUE) {
    save_plot(...)
  }
}

# Do this NA filtering now so earlier plots don't have extra plays.
plays <- import_plays() %>% filter(!is.na(absolute_yardline_number))
pbp <- import_nflfastr_pbp()
.sec_cutoff <- 2
new_features <- import_new_features()

if(FALSE) {
viz_intersections_after_n_sec <-
  receiver_intersections_adj %>% 
  semi_join(plays %>% select(game_id, play_id)) %>% 
  count(sec) %>% 
  mutate(
    across(sec, list(lab = ~sprintf('%1.1f < t <= %1.1f', sec - 0.5, sec))),
    across(sec_lab, ~forcats::fct_reorder(.x, sec))
  ) %>% 
  ggplot() +
  aes(x = sec_lab, y = n) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  theme(
    axis.text.x = element_text(size = 12),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = 'Receiver Intersections n seconds after the ball snap',
    x = 'Seconds Into the Play',
    y = '# of Plays'
  )
viz_intersections_after_n_sec
do_save_plot(viz_intersections_after_n_sec)
}

if(FALSE) {
pick_play_meta_init <-
  receiver_intersections_adj %>%
  filter(sec <= .sec_cutoff) %>%
  # Just get the `target_nfl_id` first.
  inner_join(
    plays %>%
      select(game_id, play_id, target_nfl_id),
    by = c('game_id', 'play_id')
  ) %>%
  group_by(week, game_id, play_id, nfl_id, nfl_id_intersect, is_lo, sec) %>%
  # There will be some NAs here due to missing `target_nfl_id`s.
  # I think it's best to leave these in.
  summarize(
    target_is_intersect = sum(target_nfl_id == nfl_id)
  ) %>%
  ungroup() %>%
  select(-week)
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
      rename_with(~sprintf('%s_personnel', .x), matches('^n_')) %>%
      rename(n_rusher = n_rusher_personnel),
    by = c('game_id', 'play_id')
  ) %>%
  mutate(
    is_pick_play = map_lgl(pick_data, ~!is.null(.x)),
    across(n_rusher, ~coalesce(.x, 0L)) # ,
    # # pick_data = map_if(pick_data, is.null, ~tibble())
  ) %>%
  relocate(game_id, play_id, is_pick_play, pick_data) %>%
  left_join(
    pbp %>%
      select(game_id, play_id, wp_nflfastr = wp, wpa_nflfastr = wpa, epa_nflfastr = epa),
    by = c('game_id', 'play_id')
  ) %>%
  select(-pick_data) %>%
  # mutate(across(is_pick_play, ~coalesce(.x, FALSE)))
  # Doing some mutations for the model fitting stuff.
  mutate(
    pick_play_type = if_else(is_pick_play, 'other_play', 'pick_play'),
    across(is_pick_play, ~if_else(.x, '1', '0') %>% factor()),
    pre_snap_score_diff = pre_snap_home_score - pre_snap_visitor_score
  ) %>%
  left_join(
    pbp %>%
      group_by(game_id) %>%
      arrange(game_seconds_remaining, .by_group = TRUE) %>%
      fill(wp) %>%
      ungroup() %>%
      select(game_id, play_id, wp)
  ) %>%
  mutate(
    across(c(quarter, down), factor)
  ) %>% 
  mutate(
    across(
      c(n_db, number_of_pass_rushers), 
      list(fct = ~case_when(
        .x < 4 ~ '<4',
        .x > 6 ~ '>6',
        TRUE ~ as.character(.x)
      ) %>% 
        fct_reorder(.x)
      )
    ),
    across(
      c(n_lb, n_dl, n_wr), 
      list(fct = ~case_when(
        .x < 2 ~ '<2',
        .x > 4 ~ '>4',
        TRUE ~ as.character(.x)
      ) %>% 
        fct_reorder(.x)
      )
    ),
    across(
      c(n_rb, n_te), 
      list(fct = ~case_when(
        .x < 1 ~ '<1',
        .x > 2 ~ '>2',
        TRUE ~ as.character(.x)
      ) %>% 
        fct_reorder(.x)
      )
    ),
    across(
      c(defenders_in_the_box), 
      list(fct = ~case_when(
        .x < 4 ~ '<4',
        .x > 8 ~ '>8',
        TRUE ~ as.character(.x)
      ) %>% 
        fct_reorder(.x)
      )
    )
  ) # %>% 
  # select(
  #   -one_of(
  #     sprintf('n_%s', c('rb', 'wr', 'te', 'dl', 'lb', 'db'))
  #   ), 
  #   -c(number_of_pass_rushers, defenders_in_the_box)
  # ) %>% 
  # rename_with(~str_remove(.x, '_fct'), matches('_fct'))
plays_w_pick_info

viz_pick_play_frac <-
  plays_w_pick_info %>% 
  count(tm = possession_team, is_pick_play) %>% 
  group_by(tm) %>% 
  mutate(
    frac = n / sum(n)
  ) %>% 
  ungroup() %>% 
  filter(is_pick_play == 1) %>% 
  mutate(
    rnk = row_number(-frac),
    across(tm, ~fct_reorder(.x, -rnk))
  ) %>% 
  arrange(rnk) %>% 
  ggplot() +
  aes(x = frac, y = tm) +
  geom_col() +
  scale_x_continuous(labels = scales::percent) +
  theme(
    axis.text.y = element_text(size = 12),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    title = '%s of plays involving at least one pick route combination',
    y = NULL,
    x = '% of Team\'s Plays'
  )
viz_pick_play_frac
do_save_plot(viz_pick_play_frac)

# others: 'type_dropback', 'is_defensive_pi', 'personnel_o', 'personnel_d',  'possession_team',
cols_d <-
  c(
    # 'defenders_in_the_box',
    # 'number_of_pass_rushers',
    sprintf('n_%s_fct', c('rb', 'wr', 'te')), # , 'dl', 'lb', 'db')),
    'quarter',
    'down'
  )
cols_c_i_added_nflfastr <- 'wp'
cols_c_i_added <- c(cols_c_i_added_nflfastr, 'pre_snap_score_diff')
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
    intercept = TRUE,
    # intercept = FALSE,
    data = plays_w_pick_info, 
    y = col_y, 
    x_include = cols_features
  )

rec_pick_play_prob <-
  plays_w_pick_info %>%
  recipes::recipe(fmla, data = .)
rec_pick_play_prob

spec_pick_play_prob <-
  parsnip::logistic_reg(
    mixture = tune::tune(),
    penalty = tune::tune()
  ) %>%
  parsnip::set_mode('classification') %>%
  parsnip::set_engine('glmnet')

wf_pick_play_prob_model <-
  workflows::workflow() %>%
  workflows::add_recipe(rec_pick_play_prob) %>%
  workflows::add_model(spec_pick_play_prob)

set.seed(42)
folds_pick_play_prob <-
  plays_w_pick_info %>% 
  rsample::vfold_cv(stata = all_of(col_y), v = 10)
folds_pick_play_prob

grid_params_pick_play_prob <-
  # dials::grid_regular(
  dials::grid_max_entropy(
    dials::mixture(),
    # dials::finalize(dials::mtry(), trn)
    # dials::min_n(),
    dials::finalize(dials::penalty(), plays_w_pick_info),
    size = 10
  )
grid_params_pick_play_prob

require(yardstick)
res_grid <-
  tune::tune_grid(
    spec_pick_play_prob,
    fmla_pick_play_prob,
    resamples = folds_pick_play_prob,
    control = tune::control_grid(verbose = TRUE),
    grid = grid_params_pick_play_prob,
  )
res_grid
pacman::p_unload('yardstick')
# beepr::beep(3)

metrics <- res_grid %>% tune::collect_metrics()
metrics
metrics %>% 
  # filter(.metric == 'roc_auc') %>% 
  arrange(-mean)

res_grid %>% tune::select_best('roc_auc')
res_grid %>% tune::select_best('accuracy')
fit_best <- res_grid %>% tune::select_best('roc_auc')
fit_best

spec_final <- 
  tune::finalize_model(spec_pick_play_prob, fit_best)
spec_final

fit_final <- 
  spec_final %>%
  parsnip::fit(formula = fmla_pick_play_prob, data = plays_w_pick_info)
fit_final

fit_final %>% broom::tidy() %>% arrange(-estimate) %>% mutate(across(term, ~fct_reorder(.x, estimate))) %>% ggplot() + aes(y = term, x = estimate) + geom_point()

fit_pick_play_prob <-
  parsnip::fit(wf_pick_play_prob_model, plays_w_pick_info)
fit_pick_play_prob <- glm(fmla, data = plays_w_pick_info, family = stats::binomial)

pick_plays_probs <-
  # fit_pick_play_prob %>% 
  fit_final %>% 
  broom::augment()

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

viz_pick_play_prob_coefs <-
  coefs_pick_play_prob %>%
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
    legend.position = 'top' # ,
    # legend.box = element_rec()
  ) +
  labs(
    title = 'Play-Level Pick Route Logistic Regression Model Coefficients',
    y = NULL, x = 'Estimate +- 1.96 Standard Error'
  )
viz_pick_play_prob_coefs
do_save_plot(viz_pick_play_prob_coefs)
}

# analyze defense intersections ----
# Only want the ball snap and end rush frames, not the other event frames.
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

features_min <-
  features_min_init %>% 
  anti_join(features_min_end_drop)
features_min

# # Just checking that all of these are 1
# features_min %>%
#   count(game_id, play_id, nfl_id, event) %>%
#   count(n, name = 'nn')

# Adding the `_defender` and `_intersect` columns. Need to join on itself in order to get `had_intersect`, hence the `_init` added to the variable name.

features_lag_init <-
  features_min %>%
  group_by(game_id, play_id, nfl_id) %>%
  mutate(
    nfl_id_d_robust_init = first(nfl_id_d_robust),
    has_same_init_defender = if_else(nfl_id_d_robust == nfl_id_d_robust_init, TRUE, FALSE)
  ) %>%
  ungroup() %>%
  mutate(across(has_same_init_defender, ~if_else(sec == 0, NA, .x))) %>%
  left_join(
    receiver_intersections_adj %>%
      filter(sec <= .sec_cutoff) %>% 
      select(week, game_id, play_id, nfl_id, nfl_id_intersect, sec_intersect = sec, is_lo) %>%
      mutate(has_intersect = TRUE)
  ) %>%
  mutate(
    across(has_intersect, ~coalesce(.x, FALSE)),
    across(has_intersect, ~if_else(sec == 0, NA, .x))
  ) %>% 
  # idk why i get some more plays
  distinct()

# This is to include all seconds distinctly. Adding epa in case it is useful at some point.
# `pick_plays` has epa
features_lag <-
  features_lag_init %>%
  left_join(
    features_lag_init %>%
      filter(has_intersect) %>%
      select(
        week,
        game_id,
        play_id,
        frame_id,
        nfl_id,
        nfl_id_intersect,
        nfl_id_d_robust,
        nfl_id_d_robust_init
      ) %>%
      mutate(had_intersect = TRUE)
  ) %>%
  group_by(game_id, play_id, nfl_id) %>%
  arrange(frame_id, .by_group = TRUE) %>%
  fill(had_intersect) %>%
  ungroup() %>%
  inner_join(
    plays %>%
      select(game_id, play_id, pass_result, epa, nfl_id_target = target_nfl_id)
  ) %>%
  left_join(
    pbp %>%
      select(game_id, play_id, epa, wpa_nflfastr = wpa, epa_nflfastr = epa)
  ) %>% 
  distinct()
features_lag

# Taking the 1 non-snap frame of each play
pick_features <-
  features_lag %>% 
  filter(sec > 0) %>% 
  # `had_intersect` is redundant with `has_intersect` if there is only one frame per play and it's the last frame.
  select(-had_intersect)

# t test ----
if(FALSE) {
pick_features_simple_pretty <-
  pick_features %>%
  filter(nfl_id_target == nfl_id) %>%
  mutate(
    across(
      has_intersect, ~sprintf('Is Pick Combo? = %s', ifelse(.x, 'Y', 'N'))
    )
  ) %>%
  select(has_intersect, epa)
pick_features_simple_pretty

# This should go at the top of the notebook.
t_test_simple <-
  pick_features_simple_pretty %>% 
  tidyr::drop_na() %>% 
  gtsummary::tbl_summary(
    statistic = list(
      gtsummary::all_categorical() ~ '{n} ({p}%)'
    ),
    # label = lab,
    by = has_intersect
  ) %>%
  gtsummary::add_p(
    test = gtsummary::all_continuous() ~ 't.test',
    pvalue_fun = function(x) gtsummary::style_pvalue(x, digits = 2)
  )
t_test_simple

res_t_test_simple <-
  t_test_simple %>% 
  gtsummary::as_gt() %>% 
  gt::gtsave(filename = file.path(get_bdb_dir_figs(), 't_test_simple.png'))

pick_features_pretty <-
  pick_features %>%
  filter(nfl_id_target == nfl_id) %>%
  mutate(
    across(
      has_same_init_defender, ~sprintf('Has Same Initial Defender? = %s', ifelse(.x, 'Y', 'N'))
    ),
    across(
      has_intersect, ~sprintf('Is Pick Combo? = %s', ifelse(.x, 'Y', 'N'))
    )
  ) %>%
  select(has_same_init_defender, has_intersect, matches('^[ew]pa'))
pick_features_pretty

save_new_t_test <- function(what = c('same_init_defender', 'intersect'), cnd = dplyr::quos(TRUE), suffix = NULL, sep = '_') {
  what <- match.arg(what)
  what_other <- 
    switch(
      what, 
      same_init_defender = 'intersect', 
      intersect = 'same_init_defender'
    )
  what_other_pretty <- 
    switch(
      what_other, 
      same_init_defender = 'Has Same Initial Defender?', 
      intersect = 'Is Pick Route Combo?'
    )
  col <- what %>% sprintf('has_%s', .)
  col_other <- what_other %>% sprintf('has_%s', .)
  col_sym <- col %>% sym()
  col_other_sym <- col_other %>% sym()
  # This is hacky but whatever. tbl_summary is not taking symbols
  lab <- list(temp = what_other_pretty)
  names(lab) <- col_other
  suppressMessages(
    t_test <-
      pick_features_pretty %>% 
      dplyr::filter(!!!cnd) %>% 
      tidyr::drop_na() %>% 
      dplyr::mutate(
        dplyr::across(!!col_other_sym, ~stringr::str_replace_all(.x, '(^.*)([YN]$)', '\\2'))
      ) %>% 
      gtsummary::tbl_summary(
        statistic = list(
          # gtsummary::all_continuous() ~ '{mean} ({sd})',
          gtsummary::all_categorical() ~ '{n} ({p}%)'
        ),
        # label = !!col_other_sym ~ what_other_pretty,
        label = lab,
        by = !!col_sym
      ) %>%
      gtsummary::add_p(
        test = gtsummary::all_continuous() ~ 't.test',
        pvalue_fun = function(x) gtsummary::style_pvalue(x, digits = 2)
      )
  )
  t_test
  
  if(is.null(suffix)) {
    suffix <- ''
  } else {
    suffix <- sprintf('%s%s', sep, suffix)
  }
  res <-
    t_test %>% 
    gtsummary::as_gt() %>% 
    gt::gtsave(filename = file.path(get_bdb_dir_figs(), sprintf('t_test_%s%s.png', what, suffix)))
  t_test
}

save_new_t_test('same_init_defender')
save_new_t_test(
  'same_init_defender',
  cnd = dplyr::quos(.data$has_intersect %>% str_detect('N$')), 
  suffix = 'wo_intersect'
)
save_new_t_test(
  'same_init_defender',
  cnd = dplyr::quos(.data$has_intersect %>% str_detect('Y$')), 
  suffix = 'w_intersect'
)
save_new_t_test('intersect')
save_new_t_test(
  'intersect', 
  cnd = dplyr::quos(.data$has_same_init_defender %>% str_detect('N$')),
  suffix = 'w_same_defender'
)
save_new_t_test(
  'intersect', 
  cnd = dplyr::quos(.data$has_same_init_defender %>% str_detect('Y$')),
  suffix = 'w_diff_defender'
)

pick_play_t_test_trunc <-
  pick_features_pretty %>% 
  pivot_longer(
    matches('^[ew]pa'),
    names_to = 'stat',
    values_to = 'value'
  ) %>% 
  mutate(
    across(
      value,
      ~case_when(
        str_detect(stat, 'epa') & .x < -3 ~ -3,
        str_detect(stat, 'epa') & .x > 3 ~ 3,
        stat == 'wpa_nflfastr' & .x < -0.2 ~ -0.2,
        stat == 'wpa_nflfastr' & .x > 0.2 ~ 0.2,
        TRUE ~ .x
      )
    )
  )
pick_play_t_test_trunc

set.seed(42)
viz_t_test <-
  pick_play_t_test_trunc %>% 
  drop_na() %>% 
  # sample_frac(0.1) %>% 
  ggplot() +
  aes(y = has_intersect, x = value) +
  ggbeeswarm::geom_quasirandom(
    aes(color = has_intersect),
    groupOnX = FALSE,
    alpha = 0.2
  ) +
  geom_vline(
    data =
      pick_play_t_test_trunc %>% 
      drop_na() %>% 
      group_by(has_intersect, has_same_init_defender, stat) %>% 
      summarize(
        across(value, median)
      ) %>% 
      ungroup(),
    aes(color = has_intersect, xintercept = value, group = has_intersect),
    size = 1.5
  ) +
  facet_wrap(has_same_init_defender ~ stat, scales = 'free', ncol = 2, nrow = 3, dir = 'v') +
  # scale_color_manual(
  #   values = c('Is Pick Combo? Y' = 'blue', 'Is Pick Combo? N' = 'grey50')
  # ) +
  guides(color = guide_legend('', override.aes = list(size = 3, alpha = 1))) +
  theme(
    strip.text.x = element_text(hjust = 0, size = 14),
    legend.position = 'top',
    plot.caption = element_text(size = 10),
    axis.text.y = element_blank()
  ) +
  labs(
    title = 'Distributions By Initial Defender and Pick Route Combo',
    caption = 'Medians annotated with vertical lines. Extreme values truncated.',
    x = NULL, y = NULL
  )
viz_t_test
do_save_plot(viz_t_test)
}

all_plays <-
  pick_features %>%
  # This is the last second measured on the play.
  select(-sec) %>% 
  left_join(
    players_from_tracking,
    by = c('game_id', 'play_id', 'nfl_id')
  ) %>%
  left_join(
    players_from_tracking %>%
      rename_with(~sprintf('%s_intersect', .x), c(nfl_id, display_name, jersey_number, position)),
    by = c('game_id', 'play_id', 'nfl_id_intersect')
  ) %>%
  left_join(
    players_from_tracking %>%
      rename_with(~sprintf('%s_target', .x), c(nfl_id, display_name, jersey_number, position)),
    by = c('game_id', 'play_id', 'nfl_id_target')
  ) %>%
  mutate(
    across(c(nfl_id_target, jersey_number), ~coalesce(.x, -1L)),
    across(c(display_name, position), ~coalesce(.x, '?'))
  ) %>%
  mutate(
    is_target = if_else(nfl_id == nfl_id_target, TRUE, FALSE),
    target_is_intersect = sum(nfl_id_target == nfl_id)
  ) %>% 
  left_join(
    routes,
    by = c('game_id', 'play_id', 'nfl_id')
  ) %>%
  left_join(
    routes %>%
      rename_with(~sprintf('%s_intersect', .x), c(nfl_id, route)),
    by = c('game_id', 'play_id', 'nfl_id_intersect')
  ) %>%
  left_join(
    players_from_tracking %>% 
      rename_with(~sprintf('%s_d_robust', .x), c(nfl_id, display_name, jersey_number, position)),
    by = c('game_id', 'play_id', 'nfl_id_d_robust')
  ) %>%
  left_join(
    players_from_tracking %>% 
      rename_with(~sprintf('%s_d_robust_init', .x), c(nfl_id, display_name, jersey_number, position)),
    by = c('game_id', 'play_id', 'nfl_id_d_robust_init')
  )
all_plays
usethis::use_data(all_plays, overwrite = TRUE)
