
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

# start eda ----
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

if(FALSE) {
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

# t test method 1 ----
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

# save_t_test <- function(what = c('same_init_defender', 'intersect')) {
#   what <- match.arg(what)
#   what_other <- switch(what, same_init_defender = 'intersect', intersect = 'same_init_defender')
#   col <- what %>% sprintf('has_%s', .)
#   col_other <- what_other %>% sprintf('has_%s', .)
#   col_sym <- col %>% sym()
#   col_other_sym <- col_other %>% sym()
# 
#   t_test <-
#     pick_features_pretty %>% 
#     dplyr::select(-!!col_other_sym) %>% 
#     tidyr::drop_na() %>% 
#     gtsummary::tbl_summary(
#       by = !!col_sym
#     ) %>%
#     gtsummary::add_p(
#       test = gtsummary::all_continuous() ~ 't.test',
#       pvalue_fun = function(x) gtsummary::style_pvalue(x, digits = 2)
#     )
#   res <-
#     t_test %>% 
#     gtsummary::as_gt() %>% 
#     gt::gtsave(filename = file.path(get_bdb_dir_figs(), sprintf('t_test_%s.png', what)))
#   t_test
# }
# 
# save_t_test('same_init_defender')
# save_t_test('intersect')

# t-test method 2 (best) ----
if(FALSE) {
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
    gt::gtsave(filename = file.path(get_bdb_dir_figs(), sprintf('new_t_test_%s%s.png', what, suffix)))
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
}
# t-test method 3 ----
# pick_features_target_long <-
#   pick_features %>% 
#   filter(nfl_id_target == nfl_id) %>% 
#   select(has_same_init_defender, has_intersect, matches('^[ew]pa')) %>% 
#   pivot_longer(
#     matches('^[ew]pa'),
#     names_to = 'stat',
#     values_to = 'value'
#   ) %>% 
#   group_nest(has_same_init_defender, has_intersect, stat) %>% 
#   mutate(
#     across(has_intersect, ~if_else(.x, 'has_intersect', 'not_has_intersect')),
#     across(has_same_init_defender, ~if_else(.x, 'has_same_init_defender', 'not_has_same_init_defender'))
#   )
# pick_features_target_long
# 
# do_t_test <- function(col1, col2 = sprintf('not_%s', col1)) {
#   col1_sym <- col1 %>% sym()
#   col2_sym <- col2 %>% sym()
#   pick_features_target_long %>% 
#     pivot_wider(
#       names_from = !!col1_sym,
#       values_from = data
#     ) %>% 
#     mutate(
#       t_test = map2(!!col1_sym, !!col2_sym, ~t.test(..1$value, ..2$value) %>% broom::tidy()),
#       !!col1_sym := map_int(!!col1_sym, nrow),
#       !!col2_sym := map_int(!!col2_sym, nrow)
#     ) %>% 
#     unnest(cols = c(t_test))
# }
# 
# pick_init_defender_t_test <- do_t_test('has_same_init_defender')
# pick_init_defender_t_test
# pick_intersect_t_test <- do_t_test('has_intersect')
# pick_intersect_t_test

# t test vizzing ----
if(FALSE) {
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
        str_detect(stat, 'epa') & .x < -2 ~ -2,
        str_detect(stat, 'epa') & .x > 2 ~ 2,
        stat == 'wpa_nflfastr' & .x < -0.1 ~ -0.1,
        stat == 'wpa_nflfastr' & .x > 0.1 ~ 0.1,
        TRUE ~ .x
      )
    )
  )
pick_play_t_test_trunc

# pick_play_t_test_trunc %>% 
#   sample_frac(0.2) %>% 
#   ggplot() +
#   aes(y = has_intersect, x = value) +
#   geom_boxplot(
#     aes(color = has_intersect),
#     alpha = 0.2
#   ) +
#   facet_wrap(stat ~ has_same_init_defender, scales = 'free', ncol = 2, nrow = 3)

# Skipping this for now since it crashes the session when the script is sourced.

set.seed(42)
# theme_set_and_update_bdb()
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

# example vizzes ----
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

pick_plays <-
  all_plays %>%
  filter(has_intersect)

agg_plays_by <- function(data, ...) {
  data %>% 
    group_by(has_intersect, is_lo, ...) %>% 
    summarize(
      n = n(),
      across(matches('^[ew]pa'), list(sum = sum, mean = mean), na.rm = TRUE)
    ) %>% 
    ungroup()
}

agg_all_plays_by <- partial(agg_plays_by, data = all_plays, ... =)
agg_pick_plays_by <- partial(agg_plays_by, data = pick_plays, ... =)

all_plays_by_defender <-
  agg_all_plays_by(is_target, has_same_init_defender, nfl_id_d_robust, display_name_d_robust, nfl_id_d_robust_init, display_name_d_robust_init)
all_plays_by_defender

pick_plays_by_defender <-
  all_plays_by_defender %>% 
  group_by(nfl_id = nfl_id_d_robust, display_name = display_name_d_robust, is_target, has_intersect, has_same_init_defender) %>% 
  summarize(
    n = sum(n),
    across(matches('^[ew]pa.*sum$'), sum, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  group_by(nfl_id, display_name) %>% 
  mutate(
    total = sum(n),
    frac = n / total
  ) %>% 
  ungroup() %>% 
  arrange(desc(n))
pick_plays_by_defender

# Have to re-adjust total numbers to just the filtering criteria.
pick_plays_by_defender_wide_intersect_adj <-
  pick_plays_by_defender %>% 
  filter(is_target) %>% 
  group_by(nfl_id, display_name, is_target) %>% 
  mutate(
    total = sum(n)
  ) %>% 
  ungroup() %>%
  mutate(frac = n / total) %>% 
  pivot_wider(
    names_from = c(has_intersect),
    values_from = c(n, frac, matches('^[ew]pa')),
    values_fill = list(n = 0L, 0)
  ) %>% 
  filter(total >= 10)
pick_plays_by_defender_wide_intersect_adj

pick_plays_by_defender_wide_intersect_adj_annotate_epa_TRUE <-
  bind_rows(
    pick_plays_by_defender_wide_intersect_adj %>% 
      arrange(epa_sum_TRUE) %>% 
      head(5) %>%
      mutate(grp = 'top'),
    pick_plays_by_defender_wide_intersect_adj %>% 
      arrange(-epa_sum_TRUE) %>% 
      head(5) %>% 
      mutate(grp = 'bottom')
  )

pick_plays_by_defender_wide_intersect_adj_annotate_epa_FALSE <-
  bind_rows(
    pick_plays_by_defender_wide_intersect_adj %>% 
      anti_join(pick_plays_by_defender_wide_intersect_adj_annotate_epa_TRUE) %>% 
      arrange(epa_sum_FALSE) %>% 
      head(5) %>%
      mutate(grp = 'top'),
    pick_plays_by_defender_wide_intersect_adj %>% 
      anti_join(pick_plays_by_defender_wide_intersect_adj_annotate_epa_TRUE) %>% 
      arrange(-epa_sum_FALSE) %>% 
      head(5) %>% 
      mutate(grp = 'bottom')
  )

pts <- function (x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

viz_pick_plays_by_defender_wide_intersect_adj <-
  pick_plays_by_defender_wide_intersect_adj %>% 
  ggplot() +
  aes(y = epa_sum_FALSE, x = epa_sum_TRUE) +
  geom_abline(
    data = tibble(intercept = seq(-30, 30, by = 10), slope = -1),
    aes(intercept = intercept, slope = slope),
    linetype = 2
  ) +
  geom_point(
    # data = pick_plays_by_defender_wide_intersect_adj %>% anti_join(pick_plays_by_defender_wide_intersect_adj_annotate),
    aes(size = total), alpha = 0.2
  ) +
  geom_point(
    data = pick_plays_by_defender_wide_intersect_adj_annotate_epa_TRUE %>% filter(grp == 'top'),
    aes(size = total),
    show.legend = FALSE,
    color = 'dodgerblue'
  ) +
  ggrepel::geom_text_repel(
    data = pick_plays_by_defender_wide_intersect_adj_annotate_epa_TRUE %>% filter(grp == 'top'),
    aes(label = display_name),
    show.legend = FALSE,
    family = 'Karla',
    color = 'dodgerblue'
  ) +
  geom_point(
    data = pick_plays_by_defender_wide_intersect_adj_annotate_epa_TRUE %>% filter(grp == 'bottom'),
    aes(size = total),
    show.legend = FALSE,
    color = 'darkorange'
  ) +
  ggrepel::geom_text_repel(
    data = pick_plays_by_defender_wide_intersect_adj_annotate_epa_TRUE %>% filter(grp == 'bottom'),
    aes(label = display_name),
    show.legend = FALSE,
    family = 'Karla',
    color = 'darkorange'
  ) +
  geom_point(
    data = pick_plays_by_defender_wide_intersect_adj_annotate_epa_FALSE %>% filter(grp == 'top'),
    aes(size = total),
    show.legend = FALSE,
    color = 'indianred'
  ) +
  ggrepel::geom_text_repel(
    data = pick_plays_by_defender_wide_intersect_adj_annotate_epa_FALSE %>% filter(grp == 'top'),
    aes(label = display_name),
    show.legend = FALSE,
    family = 'Karla',
    color = 'indianred'
  ) +
  geom_point(
    data = pick_plays_by_defender_wide_intersect_adj_annotate_epa_FALSE %>% filter(grp == 'bottom'),
    aes(size = total),
    show.legend = FALSE,
    color = 'forestgreen'
  ) +
  ggrepel::geom_text_repel(
    data = pick_plays_by_defender_wide_intersect_adj_annotate_epa_FALSE %>% filter(grp == 'bottom'),
    aes(label = display_name),
    show.legend = FALSE,
    family = 'Karla',
    color = 'forestgreen'
  ) +
  geom_text(
    aes(x = 5.1, y = 45, label = 'Best on pick plays'), color = 'dodgerblue', size = pts(14), hjust = 0
  ) +
  geom_text(
    aes(x = 5.1, y = 42, label = 'Worst on pick plays'), color = 'darkorange', size = pts(14), hjust = 0
  ) +
  geom_text(
    aes(x = 5.1, y = 39, label = 'Best on non-pick plays'), color = 'indianred', size = pts(14), hjust = 0
  ) +
  geom_text(
    aes(x = 5.1, y = 36, label = 'Worst on non-pick plays'), color = 'forestgreen', size = pts(14), hjust = 0
  ) +
  guides(
    size = guide_legend(title = '# of total plays', override.aes = list(alpha = 1))
  ) +
  # coord_equal(xlim = c(-30, 30)) +
  theme(
    plot.caption = element_text(size = 10),
    legend.position = 'top'
  ) +
  labs(
    title = 'Aggreggate EPA when covering targeted receiver',
    caption = 'Minimum of 10 pick plays covered.\nPlot does not differentiate based on type of coverage (e.g. man). Defender at time of thow is used.',
    y = 'EPA on non-pick plays',
    x = 'EPA on pick plays'
  )
viz_pick_plays_by_defender_wide_intersect_adj
do_save_plot(viz_pick_plays_by_defender_wide_intersect_adj)

all_plays_by_receiver <-
  all_plays %>% 
  count(has_intersect, nfl_id, display_name, is_lo, sort = TRUE)
all_plays_by_receiver

all_plays_by_receiver <-
  all_plays %>% 
  group_by(has_intersect, nfl_id, display_name, is_lo) %>% 
  summarize(
    n = n(),
    across(matches('^[ew]pa'), mean)
  ) %>% 
  ungroup()
all_plays_by_receiver

all_plays_by_receiver %>% 
  filter(has_intersect) %>% 
  filter(n >= 25) %>% 
  arrange(desc(epa))

.n_top <- 20L
.plot_picks_by_reciever_layers <- function(...) {
  list(
    aes(x = ..., y = display_name),
    # geom_col(aes(fill = is_lo)),
    scale_fill_manual(values = c(`TRUE` = 'dodgerblue', `FALSE` = 'darkorange')),
    guides(fill = guide_legend('Is Underneath Route Runner?')),
    theme(
      panel.grid.major.y = element_blank(),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.position = 'top'
    ),
    labs(
      x = '# of Plays',
      y = NULL
    )
  )
}

plot_picks_by_receiver <- function(data) {
  data %>%
    inner_join(
      data %>% 
        group_by(nfl_id, display_name) %>% 
        summarize(total = sum(n)) %>% 
        ungroup() %>% 
        mutate(
          rnk = row_number(desc(total)),
        ) %>% 
        filter(rnk <= .n_top)
    ) %>% 
    mutate(
      across(display_name, ~fct_reorder(.x, -rnk))
    ) %>% 
    arrange(rnk) %>% 
    # TODO: Replace these with above function.
    ggplot() +
    aes(x = n, y = display_name) +
    geom_col(aes(fill = is_lo)) +
    scale_fill_manual(values = c(`TRUE` = 'dodgerblue', `FALSE` = 'darkorange')) +
    guides(fill = guide_legend('Is Underneath Route Runner?')) +
    theme(
      panel.grid.major.y = element_blank(),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.position = 'top'
    ) +
    labs(
      x = '# of Plays',
      y = NULL
    )
}

viz_picks_by_receiver <-
  all_plays_by_receiver %>%
  filter(has_intersect) %>% 
  plot_picks_by_receiver() +
  labs(
    title = '# of Pick Route Combinations Involved With'
  )
viz_picks_by_receiver
do_save_plot(viz_picks_by_receiver)

all_plays_by_receiver_target <-
  all_plays %>% 
  # Drop the NA target rows.
  filter(!is.na(display_name_target)) %>% 
  count(has_intersect, nfl_id = nfl_id_target, display_name = display_name_target, is_lo, sort = TRUE)
all_plays_by_receiver_target

# TODO: Don't highlight is lo in this since it's not what is being ephasized.
viz_picks_by_receiver_target <-
  all_plays_by_receiver_target %>%
  filter(has_intersect) %>% 
  plot_picks_by_receiver() +
  labs(
    title = '# of targets When Involved In Pick Route Combination'
  )
viz_picks_by_receiver_target
do_save_plot(viz_picks_by_receiver_target)

pick_plays_by_receiver <-
  all_plays_by_receiver %>% 
  group_by(nfl_id, display_name, has_intersect) %>% 
  summarize(
    n = sum(n)
  ) %>% 
  ungroup() %>% 
  group_by(nfl_id, display_name) %>% 
  mutate(
    total = sum(n),
    frac = n / total
  ) %>% 
  ungroup()
pick_plays_by_receiver

viz_frac_by_receiver <-
  pick_plays_by_receiver %>% 
  # select(-total) %>% 
  pivot_wider(
    names_from = has_intersect,
    values_from = c(n, frac)
  ) %>% 
  ggplot() +
  aes(x = total, y = n_TRUE) +
  geom_point() +
  geom_smooth(method = 'lm', formula = formula(y ~ x + 0), se = FALSE, color = 'black', linetype = 2) +
  geom_point(
    data = 
      pick_plays_by_receiver_top,
    aes(x = total, y = n),
    color = 'red'
  ) +
  ggrepel::geom_text_repel(
    data = 
      pick_plays_by_receiver_top,
    aes(x = total, y = n, label = display_name),
    family = 'Karla',
    segment.color = 'red',
    segment.size = 0.2,
    color = 'red'
  ) +
  theme(
    plot.caption = element_text(size = 10),
  ) +
  labs(
    title = 'Relative Number of Pick Routes Ran',
    caption = 'Players with highest ratio of pick plays annotated (minimum 200 plays).\nLinear regression fit shown as dotted line',
    x = '# of plays',
    y = '# of plays Involved in pick route combination'
  )
viz_frac_by_receiver
save_plot(viz_frac_by_receiver)

# TODO
Matching::Match(
  Y = plays_w_pick_info$epa,
  Tr = plays_w_pick_info$is_pick_play,
  X = fitted(fit_pick_play_prob$fit$fit$fit),
  ties = FALSE,
  estimand = 'ATT'
)


# pick_plays_agg <-
#   all_plays %>%
#   filter(has_intersect) %>% 
#   mutate(
#     pass_complete = if_else(pass_result == 'C', TRUE, FALSE),
#     across(target_is_intersect, ~.x %>% as.logical() ),
#     across(target_is_intersect, ~if_else(is.na(.x), FALSE, .x))
#   ) %>%
#   # filter(has_same_init_defender, target_is_intersect, is_lo) %>%
#   group_by(has_intersect, has_same_init_defender, target_is_intersect, is_lo, pass_complete) %>%
#   summarize(
#     n = n(),
#     across(c(epa, wpa_nflfastr, epa_nflfastr), mean, na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   group_by(has_same_init_defender, target_is_intersect, is_lo) %>%
#   mutate(
#     frac = n / sum(n)
#   ) %>%
#   ungroup()
# pick_plays_agg

# Pick an example play from here
pick_plays_meta_viz <-
  pick_plays %>% 
  mutate(pass_complete = if_else(pass_result == 'C', TRUE, FALSE)) %>%
  filter(!is.na(target_is_intersect)) %>%
  group_by(sec_intersect, pass_complete, is_lo, target_is_intersect, has_same_init_defender) %>%
  mutate(prnk = percent_rank(epa)) %>%
  filter(prnk == min(prnk) | prnk == max(prnk)) %>%
  ungroup() %>%
  mutate(high_epa = if_else(prnk == 1, TRUE, FALSE)) %>%
  filter(high_epa == pass_complete) %>%
  arrange(sec_intersect, pass_complete, is_lo, target_is_intersect) %>%
  filter(is_lo) %>%
  inner_join(plays %>% select(game_id, play_id, yards_gained = play_result)) %>% 
  mutate(
    lab = glue::glue('Pick between {display_name} ({jersey_number}, {position}) and {display_name_intersect} ({jersey_number_intersect}, {position_intersect}) between {sec_intersect-0.5} and {sec_intersect} seconds.
                     target: {display_name_target} ({jersey_number_target}, {position_target}). Play result: {pass_result}. Yards gained: {yards_gained}.
                     BDB EPA: {scales::number(epa, accuracy = 0.01)}, nflfastR EPA: {scales::number(epa_nflfastr, accuracy = 0.01)}, nflfastR WPA: {scales::number(wpa_nflfastr, accuracy = 0.01)}'),
    path = file.path(
      bdb2021:::get_bdb_dir_figs(), 
      sprintf(
        'is_pick_play=%s-sec=%1.1f-pass_complete=%s-is_lo=%s-target_is_intersect=%s-high_epa=%s-%s-%s.png', 'Y', 
        sec_intersect, 
        ifelse(pass_complete, 'Y', 'N'), 
        ifelse(is_lo, 'Y', 'N'), 
        ifelse(target_is_intersect, 'Y', 'N'), 
        ifelse(high_epa, 'Y', 'N'), game_id, play_id)
    )
  )
pick_plays_meta_viz

primary_example_pick_plays <-
  list(
    pick_plays_meta_viz %>% 
      filter(high_epa) %>% 
      slice_max(epa, with_ties = FALSE) %>% 
      mutate(descr = 'highest_epa'),
    pick_plays_meta_viz %>% 
      filter(!high_epa) %>% 
      slice_min(epa, with_ties = FALSE) %>% 
      mutate(descr = 'lowest_epa'),
    pick_plays_meta_viz %>% 
      filter(high_epa) %>% 
      filter(sec_intersect == min(sec_intersect)) %>% 
      slice_max(epa, with_ties = FALSE) %>% 
      mutate(descr = 'y_buffer')
  ) %>% 
  reduce(bind_rows) %>% 
  mutate(
    path = file.path(dirname(path), sprintf('%s_pick_play.png', descr))
  )
primary_example_pick_plays

pick_plays_meta_viz_wo_primary <-
  pick_plays_meta_viz %>% 
  anti_join(primary_example_pick_plays %>% select(game_id, play_id))

secondary_example_pick_plays <-
  list(
    pick_plays_meta_viz_wo_primary %>% 
      filter(high_epa) %>% 
      filter(has_same_init_defender) %>% 
      slice_max(epa, with_ties = FALSE) %>% 
      mutate(descr = 'highest_epa_w_same_defender'),
    pick_plays_meta_viz_wo_primary %>% 
      filter(high_epa) %>% 
      filter(!has_same_init_defender) %>% 
      slice_max(epa, with_ties = FALSE) %>% 
      mutate(descr = 'highest_epa_w_diff_defender'),
    pick_plays_meta_viz_wo_primary %>% 
      filter(!high_epa) %>% 
      filter(has_same_init_defender) %>% 
      slice_min(epa, with_ties = FALSE) %>% 
      mutate(descr = 'lowest_epa_w_same_defender'),
    pick_plays_meta_viz_wo_primary %>% 
      filter(!high_epa) %>% 
      filter(!has_same_init_defender) %>% 
      slice_min(epa, with_ties = FALSE) %>% 
      mutate(descr = 'lowest_epa_w_diff_defender')
  ) %>% 
  reduce(bind_rows) %>% 
  mutate(
    path = file.path(dirname(path), sprintf('%s_pick_play.png', descr))
  )
secondary_example_pick_plays

res_viz <-
  # pick_plays_meta_viz %>%
  # filter(target_is_intersect & is_lo & sec >= 1 & sec <= 3) %>%
  list(
    primary_example_pick_plays,
    secondary_example_pick_plays
  ) %>% 
  reduce(bind_rows) %>% 
  # slice(3) %>% 
  mutate(
    viz = pmap(
      list(game_id, play_id, lab),
      ~animate_play(game_id = ..1, play_id = ..2, save = FALSE) +
        labs(subtitle = ..3),
    ),
    res = map2(viz, path, ~ggsave(filename = ..2, plot = ..1)) # , unit = 'in', height = 10, width = 10))
  )

# pick_play_agg <-
#   pick_plays %>%
#   # Drop the plays where the target receiver is NA.
#   # drop_na() %>%
#   # filter(!is.na(target_is_intersect)) %>%
#   mutate(across(pass_result, ~if_else(.x != 'C', 'I', .x))) %>%
#   group_by(target_is_intersect, sec, pass_result) %>%
#   summarize(
#     n = n(),
#     across(c(ends_with('_nflfastr'), epa), mean, na.rm = TRUE)
#   ) %>%
#   ungroup() # %>%
# pick_play_agg
