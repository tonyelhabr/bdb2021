
library(tidyverse)
# TODO: Update this with pass_tipped!
features <-
  file.path('inst', 'features.parquet') %>%
  arrow::read_parquet()

features_events <-
  features %>%
  mutate(across(event, ~if_else(.x == '0.0 sec', 'ball_snap', .x))) %>%
  filter(event %>% str_detect('sec$', negate = TRUE)) %>%
  distinct(game_id, play_id, event, frame_id) %>%
  group_by(game_id, play_id, event) %>%
  filter(frame_id == min(frame_id)) %>%
  ungroup()

# features_idx_wide <-
#   features_events %>%
#   group_by(game_id, play_id) %>%
#   mutate(idx = row_number(frame_id)) %>%
#   ungroup() %>%
#   select(-frame_id) %>%
#   pivot_wider(names_from = event, values_from = idx)
#
# event_seqs_n <-
#   features_idx_wide %>%
#   group_by_at(vars(-c(game_id, play_id))) %>%
#   count(sort = TRUE) %>%
#   ungroup()
# event_seqs_n

play_ids_to_drop <-
  features_events %>%
  filter(
    event %in% c('pass_shovel', 'qb_spike', 'pass_tipped')
  ) %>%
  distinct(game_id, play_id)
play_ids_to_drop

features <-
  features %>%
  anti_join(play_ids_to_drop)

events_lag1 <-
  features %>%
  distinct(game_id, play_id, frame_id, event) %>%
  group_by(game_id, play_id) %>%
  arrange(frame_id, .by_group = TRUE) %>%
  mutate(event_lag1 = dplyr::lag(event)) %>%
  ungroup() %>%
  mutate(across(event_lag1, ~coalesce(.x, 'none')))

features <-
  features %>%
  left_join(events_lag1)

nms <- features %>% names()
cols_ball <- nms %>% str_subset('^ball_')
cols_qb <- nms %>% str_subset('^qb_')
cols_od <- nms %>% str_subset('_[od]$|_[od]_')
cols_od
cols_od %>% setdiff(cols_qb)
cols_qb %>% setdiff(cols_od)
cols_rusher <- nms %>% str_subset('rusher') %>% str_subset('nfl_id', negate = TRUE)
col_y <- 'idx_o_target'
cols_id <-
  c(
    'week',
    'game_id',
    'play_id',
    'frame_id'
  )
# nw: 'qb_o', 'los', 'qb_x', 'qb_y', 'dist_qb_ball'
cols_static_value <-
  c(
    'qb_o', 'los', 'qb_x', 'qb_y' # , 'event', 'event_lag1'
  )
cols_id_model <-
  c(
    'event',
    'event_lag1',
    'idx'
  )
cols_static <-
  c(
    cols_id,
    cols_id_model,
    # 'sec',
    # 'yards_to_go',
    cols_static_value
  )
cols_pivot_name <- 'idx_o'
# nw: , 'x', 'y', 'dist_ball', 'dist_d_robust', 'dist_ball_d_robust', 'dist_qb', 'dist_qb_d_robust'
cols_pivot_value <-
  c(
    'x', 'y', 'dist_ball', 'dist_d_robust', 'dist_ball_d_robust', 'dist_d1_naive', 'o', 'o_d_robust' # , 's', 's_d_robust' # , 'a', 'a_d_robust', 'o', 'o_d_robust'
  )
cols_keep <-
  c(
    col_y,
    cols_static,
    cols_pivot_name,
    cols_pivot_value
  )
rgx_pivot_value <- sprintf('^(%s)_[1-5]$', paste(cols_pivot_value, collapse = '|', sep = ''))

features_wide <-
  features %>%
  # head(100) %>%
  filter(event %>% str_detect('sec$') | event == 'pass_forward') %>%
  # count(game_id, play_id, frame_id, nfl_id) %>%
  # distinct(game_id, play_id, frame_id, nfl_id, .keep_all = TRUE) %>%
  group_by(game_id, play_id, frame_id, nfl_id) %>%
  mutate(rn = row_number()) %>%
  filter(rn == max(rn)) %>%
  ungroup() %>%
  select(-rn) %>%
  left_join(
    routes,
    by = c('week', 'game_id', 'play_id', 'nfl_id')
  ) %>%
  mutate(
    across(matches('^x_|_x'), ~los + .x),
    across(c(cols_pivot_value), ~case_when(is.na(route) ~ 9999, TRUE ~ .x)),
    across(idx_o_target, ~coalesce(.x, 9) %>% factor())
  ) %>%
  # idx won't be in here
  select(any_of(cols_keep)) %>%
  pivot_wider(
    names_from = all_of(cols_pivot_name),
    # values_from = setdiff(nms, cols_keep)
    values_from = all_of(cols_pivot_value)
    # values_from = cols_features
  ) %>%
  mutate(idx = row_number()) %>%
  relocate(idx)
features_wide

# features_wide %>% ggplot() + aes(x = x_rusher) + geom_histogram()
features_wide_min <-
  features_wide %>%
  select(
    all_of(col_y),
    all_of(cols_id),
    all_of(cols_id_model),
    all_of(cols_static_value),
    matches(rgx_pivot_value)
  )
features_wide_min
rm('features_wide')

fmla <- paste0(col_y, ' ~ .') %>% as.formula()

rebind_probs <- function(fit, set, nm = deparse(substitute(set))) {
  bind_cols(
    fit %>% predict(set),
    fit %>% predict(set, type = 'prob')
  ) %>%
    mutate(.set = !!nm) %>%
    relocate(.set) %>%
    bind_cols(set %>% select(one_of(c(cols_id, cols_id_model, col_y))))
}

do_rf <- function(trn, tst,  min_n = 2, mtry = 42, trees = 500, suffix = 'w_s_o', overwrite = FALSE) {

  .display_info('Fitting rf for `min_n = {min_n}`, `mtry = {mtry}`, `trees = {trees}` at {Sys.time()}.')
  path_suffix <- sprintf('%s-min_n=%d-mtry=%d-trees=%d', suffix, min_n, mtry, trees)
  .path <- function(prefix, ext) {
    file.path('inst', sprintf('%s-%s.%s', prefix, path_suffix, ext))
  }

  path_fit <- .path('fit', ext = 'rds')
  path_probs <- .path('probs', ext = 'parquet')
  path_acc <- .path('acc', ext = 'csv')
  fit_exists <- path_fit %>% file.exists()
  probs_exist <- path_probs %>% file.exists()
  acc_exists <- path_acc %>% file.exists()

  if(all(fit_exists, probs_exist, acc_exists, !overwrite)) {
    .display_info('Skipping fitting and importing from "{path_acc}".')
    acc <- path_acc %>% read_csv()
    return(acc)
  }

  rec <-
    recipes::recipe(fmla, data = trn) %>%
    recipes::update_role(
      #A idx,
      all_of(cols_id),
      all_of(cols_id_model),
      new_role = 'extra'
    )
  spec <-
    parsnip::rand_forest(
      trees = !!trees,
      min_n = !!min_n,
      mtry = !!mtry
    ) %>%
    parsnip::set_mode('classification') %>%
    # parsnip::set_engine('ranger', importance = 'permutation')
    parsnip::set_engine('ranger')
  spec

  wf <-
    workflows::workflow() %>%
    workflows::add_recipe(rec) %>%
    workflows::add_model(spec)

  fit_ran <- FALSE
  if(!all(fit_exists, !overwrite)) {
    fit <- parsnip::fit(wf, trn)
    write_rds(fit, path_fit)
    fit_ran <- TRUE
  } else {
    .display_info('Skipping fitting.')
  }

  probs_ran <- FALSE
  if(!all(probs_exist, !overwrite)) {
    if(!fit_ran) {
      browser()
      fit <- path_fit %>% read_rds()
    }
    probs <-
      bind_rows(
        rebind_probs(fit, trn, 'trn'),
        rebind_probs(fit, tst, 'tst')
      )
    arrow::write_parquet(probs, path_probs)
    probs_ran <- TRUE
  } else {
    .display_info('Skipping predictions.')
  }

  if(!all(acc_exists, !overwrite)) {
    if(!fit_ran) {
      fit <- path_fit %>% read_rds()
    }
    if(!probs_ran) {
      probs <- path_probs %>% arrow::read_parquet()
    }
    acc <-
      probs %>%
      nest(data = -c(.set, event)) %>%
      mutate(res = map(data, ~yardstick::accuracy(.x, idx_o_target, .pred_class))) %>%
      select(-data) %>%
      unnest(res) %>%
      arrange(.set, event)
    write_csv(acc, path_acc)
  } else {
    acc <- path_acc %>% read_csv()
  }
  acc
}

do_rf_timed <- .time_it(do_rf)

final <- TRUE
if(!final) {
  nms_wide <- features_wide_min %>% names()
  cols_features_wide <- setdiff(nms_wide, c(cols_id, cols_id_model, col_y))
  n_features <- cols_features_wide %>% length()
  grid_params_rf <-
    crossing(
      # min_n = c(2, 10, 25),
      min_n = 2,
      # mtry = c(5, 14, 25)
      mtry = c(5, 7) %>% {. / 8 * n_features} %>% floor()
    )
  # grid_params_rf <- tibble(min_n = 2, mtry = n_features)
  # grid_params_rf
  grid_params_rf

  set.seed(42)
  splits <- features_wide_min %>% rsample::initial_split(strata = !!col_y)
  trn <- splits %>% rsample::training()
  tst <- splits %>% rsample::testing()

  rec <-
    recipes::recipe(fmla, data = trn) %>%
    recipes::update_role(
      #A idx,
      all_of(cols_id),
      all_of(cols_id_model),
      new_role = 'extra'
    ) # %>%
    # recipes::step_dummy(
    #   matches('event')
    # )
  rec

  res_grid_rf <-
    grid_params_rf %>%
    mutate(
      acc =
        map2(
          min_n, mtry,
          ~do_rf_timed(trn = trn, tst = tst, min_n = ..1, mtry = ..2, suffix = 'nw_wo_qb_event_w_d1_s_o')
        )
    )
  res_grid_rf
} else {

  set.seed(42)
  folds <- features_wide_min %>% rsample::vfold_cv(v = 10, strata = all_of(col_y))

  do_rf_fold <- function(fold, name) {
    trn <- fold %>% rsample::analysis()
    tst <- fold %>% rsample::assessment()
    # browser()

    do_rf_timed(trn = trn, tst = tst, min_n = 2, mtry = 38, suffix = sprintf('tp-final-%s', name))
  }

  res_folds <-
    folds %>%
    mutate(res = map2(splits, id, do_rf_fold))
  res_folds

  probs_folds <-
    fs::dir_ls(
      'inst',
      regexp = 'probs-tp-final-Fold.*parquet'
    ) %>%
    map_dfr(~arrow::read_parquet(.x))
  probs_folds
  arrow::write_parquet(probs_folds, file.path('inst', 'probs-tp-final-folds.parquet'))
  # write_rds(fit, path_fit)
  #
  # probs <-
  #   rebind_probs(fit, features_wide_min, 'all')
  # probs
  # probs %>% count(idx_o_target)
  #
  # probs_long_filt <-
  #   probs %>%
  #   select(-c(.set, .pred_class)) %>%
  #   filter(event == 'pass_forward') %>%
  #   mutate(across(c(idx_o_target), as.integer)) %>%
  #   pivot_longer(
  #     matches('[.]pred_[1-9]'),
  #     names_to = 'name',
  #     values_to = 'prob'
  #   ) %>%
  #   separate(name, into = c('dummy', 'idx_o_target_pred'), sep = '_') %>%
  #   mutate(across(idx_o_target_pred, as.integer)) %>%
  #   filter(idx_o_target == idx_o_target_pred)
  # probs_long_filt
  #
  # viz_probs_filt <-
  #   probs_long_filt %>%
  #   mutate(across(idx_o_target_pred, factor)) %>%
  #   ggplot() +
  #   aes(x = prob) +
  #   geom_histogram(binwidth = 0.05) +
  #   facet_wrap(~idx_o_target_pred, scales = 'free')
  # arrow::write_parquet(probs, path_probs)
  #
  # acc <-
  #   probs %>%
  #   nest(data = -c(.set, event)) %>%
  #   mutate(res = map(data, ~yardstick::accuracy(.x, idx_o_target, .pred_class))) %>%
  #   select(-data) %>%
  #   unnest(res) %>%
  #   arrange(.set, event)
  # acc
  #
  # write_csv(acc, path_acc)
}
