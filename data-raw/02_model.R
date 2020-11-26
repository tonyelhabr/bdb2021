
library(tidyverse)
# TODO: Update this with pass_tipped!
features <- file.path('inst', 'features.parquet') %>% arrow::read_parquet()

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

# 11/4 (black github hoodie), 3:07:20
# cone relative x,y to the sideline from the back of the play
# one_week.loc[one_week['toLeft'], 'x_std'] = 120 - one_week.loc[one_week[toLeft'], 'x']
# one_week.loc[one_week['toLeft'], 'y_std'] = 160/3 - one_week.loc[one_week[toLeft'], 'y']
# .idx <- 0:4
# cols_features_tp_nw <-
#   c(
#     'qb_o', # qb_o
#     'los', # los
#     sprintf('receiver_%d', .idx), # dist_qb
#     sprintf('receiver_id_%d', .idx), # nfl_id
#     'qb_football', # ?
#     sprintf('football_%d', .idx), # dist_ball
#     sprintf('nearest_defender_%d', .idx), # dist_d1_naive
#     sprintf('def_football', .idx), # dist_ball_d1_naive
#     sprintf('def_qb', .idx), # ?
#     sprintf('receiver_cone_x_%d', .idx), # x
#     sprintf('receiver_cone_y_%d', .idx), # y
#     sprintf('qb_cone_', c('x', 'y')) # qb_x, qb_y
#   )
#
# # 11/11 (terrapins shirt), 2:25:20
# # log loss of 0.40, accuracy of .845, 2:30:53
# cols_features_cp_nw <-
#   c(
#     'qb_o', # qb_o
#     'los', # los
#     sprintf('qb_cone_', c('x', 'y')) # qb_x, qb_y
#     'receiver', # dist_ball
#     'receiver_id', # nfl_id
#     'football', # dist_ball
#     'nearest_defender', # dist_d1_naive
#     'defender_id', # nfl_id_d1_naive
#     'def_football', # dist_ball_d1_naive
#     'def_qb' # dist_qb_d1_naive
#     'receiver_cone_x', # x
#     'receiver_cone_y', # y
#   )
# # columns added afterwards: event, success (0 or 1), success_rf_pred, dpoe (not used in model?)


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
cols_static_value <-
  c(
    'qb_o', 'los', 'dist_qb_ball', 'qb_x', 'qb_y'
  )
cols_static <-
  c(
    cols_id,
    'event',
    # 'sec',
    # 'yards_to_go',
    cols_static_value
  )
cols_pivot_name <- c('idx_o')

cols_pivot_value <-
  c(
    'dist_qb', 'dist_ball', 'dist_d_robust', 'dist_ball_d_robust', 'dist_qb_d_robust', 'x', 'y'
  )
cols_keep <-
  c(
    col_y,
    cols_static,
    cols_pivot_name,
    cols_pivot_value
  )
rgx_pivot_value <- sprintf('^(%s)_[1-5]$', paste(cols_pivot_value, collapse = '|', sep = ''))
# features_n <- features %>% count(game_id, play_id, frame_id, event)
# features_n %>% count(n, name = 'nn') # Should always be 5

# cols_shared_features <- c('qb_o', 'los', 'qb_x', 'qb_y')
# cols_individual_features <- c('dist_ball', 'dist_d1_naive', 'dist_ball_d1_naive', 'dist_qb_d1_naive', 'x', 'y')
# rgx_individual_features <- sprintf('^(%s)_[1-5]$', paste(cols_individual_features, collapse = '|', sep = ''))
# rgx_individual_features
# features %>%
#   filter(event == 'pass_forward') %>%
#   filter(!is.na(nfl_id_target) & is.na(nfl_id_target_d_robust)) %>%
#   count(week)

features %>%
  # sample_n(20) %>%
  head(20) %>%
  left_join(routes) %>%
  relocate(route) %>%
  select(route, one_of(cols_keep)) %>%
  mutate(
    across(matches('^x_|_x'), ~los + .x),
    across(one_of(cols_pivot_value), ~case_when(is.na(route) ~ 9999, TRUE ~ .x)),
    across(idx_o_target, ~coalesce(.x, -1) %>% factor())
  )

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
  left_join(routes) %>%
  mutate(
    across(matches('^x_|_x'), ~los + .x),
    across(c(cols_pivot_value), ~case_when(is.na(route) ~ 9999, TRUE ~ .x)),
    across(idx_o_target, ~coalesce(.x, -1) %>% factor())
  ) %>%
  select(one_of(cols_keep)) %>%
  pivot_wider(
    names_from = all_of(cols_pivot_name),
    # values_from = setdiff(nms, cols_keep)
    values_from = all_of(cols_pivot_value)
    # values_from = cols_features
  ) %>%
  # filter(!is.na(idx_o_target)) %>%
  mutate(idx = row_number()) %>%
  relocate(idx)
features_wide # %>% count(idx_o_target)
# features_wide %>% skimr::skim()

# features_wide %>% ggplot() + aes(x = x_rusher) + geom_histogram()
features_wide_min <-
  features_wide %>%
  select(
    idx,
    event,
    all_of(col_y),
    one_of(cols_static_value),
    matches(rgx_pivot_value)
  )
features_wide_min

set.seed(42)
splits <- features_wide_min %>% rsample::initial_split(strata = col_y)
trn <- splits %>% rsample::training()
tst <- splits %>% rsample::testing()

fmla <- paste0(col_y, ' ~ .') %>% as.formula()

rec <-
  recipes::recipe(fmla, data = trn) %>%
  recipes::update_role(
    idx,
    new_role = 'extra'
  ) %>%
  recipes::step_dummy(
    event
  )
rec

# spec <-
#   parsnip::boost_tree(
#     trees = 1000,
#     # Model complexity
#     min_n = tune::tune(),
#     tree_depth = tune::tune(),
#     loss_reduction = tune::tune(),
#     # Randomness
#     sample_size = tune::tune(),
#     mtry = tune::tune(),
#     # Step size
#     learn_rate = tune::tune()
#   ) %>%
#   parsnip::set_mode('classification') %>%
#   parsnip::set_engine('xgboost')
# spec

spec <-
  parsnip::rand_forest(
    trees = 500,
    min_n = 10,
    mtry = trn %>% ncol() %>%  {. / 3} %>% floor()
  ) %>%
  parsnip::set_mode('classification') %>%
  parsnip::set_engine('ranger', importance = 'permutation')
spec

wf <-
  workflows::workflow() %>%
  workflows::add_recipe(rec) %>%
  workflows::add_model(spec)

# debugonce(parsnip::fit)
# debugonce(workflows:::.fit_model)
# debugonce(workflows:::fit_from_xy)
fit <- parsnip::fit(wf, trn)
write_rds(fit, file.path('inst', 'fit_rf.rds'))
rebind_probs <- function(set, nm = deparse(substitute(set))) {
  bind_cols(
    fit %>% predict(set),
    fit %>% predict(set, type = 'prob')
  ) %>%
    mutate(.set = !!nm) %>%
    relocate(.set) %>%
    bind_cols(set)
}

probs <- bind_rows(trn %>% rebind_probs('trn'), tst %>% rebind_probs('tst'))
probs %>% count(.set, .pred_class)

probs %>%
  # filter(.set == 'trn') %>%
  # group_by(.set) %>%
  nest(data = -c(.set, event)) %>%
  mutate(res = map(data, ~yardstick::accuracy(.x, idx_o_target, .pred_class))) %>%
  select(-data) %>%
  unnest(res) %>%
  arrange(.set, event)


fit_wf <- fit %>% workflows::pull_workflow_fit()

trn_jui <-
  rec %>%
  recipes::prep(training = trn) %>%
  recipes::juice()
x_trn_jui <-  trn_jui[, setdiff(names(trn_jui), col_y)] %>% as.matrix()
y_trn_jui <- trn_jui[[col_y]] %>% as.integer() - 1L

f_predict <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}

vip_wrapper <- function(method, ...) {
  res <-
    vip::vip(
      method = method,
      ...
    ) %>%
    pluck('data') %>%
    # Will get a "Sign" solumn when using the default `method = 'model'`.
    rename(var = Variable, imp = Importance)

  if(any(names(res) == 'Sign')) {
    res <-
      res %>%
      mutate(dir = ifelse(Sign == 'POS', +1L, -1L)) %>%
      mutate(imp = dir * imp)
  }
  res
}

vip_wrapper_partial <-
  partial(
    vip_wrapper,
    object = fit_wf$fit,
    num_features = x_trn_jui %>% ncol(),
    ... =
  )

metric <- 'sse'
vip_wrapper_partial_permute <-
  partial(
    vip_wrapper_partial,
    method = 'permute',
    metric = metric,
    pred_wrapper = f_predict,
    ... =
  )

set.seed(42)
vip_wrapper_partial_shap <-
  partial(
    vip_wrapper_partial,
    method = 'shap',
    train = x_trn_jui,
    ... =
  )


vi_vip_model <- vip_wrapper_partial(method = 'model')
vi_vip_permute <-
  vip_wrapper_partial_permute(
    train = x_trn_jui %>% as.data.frame(),
    target = y_trn_jui
  )
vi_vip_shap <- vip_wrapper_partial_shap(pred_wrapper = f_predict)


probs %>%
  # mutate(across(c(.pred_class), as.integer)) %>%
  yardstick::roc_aunp(truth = idx_o_target, matches('[.]pred_[1-5]'))
probs %>%
  # mutate(across(c(.pred_class), as.integer)) %>%
  yardstick::accuracy(truth = idx_o_target, matches('[.]pred_[1-5]'))

spec <-
  parsnip::rand_forest(
    trees = 500,
    # Model complexity
    min_n = tune::tune(),
    # Randomness
    mtry = tune::tune()
  ) %>%
  parsnip::set_mode('classification') %>%
  parsnip::set_engine('ranger')
spec

wf <-
  workflows::workflow() %>%
  workflows::add_recipe(rec) %>%
  workflows::add_model(spec)

# params_grid <-
#   dials::grid_latin_hypercube(
#     dials::tree_depth(),
#     dials::min_n(),
#     dials::loss_reduction(),
#     sample_size = dials::sample_prop(),
#     dials::finalize(dials::mtry(), trn),
#     dials::learn_rate(),
#     size = 30
# )
# params_grid

params_grid <-
  dials::grid_latin_hypercube(
    dials::min_n(),
    dials::finalize(dials::mtry(), trn),
    size = 10
  )
params_grid

folds <- trn %>% rsample::vfold_cv(strata = col_y)
# fit <- wf %>% parsnip::fit(trn)
# cl <- parallel::makeCluster(2)
# doParallel::registerDoParallel(cl)
# require(yardstick)
res_tune <-
  tune::tune_grid(
    wf,
    resamples = folds,
    # metric = yardstick::metric_set(yardstick::roc_auc),
    control = tune::control_grid(verbose = TRUE)
  )
write_rds(res_tune, file.path('inst', 'res_tune_rf.rds'))
metrics <-
  res_tune %>%
  tune::collect_metrics()
metrics
res_tune %>% tune::select_best(metric = 'accuracy')
