
library(tidyverse)
features <-
  arrow::read_parquet(file.path('inst', 'features.parquet'))

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
features_n <-
  features %>%
  count(week, game_id, play_id, frame_id, sec)

features_n %>%
  count(n, name = 'nn')

bad_features <- features_n %>% filter(n != 5L)

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

set.seed(42)
splits <-
  features_wide %>%
  rsample::initial_split(strata = col_y)
trn <- splits %>% rsample::training()
tst <- splits %>% rsample::testing()

fmla <- paste0(col_y, ' ~ .') %>% as.formula()

rec <-
  recipes::recipe(fmla, data = trn) %>%
  recipes::update_role(
    # dplyr::matches('^nfl_id_'),
    # dplyr::matches('_id$'),
    idx,
    new_role = 'extra'
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
    trees = 1000
  ) %>%
  parsnip::set_mode('classification') %>%
  parsnip::set_engine('ranger')
spec

wf <-
  workflows::workflow() %>%
  workflows::add_recipe(rec) %>%
  workflows::add_model(spec)
# debugonce(parsnip::fit)
# debugonce(workflows:::.fit_model)
# debugonce(workflows:::fit_from_xy)
fit <- parsnip::fit(wf, trn)

spec <-
  parsnip::rand_forest(
    trees = 1000,
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
