
library(tidyverse)
positions <- import_positions()
plays <- import_plays(drop_bad = TRUE)
features <- arrow::read_parquet(file.path('inst', 'features.parquet'))

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
  mutate(across(idx_o_target, ~coalesce(.x, 101) %>% factor()))
features_wide %>% count(idx_o_target)

set.seed(42)
splits <- features_wide %>% rsample::initial_split(strata = col_y)
trn <- splits %>% rsample::training()
tst <- splits %>% rsample::testing()
folds <- trn %>% rsample::vfold_cv(strata = col_y)
fmla <- paste0(col_y, ' ~ .') %>% as.formula()

rec <-
  trn %>%
  recipes::recipe(fmla, data = .) %>%
  recipes::update_role(
    dplyr::matches('^nfl_id_'),
    dplyr::matches('_id$'),
    new_role = 'extra'
  ) %>%
  recipes::step_nzv(recipes::all_numeric(), -recipes::has_role('extra')) %>%
  # recipes::step_corr(recipes::all_numeric(), -recipes::has_role('extra')) %>%
  recipes::step_dummy(
    recipes::all_nominal(),
    -recipes::has_role('extra'),
    -recipes::all_outcomes(),
    one_hot = FALSE
  )
rec

spec <-
  parsnip::boost_tree(
    trees = 1000,
    # Model complexity
    min_n = tune::tune(),
    tree_depth = tune::tune(),
    loss_reduction = tune::tune(),
    # Randomnes
    sample_size = tune::tune(),
    mtry = tune::tune(),
    # Step size
    learn_rate = tune::tune()
  ) %>%
  parsnip::set_mode('classification') %>%
  parsnip::set_engine('xgboost')
spec

wf <-
  workflows::workflow() %>%
  workflows::add_recipe(rec) %>%
  workflows::add_model(spec)

params_grid <-
  dials::grid_latin_hypercube(
    dials::tree_depth(),
    dials::min_n(),
    dials::loss_reduction(),
    sample_size = dials::sample_prop(),
    dials::finalize(dials::mtry(), trn),
    dials::learn_rate(),
    size = 30
)
params_grid

# fit <- wf %>% parsnip::fit(trn)
# cl <- parallel::makeCluster(2)
# doParallel::registerDoParallel(cl)
# require(yardstick)
res_tune <-
  tune::tune_grid(
    wf,
    resamples = folds,
    # metric = yardstick::metric_set(yardstick::roc_auc),
    control = tune::control_grid(save_pred = TRUE, verbose = TRUE)
  )
write_rds(res_tune, file.path('inst', 'res_tune.rds'))
