

set.seed(42)
split <- 
  plays_w_pick_info %>% 
  filter(!is.na(absolute_yardline_number)) %>% 
  rsample::initial_split(strata = all_of(col_y))
# trn <- split %>% rsample::training()
# tst <- split %>% rsample::testing()
trn <- plays_w_pick_info
fmla <- fmla_pick_play_prob
if(FALSE) {
  rec <-
    trn %>%
    recipes::recipe(fmla, data = .) %>% 
    recipes::step_dummy(quarter, down, matches('_fct$'), one_hot = TRUE)
  rec
  rec %>% recipes::prep() %>% recipes::juice()
  
  spec <-
    parsnip::rand_forest(
      trees = 250,
      mtry  = 6,
      min_n = 30
    ) %>%
    parsnip::set_mode('classification') %>%
    parsnip::set_engine('ranger', importance = 'permutation')
  spec
  
  wf <-
    workflows::workflow() %>%
    workflows::add_recipe(rec) %>%
    workflows::add_model(spec)
  
  fit <- parsnip::fit(wf, trn)
  
  fit %>% 
    vip::vip(
      num_features = 20
    )
  
  spec <-
    parsnip::rand_forest(
      trees = tune::tune(),
      mtry  = tune::tune(),
      min_n = tune::tune()
    ) %>%
    parsnip::set_mode('classification') %>%
    parsnip::set_engine('ranger')
  spec
  
  folds <- trn %>% rsample::vfold_cv(strata = all_of(col_y))
  
  grid_params <-
    # dials::grid_regular(
    dials::grid_max_entropy(
      # dials::mtry(range = c(3, 10)),
      dials::finalize(dials::trees(), trn),
      dials::finalize(dials::mtry(), trn),
      dials::finalize(dials::min_n(), trn),
      # dials::min_n(range = c(2, 50)),
      size = 10
    )
  grid_params
  
  require(yardstick)
  res_grid <-
    tune::tune_grid(
      spec,
      fmla,
      resamples = folds,
      control = tune::control_grid(verbose = TRUE),
      grid = grid_params,
    )
  res_grid
  pacman::p_unload('yardstick')
  beepr::beep(3)
  
  res_grid %>% autoplot()
  metrics <- res_grid %>% tune::collect_metrics()
  metrics
  metrics %>% 
    arrange(.metric, -mean) %>% 
    group_by(.metric) %>% 
    slice(c(1:5)) %>% 
    ungroup()
  
  res_grid %>% tune::select_best('roc_auc')
  res_grid %>% tune::select_best('accuracy')
  fit_best <- res_grid %>% tune::select_best('roc_auc')
  fit_best
  
  spec_final <- 
    tune::finalize_model(spec, fit_best) %>% 
    parsnip::set_engine('ranger', importance = 'permutation')
  spec_final
  
  fit_final <- spec_final %>% parsnip::fit(formula = fmla, data = trn)
  fit_final
  
  fit_final %>% 
    # workflows::pull_workflow_fit() %>%
    vip::vi_shap(
      feature_names = 
      num_features = 20
    )
  
  fit_final %>% 
    # workflows::pull_workflow_fit() %>%
    vip::vi_model(num_features = 20) %>% 
    mutate(
      Importance = abs(Importance),
      Variable = fct_reorder(Variable, Importance)
    ) %>%
    ggplot(aes(x = Importance, y = Variable)) +
    geom_col() +
    scale_x_continuous(expand = c(0, 0)) +
    labs(y = NULL)
}

spec_final <-
  parsnip::rand_forest(
    trees = 500,
    mtry  = 3,
    min_n = 46
  ) %>%
  parsnip::set_mode('classification') %>%
  parsnip::set_engine('ranger', importance = 'permutation')
spec_final

fit_final <- spec_final %>% parsnip::fit(formula = fmla, data = trn)
fit_final

fit_final$fit$call

trn_jui <- rec_pick_play_prob %>% recipes::prep() %>% recipes::bake(new_data = trn)
x_trn_jui <- trn_jui[, setdiff(names(trn_jui), col_y)]
# x_trn_jui %>% as_tibble() %>% skimr::skim()
y_trn_jui <- trn_jui[[col_y]] %>% as.character() %>% as.integer()
# y_trn_jui <- as.integer(y_trn_jui)]
fit_ranger <-
  ranger::ranger(
    x = x_trn_jui,
    y = y_trn_jui,
    mtry = 3,
    num.trees = 500,
    min.node.size = 46,
    importance = "permutation",
    num.threads = 1,
    verbose = FALSE,
    seed = sample.int(10 ^ 5, 1),
    probability = TRUE
  )
fit_ranger

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
    object = fit_final, 
    num_features = x_trn_jui %>% ncol(), 
    ... = 
  )

.f_predict <- function(object, newdata) predict(object, data = newdata)$predictions
vip_wrapper_partial_shap <-
  partial(
    vip_wrapper_partial, 
    method = 'shap',
    train = x_trn_jui,
    pred_wrapper = .f_predict,
    ... = 
  )


vi_shap <-
  vip::vi_shap(
    # object = fit_final$fit,
    object = fit_ranger,
    # train = x_trn_jui,
    train = x_trn_jui %>% mutate(across(matches('.*'), as.numeric)) %>% drop_na(),
    pred_wrapper = .f_predict
  )

pfun <- function(object, newdata) {
  # predict(object, as.matrix(newdata))$predictions
  predict(object, newdata)$predictions
}
debugonce(fastshap:::explain_column)
res_fastshap <- fastshap::explain(rfo, X = X, nsim = 1, pred_wrapper = pfun)
autoplot(res_fastshap)

debugonce(fastshap:::explain_column)
# debugonce(fastshap:::genFrankensteinMatrices)
expl_fastshap <-
  fastshap::explain(
    object = fit_ranger,
    # X = trn_jui[, setdiff(names(trn_jui), col_y)],
    X = x_trn_jui %>% mutate_if(is.factor, as.integer) %>% as.matrix() %>% head(600),
    nsim = 100,
    adjust = TRUE,
    # features_names = cols_features,
    # X = x_trn_jui, #  %>% as_tibble() %>% mutate_all(as.double)
    pred_wrapper = pfun
  )
expl_fastshap
autoplot(expl_fastshap)

# Need to remove the non-diamonds_modified class in order to use {dplyr} functions.
class(expl_fastshap) <- c('tbl_df', 'tbl', 'data.frame')

vi_fastshap <-
  expl_fastshap %>%
  summarize_all(~mean(abs(.))) %>%
  # This is actually already `imp_abs`, but it won't matter in the end.
  pivot_longer(matches('.'), names_to = 'var', values_to = 'imp')

fit_final %>% 
  vip::vip(method = 'shap', train = trn, pred_wrapper = function(x,y) { mean(x == y) })


expl_dalex <- 
  DALEX::explain(
    fit_final$fit, # $fit, 
    # data = x_trn_jui %>% as.data.frame(),
    data = trn_jui,
    y = y_trn_jui, 
    verbose = FALSE
  )

set.seed(42)
vi_dalex_init <- 
  expl_dalex %>% 
  DALEX::variable_importance(
    type = 'difference',
    loss_function = DALEX::loss_accuracy, 
    n_sample = NULL
  )
vi_dalex_init

# Regarding why `permutation == 0`, see `ingredients:::feature_importance.default()`, which is called by `ingredients:::feature_importance.explainer()`, which is called by `DALEX::variable_importance`
# Specifically, this line: `res <- data.frame(variable = c("_full_model_", names(res),  "_baseline_"), permutation = 0, dropout_loss = c(res_full, res, res_baseline), label = label, row.names = NULL)`
vi_dalex <-
  vi_dalex_init %>% 
  as_tibble() %>% 
  filter(permutation == 0) %>% 
  mutate(
    imp = (dropout_loss) / max(abs(dropout_loss))
  ) %>% 
  select(var = variable, imp) %>%
  filter(!(var %in% c('_baseline_', '_full_model_'))) %>% 
  arrange(desc(imp))
vi_dalex
plot(vi_dalex_init)

y_line <-
  y_trn_jui %>%
  tibble(y = .) %>% 
  count(y) %>% 
  mutate(frac = n / sum(n)) %>% 
  filter(y == 1L) %>%
  pull(frac)

prof <-
  expl_dalex %>% 
  DALEX::model_profile()
prof %>% plot()
expl_dalex %>% 
  DALEX::variable_effect_partial_dependency(variables = cols_d) %>% 
  plot() +
  geom_hline(aes(yintercept = !!y_line)) +
  coord_cartesian(ylim = c(0, 0.4))
