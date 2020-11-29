
.path <- function(what = c('acc', 'fit', 'probs'), prefix2 = c('nw', 'nw_wo_qb', 'w_a_o'), min_n, mtry, trees = 500, strict = TRUE) {
  what <- match.arg(what)
  # prefix2 <- match.arg(prefix2)
  ext <- switch(what, 'acc' = 'csv', 'fit' = 'rds', 'probs' = 'parquet')
  path <- file.path('inst', sprintf('%s-%s-min_n=%d-mtry=%d-trees=%d.%s', what, prefix2, min_n, mtry, trees, ext))
  if(!strict) {
    return(path)
  }
  assertthat::assert_that(file.exists(path))
  path
}

fit_params <- list(prefix = 'nw_wo_qb_event_w_d1_s', min_n = 2, mtry = 38)
.path_best <- partial(.path, prefix2 = fit_params$prefix, min_n = fit_params$min_n, mtry = fit_params$mtry, ... = )
fit <- .path_best(what = 'fit') %>% read_rds()
fit_wf <- fit %>% workflows::pull_workflow_fit()
probs <- .path_best(what = 'probs') %>% arrow::read_parquet()
probs

cols_features_wide <- fit_wf$fit$forest$independent.variable.names
cols_features_wide %>% length()
cols_features_wide %>% sort() %>% str_remove_all('_[1-5]$') %>% unique()

# ...
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
    # Will get a "Sign" column when using the default `method = 'model'`.
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

# set.seed(42)
# vip_wrapper_partial_shap <-
#   partial(
#     vip_wrapper_partial,
#     method = 'shap',
#     train = x_trn_jui,
#     ... =
#   )

# vi_vip_model <- vip_wrapper_partial(method = 'model')
vi_vip_permute <-
  vip_wrapper_partial_permute(
    train = x_trn_jui %>% as.data.frame(),
    target = y_trn_jui
  )
vi_vip_permute
vi_vip_permute %>% arrange(-abs(imp))
# vi_vip_shap <- vip_wrapper_partial_shap(pred_wrapper = f_predict)
# vi_vip_shap

