
library(tidyverse)
library(bdb2021)

features <- import_new_features()
features_model <- features %>% prep_target_prob_data(all_frames = FALSE, drop_bad = TRUE)
fmla <- formula(idx_o_target ~ .)

fit_target_prob_split_timed <- .time_it(fit_target_prob_split)

final <- TRUE
if(!final) {
  nms_wide <- features_model %>% names()
  cols_lst <- retrieve_target_prob_features()
  cols_features_wide <- setdiff(nms_wide, c(cols_lst$cols_id, cols_lst$cols_id_model, cols_lst$col_y))
  n_features <- cols_features_wide %>% length()
  
  grid_params_rf <-
    crossing(
      # min_n = c(2, 10, 25),
      min_n = 5,
      # min_n = c(25),
      # mtry = c(5, 14, 25)
      mtry = c(7) %>% {. / 8 * n_features} %>% floor()
    )
  grid_params_rf
  
  set.seed(42)
  # # Method without data leakage
  # play_ids <- features_model %>% distinct(game_id, play_id, idx_o_target)
  # split_ids <- play_ids %>% rsample::initial_split(strata = idx_o_target)
  # trn_ids <- split_ids %>% rsample::training()
  # tst_ids <- split_ids %>% rsample::testing()
  # 
  # trn <- features_model %>% semi_join(trn_ids)
  # tst <- features_model %>% semi_join(tst_ids)
  
  # Yay data leakage!
  splits <- features_model %>% rsample::initial_split(strata = idx_o_target)
  trn <- splits %>% rsample::training()
  tst <- splits %>% rsample::testing()
  
  rec <-
    recipes::recipe(fmla, data = trn) %>%
    recipes::update_role(
      dplyr::all_of(cols_lst$cols_id),
      new_role = 'extra'
    )
  rec
  
  res_grid_rf <-
    grid_params_rf %>%
    mutate(
      acc =
        map2(
          min_n,
          mtry,
          ~ fit_target_prob_split_timed(
            trn = trn,
            tst = tst,
            min_n = ..1,
            mtry = ..2,
            suffix = 'target_prob_final_new',
            overwrite = FALSE
          )
        )
    )
  
  res_grid_rf %>% unnest(acc) %>% filter(.set == 'tst')
} else {
  stem <- 'target_prob_final_new'
  set.seed(42)
  # folds <- features_model %>% rsample::vfold_cv(v = 10, strata = idx_o_target)
  
  # # If folds is based on play ids.
  play_ids <- features_model %>% distinct(game_id, play_id, idx_o_target)
  folds <- play_ids %>% rsample::vfold_cv(v = 10, strata = idx_o_target)
  
  fit_target_prob_split_fold <- function(fold, idx_fold) {
    trn <- fold %>% rsample::analysis()
    tst <- fold %>% rsample::assessment()
    
    # # If folds is based on play ids.
    trn <- features_model %>% semi_join(trn)
    tst <- features_model %>% semi_join(tst)
    
    fit_target_prob_split_timed(
      trn = trn,
      tst = tst,
      fmla = fmla,
      min_n = 25,
      mtry = 39,
      suffix = sprintf('%s_%s', stem, idx_fold)
    )
  }
  
  res_folds <-
    folds %>%
    mutate(
      across(id, ~.x %>% str_remove('Fold') %>% as.integer())
    ) %>% 
    rename(idx_fold = id) %>% 
    tail(9) %>% 
    mutate(res = map2(splits, idx_fold, fit_target_prob_split_fold))
  res_folds
  
  probs_folds <-
    fs::dir_ls(
      get_bdb_dir_data(),
      regexp = sprintf('%s.*parquet', stem)
    ) %>%
    str_subset('probs[-]target') %>% 
    tibble(path = .) %>% 
    mutate(fold = path %>% str_replace_all('(^.*)_([0-9]+)([-]min.*$)', '\\2') %>% as.integer()) %>% 
    mutate(data = map(path, arrow::read_parquet)) %>% 
    select(-path) %>% 
    unnest(data)
  probs_folds
  path_export <- file.path(get_bdb_dir_data(), sprintf('%s_min_n25_folds.parquet', stem))
  # path1 <- path_export
  # path2 <- file.path(get_bdb_dir_data(), 'probs-tp-final-folds.parquet')
  # probs_folds1 <- path1 %>% arrow::read_parquet()%>% arrange(game_id, play_id, frame_id)
  # probs_folds2 <- path2 %>% arrow::read_parquet()%>% arrange(game_id, play_id, frame_id)
  # probs_folds1
  # probs_folds2
  # probs_folds %>% filter(.set == 'tst') %>% arrange(game_id, play_id, frame_id)
  
  stopifnot(!file.exists(path_export))
  arrow::write_parquet(probs_folds, path_export)
}
