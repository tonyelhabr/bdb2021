
library(tidyverse)
library(bdb2021)
data('plays_w_pick_info', package = 'bdb2021')

plays <- import_plays()
pbp <- import_nflfastr_pbp()
path_model_data <- file.path(get_bdb_dir_data(), 'nflfastr_model_data_2018.parquet')
path_epa_model_bdb <- file.path('inst', 'epa_model_bdb')

# Reference https://github.com/guga31bb/nflfastR-data/blob/master/models/model_data.R#L1}
retrieve_nflfastr_model_data <- function(overwrite = FALSE) {
  if(!file.exists(path_model_data) & !overwrite) {
    .display_info('Returning early.')
    model_data <- path_model_data %>% arrow::read_parquet()
    return(model_data)
  }
  
  find_game_next_score_half <- function(pbp_dataset) {
    
    # Which rows are the scoring plays:
    score_plays <- which(pbp_dataset$sp == 1 & pbp_dataset$play_type != 'no_play')
    
    # Define a helper function that takes in the current play index,
    # a vector of the scoring play indices, play-by-play data,
    # and returns the score type and drive number for the next score:
    find_next_score <- function(play_i, score_plays_i, pbp_df) {
      
      # Find the next score index for the current play
      # based on being the first next score index:
      next_score_i <- score_plays_i[which(score_plays_i >= play_i)[1]]
      
      # If next_score_i is NA (no more scores after current play)
      # or if the next score is in another half,
      # then return No_Score and the current drive number
      if (is.na(next_score_i) |
          (pbp_df$qtr[play_i] %in% c(1, 2) & pbp_df$qtr[next_score_i] %in% c(3, 4, 5)) |
          (pbp_df$qtr[play_i] %in% c(3, 4) & pbp_df$qtr[next_score_i] == 5)) {
        score_type <- 'No_Score'
        
        # Make it the current play index
        score_drive <- pbp_df$drive[play_i]
        
        # Else return the observed next score type and drive number:
      } else {
        
        # Store the score_drive number
        score_drive <- pbp_df$drive[next_score_i]
        
        # Then check the play types to decide what to return
        # based on several types of cases for the next score:
        
        # 1: Return TD
        if (pbp_df$touchdown[next_score_i] == 1 & (pbp_df$td_team[next_score_i] != pbp_df$posteam[next_score_i])) {
          
          # For return touchdowns the current posteam would not have
          # possession at the time of return, so it's flipped:
          if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {
            score_type <- 'Opp_Touchdown'
          } else {
            score_type <- 'Touchdown'
          }
        } else if (identical(pbp_df$field_goal_result[next_score_i], 'made')) {
          
          # 2: Field Goal
          # Current posteam made FG
          if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {
            score_type <- 'Field_Goal'
            
            # Opponent made FG
          } else {
            score_type <- 'Opp_Field_Goal'
          }
          
          # 3: Touchdown (returns already counted for)
        } else if (pbp_df$touchdown[next_score_i] == 1) {
          
          # Current posteam TD
          if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {
            score_type <- 'Touchdown'
            
            # Opponent TD
          } else {
            score_type <- 'Opp_Touchdown'
          }
          # 4: Safety (similar to returns)
        } else if (pbp_df$safety[next_score_i] == 1) {
          if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {
            score_type <- 'Opp_Safety'
          } else {
            score_type <- 'Safety'
          }
          # 5: Extra Points
        } else if (identical(pbp_df$extra_point_result[next_score_i], 'good')) {
          
          # Current posteam Extra Point
          if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {
            score_type <- 'Extra_Point'
            
            # Opponent Extra Point
          } else {
            score_type <- 'Opp_Extra_Point'
          }
          # 6: Two Point Conversions
        } else if (identical(pbp_df$two_point_conv_result[next_score_i], 'success')) {
          
          # Current posteam Two Point Conversion
          if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {
            score_type <- 'Two_Point_Conversion'
            
            # Opponent Two Point Conversion
          } else {
            score_type <- 'Opp_Two_Point_Conversion'
          }
          
          # 7: Defensive Two Point (like returns)
        } else if (identical(pbp_df$defensive_two_point_conv[next_score_i], 1)) {
          if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {
            score_type <- 'Opp_Defensive_Two_Point'
          } else {
            score_type <- 'Defensive_Two_Point'
          }
          
          # 8: Errors of some sort so return NA (but shouldn't take place)
        } else {
          score_type <- NA
        }
      }
      
      return(data.frame(
        Next_Score_Half = score_type,
        Drive_Score_Half = score_drive
      ))
    }
    
    # Using lapply and then bind_rows is much faster than
    # using map_dfr() here:
    lapply(
      c(1:nrow(pbp_dataset)),
      find_next_score,
      score_plays_i = score_plays,
      pbp_df = pbp_dataset
    ) %>%
      bind_rows() %>%
      return()
  }
  
  pbp_data <- purrr::map_df(2018, function(x) {
    readRDS(
      
      # from repo
      url(glue::glue('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds'))
      
      # local
      # glue::glue('data/play_by_play_{x}.rds')
    ) %>% filter(season_type == 'REG')
  }) %>%
    mutate(
      Winner = if_else(
        home_score > away_score,
        home_team,
        if_else(home_score < away_score, away_team, 'TIE')
      )
    )
  
  # get next score half using the provided function
  pbp_next_score_half <- map_dfr(
    unique(pbp_data$game_id),
    function(x) {
      pbp_data %>%
        filter(game_id == x) %>%
        find_game_next_score_half()
    }
  )
  
  # bind to original df
  pbp_data <- bind_cols(pbp_data, pbp_next_score_half)
  
  # for estimating the models, apply some filters
  pbp_data <- pbp_data %>%
    filter(Next_Score_Half %in% c(
      'Opp_Field_Goal',
      'Opp_Safety',
      'Opp_Touchdown',
      'Field_Goal',
      'No_Score',
      'Safety',
      'Touchdown'
    ) &
      play_type %in% c(
        'field_goal',
        'no_play',
        'pass',
        'punt',
        'run',
        'qb_spike'
      ) & is.na(two_point_conv_result) & is.na(extra_point_result) &
      !is.na(down) & !is.na(game_seconds_remaining)) %>%
    # to keep file size manageable
    select(
      game_id = old_game_id,
      play_id,
      Next_Score_Half,
      Drive_Score_Half,
      play_type,
      game_seconds_remaining,
      half_seconds_remaining,
      yardline_100,
      roof,
      posteam,
      defteam,
      home_team,
      ydstogo,
      season,
      qtr,
      down,
      week,
      drive,
      ep,
      score_differential,
      posteam_timeouts_remaining,
      defteam_timeouts_remaining,
      desc,
      receiver_player_name,
      pass_location,
      air_yards,
      yards_after_catch,
      complete_pass,
      incomplete_pass,
      interception,
      qb_hit,
      extra_point_result,
      field_goal_result,
      sp,
      Winner,
      spread_line,
      total_line
    ) %>%
    mutate(
      across(c(game_id, play_id), as.integer)
    )

  model_data <- 
    pbp_data %>%
    nflfastR:::make_model_mutations() %>%
    inner_join(
      plays %>%
        select(game_id, play_id, epa_bdb = epa),
      by = c('game_id', 'play_id')
    ) %>% 
    inner_join(
      pbp %>%
        select(game_id, play_id, epa_nflfastr = epa),
      by = c('game_id', 'play_id')
    ) %>% 
    mutate(
      label = case_when(
        Next_Score_Half == "Touchdown" ~ 0,
        Next_Score_Half == "Opp_Touchdown" ~ 1,
        Next_Score_Half == "Field_Goal" ~ 2,
        Next_Score_Half == "Opp_Field_Goal" ~ 3,
        Next_Score_Half == "Safety" ~ 4,
        Next_Score_Half == "Opp_Safety" ~ 5,
        Next_Score_Half == "No_Score" ~ 6
      ),
      label = as.factor(label),
      
      # tony added this
      label = as.numeric(label),
      label = label - 1,
      
      # Calculate the drive difference between the next score drive and the
      # current play drive:
      Drive_Score_Dist = Drive_Score_Half - drive,
      # Create a weight column based on difference in drives between play and next score:
      Drive_Score_Dist_W = (max(Drive_Score_Dist) - Drive_Score_Dist) /
        (max(Drive_Score_Dist) - min(Drive_Score_Dist)),
      # Create a weight column based on score differential:
      ScoreDiff_W = (max(abs(score_differential), na.rm=T) - abs(score_differential)) /
        (max(abs(score_differential), na.rm=T) - min(abs(score_differential), na.rm=T)),
      # Add these weights together and scale again:
      Total_W = Drive_Score_Dist_W + ScoreDiff_W,
      Total_W_Scaled = (Total_W - min(Total_W, na.rm=T)) /
        (max(Total_W, na.rm=T) - min(Total_W, na.rm=T))
    ) %>%
    filter(
      !is.na(defteam_timeouts_remaining), !is.na(posteam_timeouts_remaining),
      !is.na(yardline_100)
    ) %>%
    select(
      # added these
      game_id,
      play_id,
      epa_nflfastr,
      
      label,
      half_seconds = half_seconds_remaining,
      yardline_100,
      is_home = home,
      roofretractable = retractable,
      roofdome = dome,
      roofoutdoors = outdoors,
      yards_to_go = ydstogo,
      # era0, era1, era2, era3, era4, # removed these
      down1, down2, down3, down4,
      off_timeouts = posteam_timeouts_remaining,
      def_timeouts = defteam_timeouts_remaining,
      Total_W_Scaled
    ) %>%
    # There are some duplicates for some reason... Seems to be due to the nflfastR data.
    group_by(game_id, play_id) %>%
    filter(row_number() == 1L) %>%
    ungroup()
  arrow::write_parquet(model_data, path_model_data)
  model_data
}

.ep_model_select <- function(pbp, ...) {
  pbp %>% 
    dplyr::select(
      ...,
      half_seconds,
      yardline_100,
      is_home,
      roofretractable,
      roofdome,
      roofoutdoors,
      yards_to_go,
      down1,
      down2,
      down3,
      down4,
      off_timeouts,
      def_timeouts
    )
}

.cols_control <- c('x', 'y', 'x_o', 'dist_o', 'x_d', 'dist_d')
# These are actually the dummy variables, so can't use these directly with the select below.
.cols_trt <- 
  c(
    sprintf('is_target_picked%d', 0:1), 
    sprintf('has_same_defender%d', 0:1)
  )

model_data_nflfastr <-
  model_data %>% 
  inner_join(
    plays_w_pick_info %>%
      select(
        game_id,
        play_id,
        x,
        y,
        x_o,
        dist_o,
        x_d,
        dist_d,
        is_target_picked,
        has_same_defender,
        epa_bdb = epa
      )
  )

.bdb_model_select <- function(data, ...) {
  data %>% 
    .ep_model_select(
      x, y, x_o, dist_o, x_d, dist_d, 
      is_target_picked, has_same_defender
    ) %>% 
    mutate(
      across(c(is_target_picked, has_same_defender), binary_fct_to_int),
      is_target_picked1 = if_else(is_target_picked == 1, 1, 0), 
      is_target_picked0 = if_else(is_target_picked == 0, 1, 0),
      has_same_defender1 = if_else(has_same_defender == 1, 1, 0), 
      has_same_defender0 = if_else(has_same_defender == 0, 1, 0)
    ) %>% 
    select(-c(is_target_picked, has_same_defender))
}

model_mat_nflfastr <- 
  model.matrix(
    ~.+0, 
    data = model_data_nflfastr %>% .bdb_model_select()
  )
model_mat_nflfastr
usethis::use_data(model_mat_nflfastr, overwrite = TRUE)

if(!file.exists(path_epa_model_bdb)) {

  set.seed(42)
  folds <- caret::createFolds(model_data_nflfastr$epa_bdb, k = 10, list = TRUE, returnTrain = FALSE)
  names(folds) <- NULL
  
  n_row <- 20
  grid <- 
    dials::grid_latin_hypercube(
      dials::finalize(dials::mtry(), model_data_nflfastr),
      dials::min_n(),
      dials::tree_depth(),
      dials::learn_rate(),
      dials::loss_reduction(),
      sample_size = dials::sample_prop(),
      size = n_row
    ) %>% 
    mutate(
      learn_rate = .1 * ((1:n_row) / n_row),
      mtry = mtry / length(model_data_nflfastr)
    )
  grid
  
  full_train_bdb_epa <-
    xgboost::xgb.DMatrix(
      model_mat_nflfastr,
      label = model_data_nflfastr$epa_bdb,
      weight = model_data_nflfastr$Total_W_Scaled
    )
  
  # function to search over hyperparameter grid
  .get_metrics <- function(df, row = 1) {
    
    params <-
      list(
        booster = 'gbtree',
        objective = 'reg:squarederror',
        eval_metric = c('rmse'),
        eta = df$learn_rate,
        gamma = df$loss_reduction,
        subsample= df$sample_size,
        colsample_bytree= df$mtry,
        max_depth = df$tree_depth,
        min_child_weight = df$min_n
      )

    epa_cv_model <-
      xgboost::xgb.cv(
        data = full_train_bdb_epa, params = params, nrounds = nrounds,
        folds = folds, metrics = list('rmse'),
        early_stopping_rounds = 10, print_every_n = 10
      )
    
    output <- params
    output$iter = epa_cv_model$best_iteration
    output$rmse = epa_cv_model$evaluation_log[output$iter]$test_rmse_mean
    output$error = epa_cv_model$evaluation_log[output$iter]$test_error_mean
    
    this_param <- bind_rows(output)
    
    path <- .path_data('epa_cv_model_bdb.rds')
    if (row == 1) {
      write_rds(this_param, path)
    } else {
      prev <- path %>% read_rds()
      for_save <- bind_rows(prev, this_param)
      write_rds(for_save, path)
    }
    
    this_param
  }
  
  results <- map_df(1:nrow(grid), function(x) {
    
    cat(glue::glue('Row {x}'), sep = '\n')
    .get_metrics(grid %>% slice(x), row = x)
    
  })
  
  best_result <- results %>% slice_min(rmse)
  
  results %>%
    select(rmse, eta, gamma, subsample, colsample_bytree, max_depth, min_child_weight) %>%
    pivot_longer(
      -rmse,
      values_to = 'value',
      names_to = 'parameter'
    ) %>%
    ggplot() +
    aes(x = value, y = rmse, color = parameter) +
    geom_point(show.legend = FALSE) +
    facet_wrap(~parameter, scales = 'free_x') +
    labs(x = NULL, y = 'logloss') +
    theme_minimal()
}
# based on the hyperparameter tuning
nrounds_bdb_epa <- 522
# best_result %>% as.list()
params_bdb_epa <-
  list(
    booster = 'gbtree',
    objective = 'reg:squarederror',
    eval_metric = 'rmse',
    eta = 0.005,
    gamma = 0.6304749,
    subsample = 0.5729378,
    colsample_bytree = 0.8518519,
    max_depth = 6,
    min_child_weight = 11
  )

epa_model_bdb <- 
  xgboost::xgboost(
    params = params_bdb_epa, 
    data = full_train_bdb_epa, 
    nrounds = nrounds_bdb_epa, 
    verbose = 2
  )
xgboost::xgb.save(epa_model_bdb, path_epa_model_bdb)

