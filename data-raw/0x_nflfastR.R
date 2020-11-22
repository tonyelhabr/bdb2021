
library(tidyverse)
data('pick_play_ids_adj', package = 'bdb2021')

#' @seealso \url{https://github.com/guga31bb/nflfastR-data/blob/master/models/model_data.R#L1}

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

plays <- import_plays()
# plays %>%
#   filter(!(n_k > 0L | n_p > 0L))
plays_filt <-
  plays %>%
  anti_join(
    plays %>%
      filter((n_k > 0L | n_p > 0L)) %>%
      select(game_id, play_id),
    by = c('game_id', 'play_id')
  ) %>%
  select(game_id, play_id)
# plays_filt %>% count(game_id, play_id, sort = TRUE) %>% filter(n > 1L)

pbp <- import_nflfastr_pbp()
pbp

#' @seealso \url{https://github.com/mrcaseb/nflfastR/blob/master/data-raw/MODELS.R#L25}
nflfastr_epa_model_features <-
  pbp_data %>%
  nflfastR:::make_model_mutations() %>%
  left_join(
    pick_play_ids_adj %>%
      select(week, game_id, play_id) %>%
      mutate(is_pick_play = 1L),
    by = c('game_id', 'play_id', 'week')
  ) %>%
  semi_join(
    plays_filt,
    by = c('game_id', 'play_id')
  ) %>%
  inner_join(
    pbp %>%
      select(game_id, play_id, epa),
    by = c('game_id', 'play_id')
  ) %>%
  mutate(
    label = case_when(
      Next_Score_Half == 'Touchdown' ~ 0,
      Next_Score_Half == 'Opp_Touchdown' ~ 1,
      Next_Score_Half == 'Field_Goal' ~ 2,
      Next_Score_Half == 'Opp_Field_Goal' ~ 3,
      Next_Score_Half == 'Safety' ~ 4,
      Next_Score_Half == 'Opp_Safety' ~ 5,
      Next_Score_Half == 'No_Score' ~ 6
    ),
    label = as.factor(label),
    label = as.numeric(label),
    label = label - 1,
    across(is_pick_play, ~coalesce(.x, 0L)),
    # Calculate the drive difference between the next score drive and the
    # current play drive:
    Drive_Score_Dist = Drive_Score_Half - drive,
    # Create a weight column based on difference in drives between play and next score:
    Drive_Score_Dist_W = (max(Drive_Score_Dist) - Drive_Score_Dist) /
      (max(Drive_Score_Dist) - min(Drive_Score_Dist)),
    # Create a weight column based on score differential:
    ScoreDiff_W = (max(abs(score_differential), na.rm = T) - abs(score_differential)) /
      (max(abs(score_differential), na.rm = T) - min(abs(score_differential), na.rm = T)),
    # Add these weights together and scale again:
    Total_W = Drive_Score_Dist_W + ScoreDiff_W,
    Total_W_Scaled = (Total_W - min(Total_W, na.rm = T)) /
      (max(Total_W, na.rm = T) - min(Total_W, na.rm = T))
  ) %>%
  filter(
    !is.na(defteam_timeouts_remaining),
    !is.na(posteam_timeouts_remaining),
    !is.na(yardline_100)
  ) %>%
  select(
    game_id,
    play_id,
    epa,
    label,
    is_pick_play,
    half_seconds_remaining,
    yardline_100,
    home,
    retractable,
    dome,
    outdoors,
    ydstogo,
    era0,
    era1,
    era2,
    era3,
    era4,
    down1,
    down2,
    down3,
    down4,
    posteam_timeouts_remaining,
    defteam_timeouts_remaining,
    Total_W_Scaled
  ) %>%
  # There are some duplicates for some reason... Seems to be due to the nflfastR data.
  group_by(game_id, play_id) %>%
  filter(row_number() == 1L) %>%
  ungroup()

# usethis::use_data(nflfastr_epa_model_features, overwrite = TRUE)
nflfastr_epa_model_features
# nflfastr_epa_model_features %>% count(game_id, play_id) %>% filter(n > 1L) %>% head(1) %>% inner_join(nflfastr_epa_model_features) %>% glimpse()

xgb_wt_col <- 'Total_W_Scaled'
y_col <- 'label'
y_col_propensity <- 'is_pick_play'
extra_cols <- c('game_id', 'play_id', 'epa')
extra_cols_propensity <- c('game_id', 'play_id', 'epa', 'label')
nonfit_cols <- c(y_col, extra_cols, xgb_wt_col)
nonfit_cols_propensity <- c(y_col_propensity, extra_cols_propensity, xgb_wt_col)

# epa params
nrounds <- 525
params <-
  list(
    booster = 'gbtree',
    objective = 'multi:softprob',
    eval_metric = c('mlogloss'),
    num_class = 7,
    eta = 0.025,
    gamma = 1,
    subsample = 0.8,
    colsample_bytree = 0.8,
    max_depth = 5,
    min_child_weight = 1,
    base_score = mean(nflfastr_epa_model_features[[y_col]])
  )

nrounds_propensity <- 525
params_propensity <-
  list(
    booster = 'gbtree',
    objective = 'binary:logistic',
    eval_metric = c('logloss'),
    # num_class = 7,
    eta = 0.025,
    gamma = 1,
    subsample = 0.8,
    colsample_bytree = 0.8,
    max_depth = 5,
    min_child_weight = 1,
    base_score = mean(nflfastr_epa_model_features[[y_col_propensity]])
  )

# cp params
# nrounds <- 560
# params <-
#   list(
#     booster = 'gbtree',
#     objective = 'binary:logistic',
#     eval_metric = c('logloss'),
#     eta = 0.025,
#     gamma = 5,
#     subsample = 0.8,
#     colsample_bytree = 0.8,
#     max_depth = 4,
#     min_child_weight = 6,
#     base_score = mean(nflfastr_epa_model_features$label)
#   )

# nflfastr_epa_model_features <-
#   nflfastr_epa_model_features %>%
#   mutate(
#     label = as.numeric(label),
#     label = label - 1
#   )

full_train <-
  model.matrix(
    ~ . + 0,
    data =
      nflfastr_epa_model_features %>%
      # select(-.data[[y_col_propensity]], -.data[[xgb_wt_col]])
      select(-one_of(nonfit_cols))
  ) %>%
  xgboost::xgb.DMatrix(
    label = nflfastr_epa_model_features[[y_col]],
    weight = nflfastr_epa_model_features[[xgb_wt_col]]
  )

full_train_propensity <-
  model.matrix(
    ~ . + 0,
    data =
      nflfastr_epa_model_features %>%
      # select(-.data[[y_col_propensity]], -.data[[xgb_wt_col]])
      select(-one_of(nonfit_cols_propensity))
  ) %>%
  xgboost::xgb.DMatrix(
    label = nflfastr_epa_model_features[[y_col_propensity]],
    weight = nflfastr_epa_model_features[[xgb_wt_col]]
  )

fit <-
  xgboost::xgboost(
    params = params,
    data = full_train,
    nrounds = nrounds,
    verbose = 2
  )

propensity_fit <-
  xgboost::xgboost(
    params = params,
    data = full_train_propensity,
    nrounds = nrounds,
    verbose = 2
  )

tidy.xgb.Booster <- function(x, ...){
  data.frame(x$evaluation_log[x$best_iteration])
}
fit %>% tidy.xgb.Booster()

calcualte_expected_points <- function() {

  drop.cols <- c(
    "ep", "td_prob", "opp_td_prob", "fg_prob", "opp_fg_prob",
    "safety_prob", "opp_safety_prob", "no_score_prob"
  )

    model_data <-
      pbp_data %>%
      # drop existing values of ep and the probs before making new ones
      dplyr::select(-any_of(drop.cols)) %>%
      nflfastR:::make_model_mutations() %>%
      # nflfastR:::ep_model_select() %>%
      left_join(
        pick_play_ids_adj %>%
          select(week, game_id, play_id) %>%
          mutate(is_pick_play = 1L),
        by = c('game_id', 'play_id', 'week')
      ) %>%
      semi_join(
        plays_filt,
        by = c('game_id', 'play_id')
      ) %>%
      inner_join(
        pbp %>%
          select(game_id, play_id, epa),
        by = c('game_id', 'play_id')
      ) %>%
      select(
        game_id,
        play_id,
        epa,
        # label,
        is_pick_play,
        half_seconds_remaining,
        yardline_100,
        home,
        retractable,
        dome,
        outdoors,
        ydstogo,
        era0,
        era1,
        era2,
        era3,
        era4,
        down1,
        down2,
        down3,
        down4,
        posteam_timeouts_remaining,
        defteam_timeouts_remaining
      ) %>%
      # There are some duplicates for some reason... Seems to be due to the nflfastR data.
      group_by(game_id, play_id) %>%
      filter(row_number() == 1L) %>%
      ungroup()

    pred_data_mat <-
      model_data %>%
      select(-game_id, -play_id, -epa) %>%
      as.matrix()
  preds <- as.data.frame(
    matrix(stats::predict(fit, pred_data_mat), ncol = 7, byrow = TRUE)
    # matrix(stats::predict(nflfastR:::ep_model, as.matrix(model_data)), ncol = 7, byrow = TRUE)
  )

  colnames(preds) <- c(
    "td_prob", "opp_td_prob", "fg_prob", "opp_fg_prob",
    "safety_prob", "opp_safety_prob", "no_score_prob"
  )

  res <-
    preds %>%
    dplyr::mutate(
      ep =
        (-3 * .data$opp_fg_prob) +
        (-2 * .data$opp_safety_prob) +
        (-7 * .data$opp_td_prob) +
        (3 * .data$fg_prob) +
        (2 * .data$safety_prob) +
        (7 * .data$td_prob)
    ) %>%
    dplyr::bind_cols(model_data)
  res %>%
    as_tibble()
}

shaps <- fit %>% vip::vi_shap(train = full_train)
shaps
# fit <- glm(formula(vs ~ .), data = mtcars %>% select(vs, mpg), family = binomial())
# fit
# fit %>%
#   broom::augment(type.predict = 'response', data = mtcars) %>%
#   mutate(wts = 1 / ifelse(vs == 0, 1 - .fitted, .fitted)) %>%
#   relocate(.fitted, wts) %>%
#   arrange(-.fitted)

nflfastr_epa_model_features_w_propensity <-
  propensity_fit %>%
  stats::predict(full_train, type = 'response') %>%
  tibble(.fitted = .) %>%
  bind_cols(nflfastr_epa_model_features) %>%
  # broom::tidy()
  # broom::augment(type.predict = 'response', data = full_train) %>%
  # Compute inverse probability weights
  mutate(wts = 1 / ifelse(.data[[y_col_propensity]] == 0, 1 - .fitted, .fitted)) %>%
  relocate(wts)
nflfastr_epa_model_features_w_propensity

nflfastr_epa_model_features_w_propensity %>%
  ggplot() +
  aes(x = wts) %>%
  geom_density(size = .8) +
  scale_x_log10() +
  theme_minimal(base_size = 20) +
  xlab('Weights')

svy_des <-
  survey::svydesign(
    ids = ~ 1,
    data = nflfastr_epa_model_features_w_propensity,
    weights = ~ wts
  )

propensity_col_propensitys <- c('.fitted', 'wts')
feature_cols <-
  nflfastr_epa_model_features_w_propensity %>%
  names() %>%
  setdiff(c(nonfit_cols_propensity, propensity_col_propensitys))
feature_cols

smd_table_unweighted <-
  tableone::CreateTableOne(
    vars = feature_cols,
    strata = y_col_propensity,
    data = nflfastr_epa_model_features_w_propensity,
    test = FALSE
  )
smd_table_unweighted

smd_table <-
  tableone::svyCreateTableOne(
    vars = feature_cols,
    strata = y_col_propensity,
    data = svy_des,
    test = FALSE
  )
smd_table

plot_df <-
  tibble(
    var = rownames(tableone::ExtractSmd(smd_table)),
    Unadjusted = as.numeric(tableone::ExtractSmd(smd_table_unweighted)),
    Weighted = as.numeric(tableone::ExtractSmd(smd_table))
  ) %>%
  pivot_longer(-var, names_to = 'Method', values_to = 'SMD')
plot_df

ggplot(
  data = plot_df,
  mapping = aes(x = var, y = SMD, group = Method, color = Method)
) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0.1, color = "black", size = 0.1) +
  coord_flip() +
  theme_minimal() +
  scale_color_manual(values = c("grey85", "#00BFC4")) +
  ylim(0, .3)

# mgcv::bam
ipw_fit <- lm(
  formula(epa ~ label),
  data = nflfastr_epa_model_features_w_propensity,
  weights = wts
)
ipw_fit

ipw_estimate <-
  ipw_fit %>%
  broom::tidy(conf.int = TRUE) %>%
  filter(term == !!y_col_propensity)
ipw_estimate
