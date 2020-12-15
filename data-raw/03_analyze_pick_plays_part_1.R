
# setup ----
library(tidyverse)
library(bdb2021)
theme_set_and_update_bdb()
# data('receiver_intersections_adj', package = 'bdb2021')
data('personnel_and_rushers', package = 'bdb2021')
data('players_from_tracking', package = 'bdb2021')
data('routes', package = 'bdb2021')
players_from_tracking <- players_from_tracking %>% select(-week)
routes <- routes %>% select(-week)

# Do this NA filtering now so earlier plots don't have extra plays.
plays <- 
  import_plays() %>% 
  filter(!is.na(absolute_yardline_number)) %>% 
  mutate(is_pass_successful = if_else(pass_result == 'C', 1, 0) %>% factor())
pbp <- import_nflfastr_pbp()
.sec_cutoff <- 2
features <- import_new_features()
min_dists_naive_od_target <- import_min_dists_naive_od_target()

do_save_plot <- function(...) {
  if(TRUE) {
    save_plot(...)
  }
}

# basic eda on receiver_intersections_adj ----
if(FALSE) {
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
}

# pick_play_meta_init and plays_w_pick_info ----
if(FALSE) {
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
    is_target_picked = sum(target_nfl_id == nfl_id)
  ) %>%
  ungroup() %>%
  select(-week)
pick_play_meta_init

.f <- function(id, data) {
  if(is.null(data)) {
    return(0L)
  }
  res <- data %>% filter(nfl_id %in% id)
  ifelse(nrow(res) > 0L, 1L, 0L)
}

plays_w_pick_info <-
  plays %>%
  left_join(
    pick_play_meta_init %>%
      # Keep the pick play to one row per play at maximum
      nest(pick_data = -c(game_id, play_id)),
    by = c('game_id', 'play_id')
  ) %>%
  # relocate(pick_data) %>% 
  mutate(is_target_picked = map2_int(target_nfl_id, pick_data, ~.f(..1, ..2)) %>% factor()) %>% 
  relocate(is_target_picked) %>% 
  left_join(
    personnel_and_rushers %>%
      select(-rushers) %>%
      rename_with(~sprintf('%s_personnel', .x), matches('^n_')) %>%
      rename(n_rusher = n_rusher_personnel),
    by = c('game_id', 'play_id')
  ) %>%
  mutate(
    has_intersect = map_lgl(pick_data, ~!is.null(.x)),
    across(n_rusher, ~coalesce(.x, 0L)) # ,
    # # pick_data = map_if(pick_data, is.null, ~tibble())
  ) %>%
  relocate(game_id, play_id, has_intersect, pick_data) %>%
  left_join(
    pbp %>%
      mutate(
        across(game_half, ~case_when(.x == 'Half1' ~ 1L, TRUE ~ 2L)),
        is_home = if_else(home_team == posteam, 1, 0) %>% factor()
      ) %>% 
      select(
        game_id,
        play_id,
        game_half,
        half_seconds_remaining,
        yardline_100,
        is_home,
        roof,
        wp_nflfastr = wp,
        wpa_nflfastr = wpa,
        epa_nflfastr = epa,
        away_timeouts_remaining,
        home_timeouts_remaining
      ),
    by = c('game_id', 'play_id')
  ) %>%
  select(-pick_data) %>%
  # mutate(across(has_intersect, ~coalesce(.x, FALSE)))
  # Doing some mutations for the model fitting stuff.
  mutate(
    across(has_intersect, ~if_else(.x, 1L, 0L) %>% factor()),
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
    across(c(quarter, down, game_half), list(fct = factor))
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
  )
plays_w_pick_info
usethis::use_data(plays_w_pick_info, overwrite = TRUE)

viz_pick_play_frac <-
  plays_w_pick_info %>% 
  count(tm = possession_team, has_intersect) %>% 
  group_by(tm) %>% 
  mutate(
    frac = n / sum(n)
  ) %>% 
  ungroup() %>% 
  filter(has_intersect == 1) %>% 
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
}

# defense intersections, ending with pick_features df ----
if(FALSE) {
# Only want the ball snap and end rush frames, not the other event frames.
events_end_rush <- .get_events_end_rush()
features_min_init <-
  features %>%
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
    has_same_defender = if_else(nfl_id_d_robust == nfl_id_d_robust_init, TRUE, FALSE)
  ) %>%
  ungroup() %>%
  mutate(across(has_same_defender, ~if_else(sec == 0, NA, .x))) %>%
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
      select(game_id, play_id, is_pass_successful, pass_result, epa, nfl_id_target = target_nfl_id)
  ) %>%
  left_join(
    pbp %>%
      select(game_id, play_id, wpa_nflfastr = wpa, epa_nflfastr = epa)
  ) %>% 
  distinct() %>% 
  mutate(
    across(has_same_defender, ~.x %>% as.integer() %>% factor())
  )
features_lag

# Taking the 1 non-snap frame of each play
pick_features <-
  features_lag %>% 
  filter(sec > 0) %>% 
  # `had_intersect` is redundant with `has_intersect` if there is only one frame per play and it's the last frame.
  select(-had_intersect) %>% 
  # This is the last second measured on the play.
  select(-sec)
pick_features

plays_w_pick_def_info <-
  pick_features %>%
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
    is_target = if_else(nfl_id == nfl_id_target, 1L, 0L)
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
plays_w_pick_def_info
usethis::use_data(plays_w_pick_def_info, overwrite = TRUE)

}

# causal analysis ----
if(TRUE) {
  data('plays_w_pick_info', package = 'bdb2021')
  data('plays_w_pick_def_info', package = 'bdb2021')
  plays_w_pick_info %>% count(is_pass_successful, is_epa_neg = epa < 0)
  plays_w_pick_info_min <-
    plays_w_pick_info %>%
    select(
      
      # extra
      
      game_id,
      play_id,
      is_pass_successful,
      
      epa, # response
      is_target_picked, # treatment
      
      # stuff we already have that's redudndant with nflfastR covariates
      
      down_fct,
      yards_to_go,
      
      # nflfastR stuff
      
      game_half,
      half_seconds_remaining,
      yardline_100,
      is_home,
      roof,
      away_timeouts_remaining,
      home_timeouts_remaining
    )
  
  plays_w_pick_def_info_min <-
    plays_w_pick_def_info %>% 
    filter(nfl_id == nfl_id_target) %>% 
    select(
      game_id, play_id, has_same_defender
    )
  
  snap_frames <-
    min_dists_naive_od_target %>%
    group_by(game_id, play_id) %>%
    filter(frame_id == min(frame_id)) %>%
    ungroup() %>%
    select(game_id, play_id, frame_id)
  snap_frames
  
  min_dists_naive_od_target_filt <-
    min_dists_naive_od_target %>%
    semi_join(snap_frames)
  min_dists_naive_od_target_filt

  features_wide <-
    min_dists_naive_od_target_filt %>% 
    select(-target_nfl_id) %>% 
    inner_join(plays_w_pick_info_min) %>% 
    inner_join(plays_w_pick_def_info_min) %>% 
    mutate(
      epa_abs = epa %>% abs(),
    )
  features_wide
  arrow::write_parquet(features_wide, file.path('inst', 'features_wide.parquet'))
  
  fmla_is_target_picked_prob_prop_chr <- 
    'is_target_picked ~ game_half_fct + x_o + x_d + dist_o + dist_d + yardline_100 + pre_snap_score_diff*pre_snap_home_score + down_fct*yards_to_go'
  fmla_is_target_picked_prob_prop <- 
    fmla_is_target_picked_prob_prop_chr %>% as.formula()

  .str_replace_f <- function(x, i) {
    x %>% str_replace('(^.*)_(mean|sd)$', sprintf('\\%d', i))
  }
  
  .aggregate_to_sd_diffs <- function(data) {
    col_trt <- 'is_target_picked'
    cols_features <- c('game_half_fct', 'down_fct', 'x_o', 'x_d', 'dist_o', 'dist_d', 'yardline_100', 'pre_snap_score_diff', 'yards_to_go', 'pre_snap_home_score')
    col_trt_sym <- col_trt %>% sym()
    agg <-
      data %>% 
      # Grab the original `n_wr` instead of `n_wr_fct`.
      select(one_of(c(cols_features, col_trt))) %>% 
      mutate(
        across(where(is.double), ~if_else(.x == 9999, NA_real_, .x)),
        across(where(is.factor), as.integer),
        across(!!col_trt_sym, ~.x - 1L)
      ) %>% 
      group_by(!!col_trt_sym) %>% 
      summarise(
        across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE)
      ) %>% 
      ungroup() %>% 
      pivot_longer(
        -one_of(col_trt)
      ) %>% 
      mutate(
        across(
          name,
          list(
            col = ~.str_replace_f(.x, 1),
            stat = ~.str_replace_f(.x, 2)
          ),
          .names = '{fn}'
        )
      ) %>% 
      select(-name)
    agg
    
    res <-
      agg %>% 
      pivot_wider(
        names_from = all_of(c('stat', col_trt)),
        values_from = 'value'
      ) %>% 
      mutate(sd_diff = (mean_1 - mean_0) / sd_1)
    res
  }
  
  path_fit_bart <- file.path(get_bdb_dir_data(), 'fit_bart.rds')
  if(!file.exists(path_fit_bart)) {
  rec <-
    features_wide %>% 
    # recipes::recipe(formula(is_target_picked ~ .), .) %>% 
    recipes::recipe(formula(epa ~ is_target_picked + game_half_fct + x_o + x_d + dist_o + dist_d + yardline_100 + pre_snap_score_diff + pre_snap_home_score + down_fct + yards_to_go), .) %>% 
    # recipes::update_role(game_id, play_id, epa, epa_abs, is_pass_successful, has_same_defender, new_role = 'id') %>% 
    # recipes::step_normalize(recipes::all_numeric()) %>% 
    recipes::step_dummy(recipes::all_nominal()) %>% 
    recipes::step_interact(terms = ~(pre_snap_score_diff * pre_snap_home_score)) %>% 
    recipes::step_interact(terms = ~(matches('^down') * yards_to_go))
  rec
  df <- rec %>% recipes::prep() %>% recipes::juice()
  y <- df[['epa']]
  x <- df[,setdiff(names(df), 'epa')] %>% as.data.frame()
  # require(BART)
  # require(coda)
  fit_bart_1 <- BART::wbart(x.train = x, y.train = y)
  
  # fit_bart_2 <- BART::wbart(x.train = x, y.train = y)
  write_rds(fit_bart_1, path_fit_bart)
  } else {
    fit_bart_1 <- path_fit_bart %>% read_rds()
  }
  fit_bart_1$sigma %>% plot(type = 'l')
  abline(v = 100, lwd = 2, col = 'red')
  # preds <- predict(fit_bart_1, newdata = x)
  fit_lm <- lm(formula(epa ~ .), df)
  mat <- cbind(y, fit_bart_1$yhat.train.mean, fit_lm$fitted.values)
  colnames(mat) <- c('y', 'bart', 'lm')
  mat_cor <- mat %>% cor()
  mat_cor
  mat %>% pairs()

  # rhat <- gelman.diag(mcmc.list(mcmc(fit_bart_1$yhat.train), mcmc(fit_bart_2$yhat.train)), multivariate = FALSE)
  # ess <- effectiveSize(mcmc.list(mcmc(fit_bart_1$yhat.train), mcmc(fit_bart_2$yhat.train)))
  # rhat
  # ess
  cols_profile <- 'yardline_100'
  apply(df[, cols_profile], FUN =sd, MARGIN = 2)
  
  
  spec <-
    # parsnip::logistic_reg() %>% 
    # parsnip::set_engine('glm') %>% 
    parsnip::rand_forest() %>% 
    parsnip::set_engine('randomForest') %>% 
    parsnip::set_mode('classification')
  wf <-
    workflows::workflow() %>% 
    workflows::add_recipe(rec) %>% 
    workflows::add_model(spec)
  fit <- parsnip::fit(wf, features_wide)
  fit %>% workflows::pull_workflow_fit() %>% vip::vip(100)
  fit %>% broom::tidy()
  
  fit_is_target_picked_prob_prop <-
    features_wide %>% 
    glm(fmla_is_target_picked_prob_prop, data = ., family = stats::binomial)
  fit_is_target_picked_prob_prop
  
  res_match <-
    Matching::Match(
      caliper = 0.25,
      ties = FALSE,
      X = fit_is_target_picked_prob_prop %>% fitted(),
      # X = model.matrix(fmla, features_wide),
      Y = features_wide[['epa']],
      Tr = features_wide[['is_target_picked']] %>% as.integer() %>% {. - 1L}
    )
  # # Lots of unnecesary stuff.
  # res_match
  
  features_match <- 
    bind_rows(
      features_wide[res_match[['index.control']], ] %>% mutate(grp = 'control'), 
      features_wide[res_match[['index.treated']], ] %>% mutate(grp = 'treatment')
    )
  features_match
  
  sd_diffs <-
    bind_rows(
      features_wide %>% .aggregate_to_sd_diffs() %>% mutate(grp = 'Un-adjusted'),
      features_match %>% .aggregate_to_sd_diffs() %>% mutate(grp = 'Adjusted')
    ) %>% 
    mutate(
      across(grp, ~.x %>% fct_inorder())
    )
  sd_diffs
  
  sd_diffs_rnk <-
    sd_diffs %>% 
    filter(grp == 'Un-adjusted') %>% 
    # arrange(desc(abs_diff)) %>% 
    mutate(rnk = row_number(desc(abs(sd_diff)))) %>% 
    select(col, rnk) %>% 
    arrange(rnk)
  sd_diffs_rnk
  
  viz_love_is_target_picked_prob <-
    sd_diffs %>% 
    left_join(sd_diffs_rnk) %>% 
    mutate(across(col, ~fct_reorder(.x, -rnk))) %>% 
    arrange(grp, rnk) %>% 
    ggplot() +
    aes(y = col, x = abs(sd_diff), color = grp) +
    geom_vline(aes(xintercept = 0.1), size = 1, linetype = 2) +
    geom_point(size = 2) +
    geom_path(aes(group = grp), size = 0.5) +
    scale_color_manual(values = c(`Un-adjusted` = 'dodgerblue', `Adjusted` = 'darkorange')) +
    guides(
      color = guide_legend('', override.aes = list(size = 3))
    ) +
    theme(
      legend.position = 'top'
    ) +
    labs(
      title = 'Bias among coefficients for pick play model after matching',
      x = 'Absolute standardized mean difference',
      y = NULL
    )
  viz_love_is_target_picked_prob
  do_save_plot(viz_love_is_target_picked_prob)
  
  fmla_epa_w_picks_chr <-
    fmla_is_target_picked_prob_prop_chr %>% 
    str_replace_all(
      c(
        'is_target_picked' = 'epa_abs',
        '~' = '~ is_target_picked + '
      )
    )
  fmla_epa_w_picks <- fmla_epa_w_picks_chr %>% as.formula()
  
  fit_epa_w_picks <-
    features_match %>% 
    lm(fmla_epa_w_picks, data = .)
  fit_epa_w_picks
  
  coefs_epa_w_picks <-
    fit_epa_w_picks %>%
    broom::tidy() %>%
    mutate(
      across(term, ~fct_reorder(.x, estimate)),
      lo = estimate - 1.96 * std.error,
      hi = estimate + 1.96 * std.error,
      is_signif = if_else(p.value < 0.05, TRUE, FALSE)
    )
  coefs_epa_w_picks

  viz_epa_w_picks_coefs <-
    coefs_epa_w_picks %>%
    ggplot() +
    aes(y = term, x = estimate, color = is_signif) +
    geom_point(size = 2) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), size = 1) +
    scale_color_manual(values = c(`TRUE` = 'red', `FALSE` = 'black')) +
    geom_vline(data = tibble(), aes(xintercept = 0), linetype = 2) +
    guides(color = guide_legend('Is statistically significant?')) +
    theme(
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.position = 'top' # ,
      # legend.box = element_rec()
    ) +
    labs(
      title = 'Linear regression EPA model coefficients, adjusting for pick plays',
      y = NULL, x = 'Estimate +- 1.96 standard error'
    )
  viz_epa_w_picks_coefs
  do_save_plot(viz_epa_w_picks_coefs)

  # don't need this quite yet
  .binary_factor_to_lgl <- function(x) {
    x %>% as.integer() %>% {. - 1L} %>% as.logical()
  }
  
  .simplify_pick_features <- function(data) {
    data %>% 
      mutate(
        across(c(is_target_picked, is_pass_successful, has_same_defender), .binary_factor_to_lgl),
        across(
          is_target_picked, ~sprintf('Target Is Picked? %s', ifelse(.x, 'Y', 'N'))
        ),
        across(
          is_pass_successful, ~sprintf('Pass Successful? %s', ifelse(.x, 'Y', 'N'))
        ),
        across(
          has_same_defender, ~sprintf('Same Defender? %s', ifelse(.x, 'Y', 'N'))
        )
      ) %>%
      select(is_target_picked, is_pass_successful, is_same_defender = has_same_defender, epa)
  }
  
  # def ----
  fmla_has_same_defender_prob_prop_chr <-
    fmla_is_target_picked_prob_prop_chr %>% 
    str_replace_all(
      c(
        'is_target_picked' = 'has_same_defender'
      )
    ) %>% 
    paste0('')
  
  fmla_has_same_defender_prob_prop <- fmla_has_same_defender_prob_prop_chr %>% as.formula()
  
  res_match <-
    Matching::Match(
      caliper = 0.25,
      ties = FALSE,
      X = fit_is_target_picked_prob_prop %>% fitted(),
      # X = model.matrix(fmla, features_wide),
      Y = features_wide[['epa_abs']],
      Tr = features_wide[['is_target_picked']] %>% as.integer() %>% {. - 1L}
    )
  # # Lots of unnecesary stuff.
  # res_match
  
  control_match <- features_wide[res_match[['index.control']], ]
  treatment_match <- features_wide[res_match[['index.treated']], ]
  features_match <- 
    bind_rows(
      control_match %>% mutate(grp = 'control'), 
      treatment_match %>% mutate(grp = 'treatment')
    )
  features_match
  
  .str_replace_f <- function(x, i) {
    x %>% str_replace('(^.*)_(mean|sd)$', sprintf('\\%d', i))
  }
  
  fit_has_same_defender_prob_prop <-
    features_wide %>% 
    # filter(is_pass_successful == '0') %>% 
    filter(is_target_picked == '1') %>% 
    glm(fmla_has_same_defender_prob_prop, data = ., family = stats::binomial)
  fit_has_same_defender_prob_prop
  
  # features_wide %>% .simplify_pick_features()
  
  fmla_epa_w_picks_def_chr <-
    fmla_is_target_picked_prob_prop_chr %>% 
    str_replace_all(
      c(
        # 'is_target_picked' = 'has_same_defender*is_target_picked'
        'is_target_picked' = 'has_same_defender*is_target_picked'
      )
    ) %>% 
    paste0('')
  fmla_epa_w_picks_def <- fmla_epa_w_picks_def_chr %>% as.formula()
  # fmla_epa_w_picks_def <- formula(abs(epa) ~ has_same_defender*is_target_picked)
  
  fit_epa_w_picks_def <-
    features_match %>% 
    # filter(is_pass_successful == '0') %>% 
    filter(is_target_picked == '1') %>% 
    lm(fmla_epa_w_picks_def, data = .)
  fit_epa_w_picks_def
  
  coefs_epa_w_picks_def <-
    fit_epa_w_picks_def %>%
    broom::tidy() %>%
    mutate(
      across(term, ~fct_reorder(.x, estimate)),
      lo = estimate - 1.96 * std.error,
      hi = estimate + 1.96 * std.error,
      is_signif = if_else(p.value < 0.05, TRUE, FALSE)
    )
  coefs_epa_w_picks_def
  
  viz_epa_w_picks_def_coefs <-
    coefs_epa_w_picks_def %>%
    ggplot() +
    aes(y = term, x = estimate, color = is_signif) +
    geom_point(size = 2) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), size = 1) +
    scale_color_manual(values = c(`TRUE` = 'red', `FALSE` = 'black')) +
    geom_vline(data = tibble(), aes(xintercept = 0), linetype = 2) +
    guides(color = guide_legend('Is statistically significant?')) +
    theme(
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.position = 'top' # ,
      # legend.box = element_rec()
    ) +
    labs(
      title = 'Linear regression EPA model coefficients, adjusting for pick plays',
      y = NULL, x = 'Estimate +- 1.96 standard error'
    )
  viz_epa_w_picks_def_coefs
  do_save_plot(viz_epa_w_picks_def_coefs)
  
}

# t test stuff ----
if(FALSE) {

# .simplify_pick_features <- function(data) {
#   data %>% 
#     mutate(
#       across(c(has_intersect, is_pass_successful), .binary_factor_to_lgl),
#       across(
#         has_intersect, ~sprintf('Target Is Picked? %s', ifelse(.x, 'Y', 'N'))
#       ),
#       across(
#         is_pass_successful, ~sprintf('Pass Successful? %s', ifelse(.x, 'Y', 'N'))
#       )
#     ) %>%
#     select(is_target_picked = has_intersect, is_pass_successful, epa)
# }

# gtsummary offensive t tests----
.f_gtsummary <- function(data, by) {
  data %>% 
    gtsummary::tbl_summary(
      statistic = list(
        gtsummary::all_continuous() ~ '{median} ({p25}, {p75})'
      ),
      by = all_of(by)
    ) %>%
    gtsummary::add_p(
      test = gtsummary::all_continuous() ~ 't.test',
      pvalue_fun = function(x) gtsummary::style_pvalue(x, digits = 2)
    )
}

do_save_t_test_tabs <- function(data, suffix = NULL, sep = '_') {

  t1 <-
    data %>%
    select(is_pass_successful, epa) %>% 
    mutate(
      across(is_pass_successful, ~.x %>% str_replace_all('(.*)([YN]$)', '\\2'))
    ) %>%
    .f_gtsummary('is_pass_successful')
  t1
  
  t2 <-
    data %>%
    select(is_target_picked, epa) %>% 
    mutate(
      across(is_target_picked, ~.x %>% str_replace_all('(.*)([YN]$)', '\\2'))
    ) %>%
    .f_gtsummary('is_target_picked')
  t2
  
  res <- 
    gtsummary::tbl_merge(
      list(t1, t2), 
      tab_spanner = c('Pass Successful?', 'Is Target Picked?')
    ) %>% 
    gtsummary::as_gt()
  if(is.null(suffix)) {
    suffix <- ''
  } else {
    suffix <- sprintf('%s%s', sep, suffix)
  }
  gt::gtsave(res, filename = file.path(get_bdb_dir_figs(), sprintf('tab_t_test_epa%s.png', suffix)))
  res
}

features_wide %>% .simplify_pick_features() %>% do_save_t_test_tabs(suffix = 'nonadjusted')
features_match %>% .simplify_pick_features() %>% do_save_t_test_tabs(suffix = 'adjusted')

# non gtsummary offensive pick play t tests----
.f_agg_epa <- function(data) {

  data %>% 
    group_by(is_target_picked, is_pass_successful) %>% 
    summarize(across(c(epa), list(median = median, q25 = ~quantile(.x, 0.25), q75 = ~quantile(.x, 0.75)), .names = '{fn}')) %>% 
    ungroup() %>% 
    mutate(
      value = sprintf('%.02f (%.02f, %.02f)', median, q25, q75)
    )
}

.f_agg_n <- function(data) {

  data %>% 
    group_by(is_target_picked, is_pass_successful) %>% 
    summarize(n = n()) %>% 
    ungroup() %>% 
    group_by(is_target_picked) %>% 
    mutate(total = sum(n), frac = n / total) %>% 
    ungroup() %>% 
    mutate(
      # is_pass_successful = sprintf('%s (%s)', is_pass_successful, scales::comma(total)),
      value = sprintf('%s (%s)', scales::comma(n), scales::percent(frac, accuracy = 0.1))
    )
}

.f_pivot_agg <- function(data) {
  data %>% 
    select(is_target_picked, is_pass_successful, value) %>% 
    pivot_wider(names_from = is_target_picked, values_from = value) %>% 
    mutate(
      across(is_pass_successful, ~.x %>% str_replace_all('(.*)([YN]$)', '\\2'))
    )
}

.f_gt <- function(data, subtitle) {

  data %>% 
    rename(`Pass Successful?` = is_pass_successful) %>% 
    gt::gt() %>% 
    gt::tab_header(
      subtitle = subtitle,
      title = gt::md('EPA by pass outcome')
    ) %>% 
    gt::cols_label(
      # `t-test p-value` = gt::md('**t-test\np-value**'),
      `Target Is Picked? N` = gt::md('**Target Is Picked? N**'),
      `Target Is Picked? Y` = gt::md('**Target Is Picked? Y**')
    )
}

.f_save_gt <- function(gt, suffix, prefix = c('epa', 'n')) {
  prefix <- match.arg(prefix)
  gt %>% 
    gt::gtsave(filename = file.path(get_bdb_dir_figs(), sprintf('tab_%s%s.png', prefix, suffix)))
}

do_save_epa_tabs <- function(data, subtitle = NULL, suffix = NULL, sep = '_') {
  # browser()
  # data <- features_wide %>% .simplify_pick_features(); subtitle = NULL; suffix = NULL; sep = '_'
  tab_epa <- 
    data %>% 
    .f_agg_epa() %>% 
    .f_pivot_agg()
  tab_epa

  .t_test_target <- function(cnd = c('Y', 'N')) {
    # cnd <- match.arg(cnd)
    t.test(epa ~ is_target_picked, data = data %>% filter(is_pass_successful %>% str_detect(sprintf('%s$', cnd))))
  }
  # .t_test_success <- function(cnd = c('Y', 'N')) {
  #   # cnd <- match.arg(cnd)
  #   t.test(epa ~ is_pass_successful, data = data %>% filter(is_target_picked %>% str_detect(sprintf('%s$', cnd))))
  # }
  t_target_y <- .t_test_target('Y')
  t_target_n <- .t_test_target('N')
  # t_success_y <- .t_test_success('Y')
  # t_success_n <- .t_test_success('N')
  tab_epa['t-test p-value'] <- 
    c(t_target_n$p.value, t_target_y$p.value) %>% 
    sprintf('%0.03f', .)
  
  tab_n <- data %>% .f_agg_n() %>% .f_pivot_agg()
  
  if(is.null(suffix)) {
    suffix <- ''
  } else {
    suffix <- sprintf('%s%s', sep, suffix)
  }
  tab_epa %>% .f_gt(subtitle = subtitle) %>% .f_save_gt(suffix = suffix, prefix = 'epa')
  tab_n %>% .f_gt(subtitle = subtitle) %>% .f_save_gt(suffix = suffix, prefix = 'n')
  # list(epa = tab_epa, n = tab_n)
  invisible()
}

features_wide %>% .simplify_pick_features() %>% do_save_epa_tabs(subtitle = 'Before matching', suffix = 'nonadjusted')
features_match %>% .simplify_pick_features() %>% do_save_epa_tabs(subtitle = 'After matching', suffix = 'adjusted')

# defense t tests----
}
