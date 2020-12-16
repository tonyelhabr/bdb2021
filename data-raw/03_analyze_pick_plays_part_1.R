
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

# pick_play_meta_init and plays_w_pick_off_info ----
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

plays_w_pick_off_info <-
  plays %>%
  mutate(across(down, list(fct = factor))) %>% 
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
    across(n_rusher, ~coalesce(.x, 0L))
  ) %>%
  relocate(game_id, play_id, has_intersect, pick_data) %>%
  left_join(
    pbp %>%
      mutate(
        across(roof, factor),
        is_home = if_else(home_team == posteam, 1, 0) %>% factor()
      ) %>% 
      select(
        game_id,
        play_id,
        # game_half,
        half_seconds_remaining,
        yardline_100,
        is_home,
        roof,
        # wp_nflfastr = wp,
        # wpa_nflfastr = wpa,
        # epa_nflfastr = epa,
        away_timeouts_remaining,
        home_timeouts_remaining
      ),
    by = c('game_id', 'play_id')
  ) %>%
  select(-pick_data) %>%
  mutate(
    across(has_intersect, ~if_else(.x, 1L, 0L) %>% factor())
  )
plays_w_pick_off_info
usethis::use_data(plays_w_pick_off_info, overwrite = TRUE)

viz_pick_play_frac <-
  plays_w_pick_off_info %>% 
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

# defense intersections, ending with pick_features df ----
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

# causal analysis ----
# data('plays_w_pick_off_info', package = 'bdb2021')
# data('plays_w_pick_def_info', package = 'bdb2021')
# plays_w_pick_off_info %>% count(is_pass_successful, is_epa_neg = epa < 0)
plays_w_pick_off_info_min <-
  plays_w_pick_off_info %>%
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
    
    # game_half,
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
  inner_join(plays_w_pick_off_info_min) %>% 
  inner_join(plays_w_pick_def_info_min)
features_wide
# arrow::write_parquet(features_wide, file.path('inst', 'features_wide.parquet'))

.str_replace_f <- function(x, i) {
  x %>% str_replace('(^.*)_(mean|sd)$', sprintf('\\%d', i))
}

.get_valid_col_trt <- function() {
  c('is_target_picked', 'has_same_defender')
}
.validate_col_trt <- function(x = .get_valid_col_trt()) {
  match.arg(x)
}

.aggregate_to_sd_diffs <- function(data, col_trt) {
  .validate_col_trt(col_trt)
  cols_features <- 
    c(
      'x_o', 'x_d', 'dist_o', 'dist_d', 
      'down_fct',
      'yards_to_go',
      'half_seconds_remaining',
      'yardline_100',
      'is_home',
      'roof',
      'away_timeouts_remaining',
      'home_timeouts_remaining'
    )
  col_trt_sym <- col_trt %>% sym()
  agg <-
    data %>% 
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

do_causal_stuff <- function(col_trt) {
  
  col_trt <- 'is_target_picked'
  fmla_chr <-
    sprintf('%s ~ x_o + x_d + dist_o + dist_d + half_seconds_remaining + yardline_100 + away_timeouts_remaining + home_timeouts_remaining + roof + is_home + down_fct*yards_to_go', col_trt)
  fmla <- fmla_chr %>% as.formula()
  
  fit <-
    features_wide %>% 
    glm(fmla, data = ., family = stats::binomial)
  fit
  
  res_match <-
    Matching::Match(
      caliper = 0.25,
      ties = FALSE,
      X = fit %>% fitted(),
      Y = features_wide[['epa']],
      Tr = features_wide[[col_trt]] %>% as.integer() %>% {. - 1L}
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
      features_wide %>% .aggregate_to_sd_diffs(col_trt = col_trt) %>% mutate(grp = 'Un-adjusted'),
      features_match %>% .aggregate_to_sd_diffs(col_trt = col_trt) %>% mutate(grp = 'Adjusted')
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
  
  if(col_trt == 'is_target_picked') {
    title_suffix_1 <- 'targeted pick play'
    title_suffix_2 <- 'targeted picks'
  } else if (col_trt == 'has_same_defender') {
    title_suffix_1 <- 'targeted defender coverage'
    title_suffix_2 <- 'coverage'
  }
  file_suffix <- col_trt
  
  viz_love <-
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
      title = sprintf('Bias among coefficients for %s probability model', title_suffix_1),
      x = 'Absolute standardized mean difference',
      y = NULL
    )
  viz_love
  do_save_plot(viz_love, file = sprintf('viz_love_%s', file_suffix))
  
  fmla_epa_chr <-
    fmla_chr %>% 
    # Unfortunately can't put `col_trt` on the left-hand side here, so replace it afterwards.
    str_replace_all(
      c(
        '~' = sprintf('~ %s + ', col_trt)
      )
    )
  fmla_epa_chr <- fmla_epa_chr %>% str_replace('.*[~]', 'epa ~')
  fmla_epa <- fmla_epa_chr %>% as.formula()
  
  fit_epa <-
    features_match %>% 
    lm(fmla_epa, data = .)
  fit_epa
  
  coefs_epa <-
    fit_epa %>%
    broom::tidy() %>%
    mutate(
      across(term, ~fct_reorder(.x, estimate)),
      lo = estimate - 1.96 * std.error,
      hi = estimate + 1.96 * std.error,
      is_signif = if_else(p.value < 0.05, TRUE, FALSE)
    )
  coefs_epa
  
  viz_epa <-
    coefs_epa %>%
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
      title = sprintf('Linear regression EPA model coefficients, adjusting for %s', title_suffix_2),
      y = NULL, x = 'Estimate +- 1.96 standard error'
    )
  viz_epa
  do_save_plot(viz_epa, file = sprintf('viz_epa_%s', file_suffix))
  features_match
}


# .get_valid_col_trt() %>% walk(do_causal_stuff)
features_match_is_target_picked <- 'is_target_picked' %>% do_causal_stuff()
features_match_has_same_defender <- 'has_same_defender' %>% do_causal_stuff()

# t test stuff ----
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

features_wide_simple <- 
  features_wide %>% 
  .simplify_pick_features()

features_wide_simple %>% 
  do_save_t_test_tabs(suffix = 'unadjusted')

features_match_is_target_picked_simple <- 
  features_match_is_target_picked %>% 
  .simplify_pick_features()

features_match_is_target_picked_simple %>% 
  do_save_t_test_tabs(suffix = 'adjusted_is_target_picked')

features_match_has_same_defender_simple <- 
  features_match_has_same_defender %>% 
  .simplify_pick_features()

features_match_has_same_defender_simple %>% 
  do_save_t_test_tabs(suffix = 'adjusted_has_same_defender')

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

  tab_epa <- 
    data %>% 
    .f_agg_epa() %>% 
    .f_pivot_agg()
  tab_epa
  
  .t_test_target <- function(cnd = c('Y', 'N')) {
    t.test(epa ~ is_target_picked, data = data %>% filter(is_pass_successful %>% str_detect(sprintf('%s$', cnd))))
  }

  t_target_y <- .t_test_target('Y')
  t_target_n <- .t_test_target('N')

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
  invisible()
}

features_wide_simple %>%
  do_save_epa_tabs(suffix = 'unadjusted_nosubtitle')

features_wide_simple %>% 
  do_save_epa_tabs(subtitle = 'Before matching', suffix = 'unadjusted')

features_match_is_target_picked_simple %>% 
  do_save_epa_tabs(
    subtitle = 'After matching for targeted pick plays', 
    suffix = 'adjusted_is_target_picked'
  )

features_match_has_same_defender_simple %>% 
  do_save_epa_tabs(
    subtitle = 'After matching for targeted defender coverage', 
    suffix = 'adjusted_has_same_defender'
  )

