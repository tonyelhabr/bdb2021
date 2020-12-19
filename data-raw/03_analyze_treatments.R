
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
def_features <- import_new_features()
min_dists_naive_od_target <- import_min_dists_naive_od_target()

do_save_plot <- function(...) {
  if(TRUE) {
    save_plot(...)
  }
}

# basic eda ----
.binary_factor_to_lgl <- function(x) {
  x %>% as.integer() %>% {. - 1L} %>% as.logical()
}
if(FALSE) {
viz_epa_swarm <-
  plays %>% 
  mutate(
    # across(is_pass_successful, .binary_factor_to_lgl),
    across(is_pass_successful, ~if_else(.x == '1', 'Y', 'N')),
    across(
      epa, ~case_when(
        .x < -3 ~ -3,
        .x > 3 ~ 3,
        TRUE ~ .x
      )
    )
  ) %>% 
  ggplot() +
  aes(x = epa, y = is_pass_successful, color = is_pass_successful) +
  ggbeeswarm::geom_quasirandom(
    groupOnX = FALSE, 
    varwidth = TRUE, 
    size = 1, 
    alpha = 0.2
  ) +
  scale_color_manual(values = c(`Y` = 'dodgerblue', `N` = 'darkorange')) +
  guides(
    color = guide_legend('Pass successful?', override.aes = list(alpha = 1, size = 3))
  ) +
  theme(
    legend.position = 'top',
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    title = 'EPA by pass success',
    caption = 'EPA truncated to -3 and 3.',
    x = 'EPA',
    y = NULL
  )
viz_epa_swarm
do_save_plot(viz_epa_swarm)

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

# offensive features for models ----
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

.f_nrow <- function(id, data) {
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
  mutate(is_target_picked = map2_int(target_nfl_id, pick_data, ~.f_nrow(..1, ..2)) %>% factor()) %>% 
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
        model_roof = if_else(
          is.na(roof) | roof == 'open' | roof == 'closed', 'retractable',
          as.character(roof)
        ),
        model_roof = as.factor(model_roof),
        retractable = if_else(model_roof == 'retractable', 1, 0),
        dome = if_else(model_roof ==  'dome', 1, 0),
        outdoors = if_else(model_roof == 'outdoors', 1, 0),
        across(roof, factor),
        is_home = if_else(home_team == posteam, 1, 0) %>% factor()
      ) %>% 
      select(
        game_id,
        play_id,
        half_seconds = half_seconds_remaining,
        yardline_100,
        is_home,
        retractable,
        dome,
        outdoors,
        roof,
        off_timeouts = posteam_timeouts_remaining,
        def_timeouts = defteam_timeouts_remaining,
        
        wp_nflfastr = wp,
        wpa_nflfastr = wpa,
        epa_nflfastr = epa
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

# defensive features for models ----
# Only want the ball snap and end rush frames, not the other event frames.
events_end_rush <- .get_events_end_rush()
features_min_init <-
  def_features %>%
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
      # filter(sec <= .sec_cutoff) %>% 
      mutate(before_cutoff = if_else(sec <= .sec_cutoff, TRUE, FALSE)) %>% 
      select(week, game_id, play_id, nfl_id, nfl_id_intersect, sec_intersect = sec, is_lo, before_cutoff) %>%
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
  fill(had_intersect, before_cutoff) %>%
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
features_lag %>% count(before_cutoff)

# Taking the 1 non-snap frame of each play
pick_features <-
  features_lag %>% 
  filter(sec > 0) %>% 
  # `had_intersect` is redundant with `has_intersect` if there is only one frame per play and it's the last frame.
  select(-had_intersect) %>% 
  # This is the last second measured on the play.
  select(-sec)
pick_features %>% count(before_cutoff)

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
    is_target = if_else(nfl_id == nfl_id_target, TRUE, FALSE)
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
plays_w_pick_def_info # %>% filter(!before_cutoff)
usethis::use_data(plays_w_pick_def_info, overwrite = TRUE)
}

# causal analysis prep----
data('plays_w_pick_off_info', package = 'bdb2021')
data('plays_w_pick_def_info', package = 'bdb2021')
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
    
    down = down_fct,
    yards_to_go,
    
    # nflfastR stuff
    
    half_seconds,
    yardline_100,
    is_home,
    # retractable,
    # dome,
    # outdoors,
    roof,
    off_timeouts,
    def_timeouts
  )

plays_w_pick_def_info_min <-
  plays_w_pick_def_info %>% 
  filter(nfl_id == nfl_id_target) %>% 
  # filter(before_cutoff) %>% 
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

features <-
  min_dists_naive_od_target_filt %>% 
  select(-target_nfl_id) %>% 
  inner_join(plays_w_pick_off_info_min) %>% 
  inner_join(plays_w_pick_def_info_min)
features
# arrow::write_parquet(features, file.path('inst', 'features.parquet'))
arrow::write_parquet(features, file.path(get_bdb_dir_data(), 'plays_w_pick_info_features.parquet'))

.str_replace_f <- function(x, i) {
  x %>% str_replace('(^.*)_(mean|sd)$', sprintf('\\%d', i))
}

.get_valid_col_trt <- function() {
  c('is_target_picked', 'has_same_defender')
}

.validate_col_trt <- function(x = .get_valid_col_trt(), ...) {
  match.arg(x, ...)
}
# dag stuff ----
.tidy_dag <- function(ggdag) {
  ggdag %>% 
    ggdag::tidy_dagitty() %>%
    ggdag::node_status()
}

.node_labels <-
  c(
    'epa' = 'EPA',
    'has_same_defender' = 'Same Defender?',
    'is_target_picked' = 'Target Picked?',
    'player_tracking' = 'Player Tracking',
    'epa_predictors' = 'EPA Predictors'
  )

.generate_simple_tidy_dag <- function(exposure = .get_valid_col_trt()) {
  exposure <- .validate_col_trt(exposure)
  fmla <- sprintf('epa ~ %s', exposure) %>% as.formula()
  ggdag::dagify(
    fmla,
    exposure = exposure,
    outcome = 'epa',
    labels = .node_labels
  ) %>%
    .tidy_dag()
}

.generate_tidy_dag <- function(..., exposure = .get_valid_col_trt()) {
  exposure <- .validate_col_trt(exposure, several.ok = TRUE)
  ggdag::dagify(
    epa ~ epa_predictors,
    epa ~ player_tracking,
    ...,
    exposure = exposure,
    outcome = 'epa',
    labels = .node_labels
  ) %>%
    .tidy_dag()
}

.save_dag <- function(data, col, suffix = NULL, ..., sep = '_') {
  status_colors <-
    c(
      exposure = 'cornflowerblue',
      outcome = 'darkorange',
      latent = 'grey50'
    )
  na_color <- 'grey20'
  viz <-
    data %>%
    ggplot() +
    aes(x = x, y = y, xend = xend, yend = yend) +
    ggdag::geom_dag_edges() +
    ggdag::geom_dag_point(aes(color = status)) +
    ggdag::geom_dag_label_repel(
      aes(label = label, fill = status),
      seed =  42,
      color = 'white',
      fontface = 'bold'
    ) +
    scale_color_manual(values = status_colors, na.value = na_color) +
    scale_fill_manual(values = status_colors, na.value = na_color) +
    guides(color = FALSE, fill = FALSE) +
    # ggdag::theme_dag() +
    theme(
      # panel.grid = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank()
    ) +
    labs(
      x = NULL, y = NULL,
      ...
    )
  if(is.null(suffix)) {
    suffix <- ''
  } else {
    suffix <- sprintf('%s%s', sep, suffix)
  }
  ggsave(viz, filename = file.path(get_bdb_dir_figs(), sprintf('dag_%s%s.png', col, suffix)), type = 'cairo', width = 6, height = 6)
  viz
}

.save_simple_tidy_dag <- function(col, suffix = 't-test', col_pretty = NULL) {
  dag <- .generate_simple_tidy_dag(exposure = col)
  if(is.null(col_pretty)) {
    col_pretty <-
      case_when(
        col == 'is_target_picked' ~ 'Target Picked?',
        col == 'has_same_defender' ~ 'Same Defender?'
      )
    
  }  .save_dag(dag, col = col, suffix = suffix, title = sprintf('DAG for t-test for %s', col_pretty))
}

cols_trt <- .get_valid_col_trt()
walk(cols_trt, .save_simple_tidy_dag)

dag_is_target_picked <- .generate_tidy_dag(exposure = 'is_target_picked', epa ~ is_target_picked)
dag_has_same_defender <- .generate_tidy_dag(exposure = 'has_same_defender', epa ~ has_same_defender)

dag_hard <- 
  .generate_tidy_dag(
    epa ~ is_target_picked, 
    epa ~ has_same_defender ,
    has_same_defender ~ is_target_picked
  )

dag_is_target_picked %>%
  filter(name != 'player_tracking') %>%
  .save_dag(
    suffix = 'wo_player_tracking',
    title = 'DAG for estimating causal effect of\ntargeted pick route',
    subtitle = 'Without player tracking',
    col = 'is_target_picked'
  )

dag_is_target_picked %>%
  .save_dag(
    title = 'DAG for estimating causal effect of\ntargeted pick route',
    col = 'is_target_picked'
  )

dag_has_same_defender %>%
  .save_dag(
    title = 'DAG for estimating causal effect of\ntargeted defender coverage',
    col = 'has_same_defender'
  )

dag_hard %>%
  .save_dag(
    title = 'DAG for estimating causal effects of\ntargeted pick route and defender coverage',
    col = 'simultaneous'
  )

# causal stuff ----

.extract_x_from_fmla <- function(fmla) {
  # browser()
  fmla[[3]] %>% 
    as.character() %>% 
    .[2:length(.)] %>% 
    paste0(collapse = ' + ') %>% 
    str_replace_all(c('\\s[+*]\\s' = ', ')) %>% 
    # Remove leading +
    str_remove_all('[+]') %>% 
    str_trim() %>% 
    str_split(pattern = '\\, ') %>% 
    pluck(1) %>% 
    unique()
}

.aggregate_to_sd_diffs <- function(data, col_trt, fmla) {
  
  .validate_col_trt(col_trt)
  cols_features <- .extract_x_from_fmla(fmla)
  
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

do_causal_analysis <- function(col_trt = .get_valid_col_trt()) {
  # col_trt <- 'has_same_defender'
  .validate_col_trt(col_trt)

  fmla_chr <-
    sprintf('%s ~ x*y + x_o*dist_o + x_d*dist_d + x_o*x_d + dist_o*dist_d + half_seconds + yardline_100 + off_timeouts + def_timeouts + roof + is_home + down*yards_to_go', col_trt)
  # fmla_chr <- sprintf('%s ~ x', 'y')
  fmla <- fmla_chr %>% as.formula()
  
  fit <-
    glm(fmla, data = features, family = 'binomial')
  fit
  
  res_match <-
    Matching::Match(
      # caliper = 0.01,
      exact = TRUE,
      ties = FALSE,
      X = fit %>% fitted(),
      Y = features[['epa']],
      Tr = features[[col_trt]] %>% as.integer() %>% {. - 1L}
    )
  # # Lots of unnecesary stuff.
  # res_match
  # res_balance <- Matching::MatchBalance(fmla, data = features %>% mutate(across(col_trt, .binary_factor_to_lgl)), match.out = res_match, nboots = 5)
  
  features_match <- 
    bind_rows(
      features[res_match[['index.control']], ] %>% mutate(grp = 'control'), 
      features[res_match[['index.treated']], ] %>% mutate(grp = 'treatment')
    )
  features_match
  
  if(col_trt == 'is_target_picked') {
    title_suffix_1 <- 'targeted pick play'
    title_suffix_2 <- 'targeted picks'
    ylim <- 100
    axis_label <- 'target picked'
  } else if (col_trt == 'has_same_defender') {
    title_suffix_1 <- 'targeted defender coverage'
    title_suffix_2 <- 'coverage'
    ylim <- 150
    axis_label <- 'same defender'
  }
  file_suffix <- col_trt
  
  .f_predict <- function(data) {
    fit %>% broom::augment(newdata = data, type.predict = 'response')
  }
  
  preds <-
    bind_rows(
      features %>% .f_predict() %>% mutate(grp_adj = 'Un-adjusted'),
      features_match %>% .f_predict() %>% mutate(grp_adj = 'Adjusted')
    ) %>% 
    mutate(
      across(c(grp), str_to_title)
    )
  preds

  viz_prop_probs <-
    preds %>% 
    ggplot() + 
    aes(x = .fitted, fill = grp) + 
    geom_histogram(data = . %>% filter(grp_adj == 'Un-adjusted'), fill = 'grey80', binwidth = 0.02, aes(y = -..count..)) +
    geom_histogram(data = . %>% filter(grp == 'Control', grp_adj == 'Adjusted'), binwidth = 0.02, aes(y = -..count..)) +
    geom_histogram(data = . %>% filter(grp == 'Treatment', grp_adj == 'Adjusted'), binwidth = 0.02) +
    scale_fill_manual(values = c(`Control` = 'dodgerblue', `Treatment` = 'darkorange')) +
    coord_cartesian(ylim = c(-ylim, ylim), clip = 'off') +
    guides(fill = guide_legend('')) +
    theme(
      plot.title = ggtext::element_markdown(size = 16, face = 'bold'),
      legend.position = 'top'
    ) +
    labs(
      title = sprintf('Probabiliites fit for <i>%s</i> model', title_suffix_1),
      caption = 'Gray illustrates un-adjusted samples, while colors illustrate adjusted samples.',
      y = '# of plays',
      x = sprintf('P(%s)', axis_label)
    )
  viz_prop_probs
  do_save_plot(viz_prop_probs, file = sprintf('viz_prop_probs_%s', file_suffix))
  
  sd_diffs <-
    bind_rows(
      features %>% 
        .aggregate_to_sd_diffs(col_trt = col_trt, fmla = fmla) %>% 
        mutate(grp_adj = 'Un-adjusted'),
      features_match %>% 
        .aggregate_to_sd_diffs(col_trt = col_trt, fmla = fmla) %>% 
        mutate(grp_adj = 'Adjusted')
    ) %>% 
    mutate(
      across(grp_adj, ~.x %>% fct_inorder())
    )
  sd_diffs
  
  sd_diffs_rnk <-
    sd_diffs %>% 
    filter(grp_adj == 'Un-adjusted') %>% 
    # arrange(desc(abs_diff)) %>% 
    mutate(rnk = row_number(desc(abs(sd_diff)))) %>% 
    select(col, rnk) %>% 
    arrange(rnk)
  sd_diffs_rnk

  viz_love <-
    sd_diffs %>% 
    left_join(sd_diffs_rnk) %>% 
    # .prep_viz_data() %>% 
    mutate(across(col, ~fct_reorder(.x, -rnk))) %>% 
    rename(lab = col) %>% 
    arrange(grp_adj, rnk) %>% 
    ggplot() +
    aes(y = lab, x = abs(sd_diff), color = grp_adj) +
    geom_vline(aes(xintercept = 0.1), size = 1, linetype = 2) +
    geom_point(size = 2) +
    geom_path(aes(group = grp_adj), size = 0.5) +
    scale_color_manual(values = c(`Un-adjusted` = 'dodgerblue', `Adjusted` = 'darkorange')) +
    guides(
      color = guide_legend('', override.aes = list(size = 3))
    ) +
    theme(
      axis.text.y = ggtext::element_markdown(),
      plot.title = ggtext::element_markdown(size = 16, face = 'bold'),
      legend.position = 'top'
    ) +
    labs(
      title = sprintf('Bias among coefficients for <i>%s</i> probability model', title_suffix_1),
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
      is_signif = if_else(sign(lo) == sign(hi), TRUE, FALSE)
    )
  coefs_epa
  
  viz_epa <-
    coefs_epa %>%
    mutate(
      color = 
        case_when(
          str_detect(term, !!col_trt) ~ '#000000', #  gplots::col2hex('cornflowerblue'), #  gplots::col2hex('#0000ff'), 
          TRUE ~ gplots::col2hex('grey30')
        ),
      tag = 
        case_when(
          str_detect(term, !!col_trt) ~ 'b',
          TRUE ~ 'span'
        )
      lab = glue::glue('<{tag} style="color:{color}">{term}</{tag}>'),
      across(lab, ~fct_reorder(.x, estimate))
    ) %>% 
    ggplot() +
    aes(y = lab, x = estimate, color = is_signif) +
    geom_point(size = 2) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), size = 1) +
    scale_color_manual(values = c(`TRUE` = 'red', `FALSE` = 'black')) +
    geom_vline(data = tibble(), aes(xintercept = 0), linetype = 2) +
    guides(color = guide_legend('Is statistically significant?')) +
    theme(
      axis.text.y = ggtext::element_markdown(),
      plot.title = ggtext::element_markdown(size = 16, face = 'bold'),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 10),
      legend.position = 'top' # ,
      # legend.box = element_rec()
    ) +
    labs(
      title = sprintf('Linear regression EPA model coefficients, adjusting for <i>%s</i>', title_suffix_2),
      y = NULL, x = 'Estimate +- 1.96 standard error'
    )
  viz_epa
  do_save_plot(viz_epa, file = sprintf('viz_epa_%s', file_suffix))
  features_match
}


# .get_valid_col_trt() %>% walk(do_causal_analysis)
features_match_is_target_picked <- 'is_target_picked' %>% do_causal_analysis()
features_match_has_same_defender <- 'has_same_defender' %>% do_causal_analysis()

# t test stuff ----
.simplify_pick_features <- function(data) {
  data %>% 
    mutate(
      across(c(is_target_picked, is_pass_successful, has_same_defender), .binary_factor_to_lgl),
      across(
        is_target_picked, ~sprintf('Target Picked? %s', ifelse(.x, 'Y', 'N'))
      ),
      across(
        is_pass_successful, ~sprintf('Pass Successful? %s', ifelse(.x, 'Y', 'N'))
      ),
      across(
        has_same_defender, ~sprintf('Same Defender? %s', ifelse(.x, 'Y', 'N'))
      )
    ) %>%
    select(is_target_picked, is_pass_successful, has_same_defender, epa)
}

# gtsummary offensive t tests----
.get_valid_col_value <- function() {
  c('epa', 'n')
}

.validate_col_value <- function(x = .get_valid_col_value(), ...) {
  match.arg(x, ...)
}

.f_gtsummary <- function(data, by, col_value) {
  .validate_col_value(col_value)
  
  res <-
    data %>% 
    gtsummary::tbl_summary(
      statistic = list(
        gtsummary::all_integer() ~ '{N}',
        gtsummary::all_continuous() ~ '{mean} ({p25}, {p75})'
      ),
      by = all_of(by)
    )
  
  if(col_value == 'n') {
    return(res)
  }
  res %>% 
    gtsummary::add_p(
      test = gtsummary::all_continuous() ~ 't.test',
      # test = gtsummary::all_continuous() ~ '.t_test_epa',
      pvalue_fun = function(x) gtsummary::style_pvalue(x, digits = 2)
    )
}

.f_col_gtsummary <- function(data, col, ..., col_value) {
  .validate_col_value(col_value)
  data %>% 
    select(!!sym(col), ...) %>% 
    .f_gtsummary(col, col_value = col_value)
}

.postprocess_t_test_tabs <- function(..., col_value, col, subtitle = NULL, suffix = NULL, sep = '_') {
  .validate_col_trt(col)
  .validate_col_value(col_value)
  # browser()
  res <-
    gtsummary::tbl_stack(
      list(...)
    )  %>% 
    gtsummary::as_gt() %>% 
    gt::tab_header(
      subtitle = subtitle,
      title = gt::md('EPA breakdown')
    )
  res
  
  if(is.null(suffix)) {
    suffix <- ''
  } else {
    suffix <- sprintf('%s%s', sep, suffix)
  }

  gt::gtsave(res, filename = file.path(get_bdb_dir_figs(), sprintf('tab_t_test_%s_%s%s.png', col_value, col, suffix)))
  res
}

.save_t_test_tabs <- function(data, col = 'is_target_picked', col_value = c('epa', 'n'), subtitle, suffix, ...) {
  
  .validate_col_trt(col)
  .validate_col_value(col_value)
  
  if(col == 'is_target_picked') {
    col_other <- 'has_same_defender'
    col_other_pretty <- 'Same Defender'
  } else {
    col_other <- 'is_target_picked'
    col_other_pretty <- 'Target Picked'
  }
  col_other_sym <- col_other %>% sym()
  col_value_pretty <- col_value %>% toupper()

  if(col_value == 'epa') {
    data['.value'] <- data$epa
  } else {
    data$.value <- 1L
  }

  datay <- 
    data %>%
    filter(!!col_other_sym %>% str_detect('Y$')) 
  
  datan <-
    data %>%
    filter(!!col_other_sym %>% str_detect('N$'))
  
  dataxy <-
    data %>% 
    filter(is_pass_successful %>% str_detect('Y$'))
  
  dataxn <-
    data %>% 
    filter(is_pass_successful %>% str_detect('N$'))
  
  datayy <-
    datay %>% 
    filter(is_pass_successful %>% str_detect('Y$'))
  
  datayn <-
    datay %>% 
    filter(is_pass_successful %>% str_detect('N$'))
  
  datany <-
    datan %>% 
    filter(is_pass_successful %>% str_detect('Y$'))
  
  datann <-
    datan %>% 
    filter(is_pass_successful %>% str_detect('N$'))
  
  t <- data %>% .f_col_gtsummary(col, !!sym(sprintf('%s', col_value_pretty)) := .value, col_value = col_value)
  ty <- datay %>% .f_col_gtsummary(col, !!sym(sprintf('%s | %s? Y', col_value_pretty, col_other_pretty)) := .value, col_value = col_value)
  tn <- datan %>% .f_col_gtsummary(col, !!sym(sprintf('%s | %s? N', col_value_pretty, col_other_pretty)) := .value, col_value = col_value)
  txy <- dataxy %>% .f_col_gtsummary(col, !!sym(sprintf('%s | Pass Successful? Y', col_value_pretty)) := .value, col_value = col_value)
  txn <- dataxn %>% .f_col_gtsummary(col, !!sym(sprintf('%s | Pass Successful? N', col_value_pretty)) := .value, col_value = col_value)
  
  tyy <-
    datayy %>% 
    .f_col_gtsummary(col, !!sym(sprintf('%s | Pass Successful? Y & %s? Y', col_value_pretty, col_other_pretty)) := .value, col_value = col_value)
  
  tyn <-
    datayn %>% 
    .f_col_gtsummary(col, !!sym(sprintf('%s | Pass Successful? Y & %s? N', col_value_pretty, col_other_pretty)) := .value, col_value = col_value)
  
  tny <-
    datany %>% 
    .f_col_gtsummary(col, !!sym(sprintf('%s | Pass Successful? N & %s? Y', col_value_pretty, col_other_pretty)) := .value, col_value = col_value)
  
  tnn <-
    datann %>% 
    .f_col_gtsummary(col, !!sym(sprintf('%s | Pass Successful? N & %s? N', col_value_pretty, col_other_pretty)) := .value, col_value = col_value)
  
  .postprocess_t_test_tabs(t, ty, txy, tyy, tyn, tn, txn, tny, tnn, ..., col_value = col_value, col = col, subtitle = subtitle, suffix = suffix)
}

do_save_t_test_tabs <- function(features, subtitle, suffix) {
  features_simple <- 
    features %>% 
    .simplify_pick_features()
  features_simple
  
  cols_trt <- .get_valid_col_trt()
  cols_value <- .get_valid_col_value()
  
  crossing(
    col = cols_trt,
    col_value = cols_value
  ) %>% 
    mutate(
      res = 
        map2(
          col, col_value, 
          ~.save_t_test_tabs(
            features_simple, 
            col = ..1, 
            col_value = ..2, 
            subtitle = subtitle, 
            suffix = suffix
          )
        )
    )
  features_simple
}

features_simple <- 
  features %>% 
  do_save_t_test_tabs(
    subtitle = 'Before matching',
    suffix = 'unadjusted'
  )

features_match_is_target_picked_simple <- 
  features_match_is_target_picked %>% 
  do_save_t_test_tabs(
    subtitle = 'After matching for targeted pick plays',
    suffix = 'adjusted_is_target_picked'
  )

features_match_has_same_defender_simple <- 
  features_match_has_same_defender %>% 
  do_save_t_test_tabs(
    subtitle = 'After matching for targeted defender coverage', 
    suffix = 'adjusted_has_same_defender'
  )

# non gtsummary offensive pick play t tests----
# .f_agg_epa <- function(data) {
#   
#   data %>% 
#     group_by(is_target_picked, is_pass_successful) %>% 
#     summarize(across(c(epa), list(median = median, q25 = ~quantile(.x, 0.25), q75 = ~quantile(.x, 0.75)), .names = '{fn}')) %>% 
#     ungroup() %>% 
#     mutate(
#       value = sprintf('%.02f (%.02f, %.02f)', median, q25, q75)
#     )
# }
# 
# .f_agg_n <- function(data) {
#   
#   data %>% 
#     group_by(is_target_picked, is_pass_successful) %>% 
#     summarize(n = n()) %>% 
#     ungroup() %>% 
#     group_by(is_target_picked) %>% 
#     mutate(total = sum(n), frac = n / total) %>% 
#     ungroup() %>% 
#     mutate(
#       # is_pass_successful = sprintf('%s (%s)', is_pass_successful, scales::comma(total)),
#       value = sprintf('%s (%s)', scales::comma(n), scales::percent(frac, accuracy = 0.1))
#     )
# }
# 
# .f_pivot_agg <- function(data) {
#   data %>% 
#     select(is_target_picked, is_pass_successful, value) %>% 
#     pivot_wider(names_from = is_target_picked, values_from = value) %>% 
#     mutate(
#       across(is_pass_successful, ~.x %>% str_replace_all('(.*)([YN]$)', '\\2'))
#     )
# }
# 
# .f_gt <- function(data, subtitle) {
#   
#   data %>% 
#     rename(`Pass Successful?` = is_pass_successful) %>% 
#     gt::gt() %>% 
#     gt::tab_header(
#       subtitle = subtitle,
#       title = gt::md('EPA by pass outcome')
#     ) %>% 
#     gt::cols_label(
#       # `t-test p-value` = gt::md('**t-test\np-value**'),
#       `Target Picked? N` = gt::md('**Target Picked? N**'),
#       `Target Picked? Y` = gt::md('**Target Picked? Y**')
#     )
# }
# 
# .f_save_gt <- function(gt, suffix, prefix = c('epa', 'n')) {
#   prefix <- match.arg(prefix)
#   gt %>% 
#     gt::gtsave(filename = file.path(get_bdb_dir_figs(), sprintf('tab_%s%s.png', prefix, suffix)))
# }
# 
# do_save_epa_tabs <- function(data, subtitle = NULL, suffix = NULL, sep = '_') {
# 
#   tab_epa <- 
#     data %>% 
#     .f_agg_epa() %>% 
#     .f_pivot_agg()
#   tab_epa
#   
#   .t_test_target <- function(cnd = c('Y', 'N')) {
#     t.test(epa ~ is_target_picked, data = data %>% filter(is_pass_successful %>% str_detect(sprintf('%s$', cnd))))
#   }
# 
#   t_target_y <- .t_test_target('Y')
#   t_target_n <- .t_test_target('N')
# 
#   tab_epa['t-test p-value'] <- 
#     c(t_target_n$p.value, t_target_y$p.value) %>% 
#     sprintf('%0.03f', .)
#   
#   tab_n <- data %>% .f_agg_n() %>% .f_pivot_agg()
#   
#   if(is.null(suffix)) {
#     suffix <- ''
#   } else {
#     suffix <- sprintf('%s%s', sep, suffix)
#   }
#   tab_epa %>% .f_gt(subtitle = subtitle) %>% .f_save_gt(suffix = suffix, prefix = 'epa')
#   tab_n %>% .f_gt(subtitle = subtitle) %>% .f_save_gt(suffix = suffix, prefix = 'n')
#   invisible()
# }
# 
# features_simple %>%
#   do_save_epa_tabs(suffix = 'unadjusted_nosubtitle')
# 
# features_simple %>% 
#   do_save_epa_tabs(subtitle = 'Before matching', suffix = 'unadjusted')
# 
# features_match_is_target_picked_simple %>% 
#   do_save_epa_tabs(
#     subtitle = 'After matching for targeted pick plays', 
#     suffix = 'adjusted_is_target_picked'
#   )
# 
# features_match_has_same_defender_simple %>% 
#   do_save_epa_tabs(
#     subtitle = 'After matching for targeted defender coverage', 
#     suffix = 'adjusted_has_same_defender'
#   )

