
# setup ----
# extrafont::loadfonts(device = 'win', quiet = TRUE)
library(tidyverse)
library(bdb2021)
theme_set_and_update_bdb()
# data('receiver_intersections_adj', package = 'bdb2021')
data('personnel_and_rushers', package = 'bdb2021')
data('players_from_tracking', package = 'bdb2021')
data('routes', package = 'bdb2021')
players_from_tracking <- players_from_tracking %>% select(-week)
routes <- routes %>% select(-week)

do_save_plot <- function(...) {
  if(TRUE) {
    save_plot(...)
  }
}

# Do this NA filtering now so earlier plots don't have extra plays.
plays <- import_plays() %>% filter(!is.na(absolute_yardline_number))
pbp <- import_nflfastr_pbp()
.sec_cutoff <- 2
new_features <- import_new_features()

# start eda ----
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
    target_is_intersect = sum(target_nfl_id == nfl_id)
  ) %>%
  ungroup() %>%
  select(-week)
pick_play_meta_init

pick_play_meta_init %>% 
  left_join

plays_w_pick_info <-
  plays %>%
  left_join(
    pick_play_meta_init %>%
      # Keep the pick play to one row per play at maximum
      nest(pick_data = -c(game_id, play_id)),
    by = c('game_id', 'play_id')
  ) %>%
  left_join(
    personnel_and_rushers %>%
      select(-rushers) %>%
      rename_with(~sprintf('%s_personnel', .x), matches('^n_')) %>%
      rename(n_rusher = n_rusher_personnel),
    by = c('game_id', 'play_id')
  ) %>%
  mutate(
    is_pick_play = map_lgl(pick_data, ~!is.null(.x)),
    across(n_rusher, ~coalesce(.x, 0L)) # ,
    # # pick_data = map_if(pick_data, is.null, ~tibble())
  ) %>%
  relocate(game_id, play_id, is_pick_play, pick_data) %>%
  left_join(
    pbp %>%
      select(game_id, play_id, wp_nflfastr = wp, wpa_nflfastr = wpa, epa_nflfastr = epa),
    by = c('game_id', 'play_id')
  ) %>%
  select(-pick_data) %>%
  # mutate(across(is_pick_play, ~coalesce(.x, FALSE)))
  # Doing some mutations for the model fitting stuff.
  mutate(
    pick_play_type = if_else(is_pick_play, 'other_play', 'pick_play'),
    across(is_pick_play, ~if_else(.x, '1', '0') %>% factor()),
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
    across(c(quarter, down), factor)
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
  ) %>% 
  select(
    -one_of(
      sprintf('n_%s', c('rb', 'wr', 'te', 'dl', 'lb', 'db'))
    ), 
    -c(number_of_pass_rushers, defenders_in_the_box)
  ) %>% 
  rename_with(~str_remove(.x, '_fct'), matches('_fct'))
plays_w_pick_info

viz_pick_play_frac <-
  plays_w_pick_info %>% 
  count(tm = possession_team, is_pick_play) %>% 
  group_by(tm) %>% 
  mutate(
    frac = n / sum(n)
  ) %>% 
  ungroup() %>% 
  filter(is_pick_play == 1) %>% 
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

# Note that we have same number of NAs for these 2. It's cool that they are the same.
plays_w_pick_info %>% filter(is.na(number_of_pass_rushers))
plays_w_pick_info %>% filter(is.na(type_dropback))

# plays %>% names()
# others: 'type_dropback', 'is_defensive_pi', 'personnel_o', 'personnel_d',  'possession_team',
cols_d <-
  c(
    'quarter',
    'down',
    sprintf('n_%s', c('rb', 'wr', 'te', 'dl', 'lb', 'db')),
    'defenders_in_the_box',
    'number_of_pass_rushers'
  )
cols_c_i_added_nflfastr <- 'wp'
cols_c_i_added <- c(cols_c_i_added_nflfastr, 'pre_snap_score_diff')
# don't need  'pre_snap_visitor_score' if have home and differential
cols_c_i <-
  c(
    cols_c_i_added,
    'yards_to_go',
    'absolute_yardline_number',
    'pre_snap_home_score'
  )
cols_features <- c(cols_d, cols_c_i)

col_y <- 'is_pick_play'
fmla <-
  generate_formula(
    data = plays_w_pick_info, 
    y = col_y, 
    x_include = cols_features, 
    intercept = TRUE
  )

rec_pick_play_prob <-
  plays_w_pick_info %>%
  recipes::recipe(fmla, data = .)
rec_pick_play_prob

spec_pick_play_prob <-
  parsnip::logistic_reg() %>%
  parsnip::set_mode('classification') %>%
  parsnip::set_engine('glm')

wf_pick_play_prob_model <-
  workflows::workflow() %>%
  workflows::add_recipe(rec_pick_play_prob) %>%
  workflows::add_model(spec_pick_play_prob)

fit_pick_play_prob <-
  parsnip::fit(wf_pick_play_prob_model, plays_w_pick_info)
fit_pick_play_prob <- glm(fmla, data = plays_w_pick_info, family = stats::binomial)
pick_plays_probs <-
  fit_pick_play_prob %>% 
  broom::augment()

coefs_pick_play_prob <-
  fit_pick_play_prob %>%
  broom::tidy() %>%
  mutate(
    # across(term, ~str_replace_all(.x, '(^.*)([<>2-8]+)', '\\1_\\2')),
    across(term, ~fct_reorder(.x, estimate)),
    lo = estimate - 1.96 * std.error,
    hi = estimate + 1.96 * std.error,
    is_signif = if_else(p.value < 0.05, TRUE, FALSE)
  )

viz_pick_play_prob_coefs <-
  coefs_pick_play_prob %>%
  ggplot() +
  aes(y = term, x = estimate, color = is_signif) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), size = 1) +
  scale_color_manual(values = c(`TRUE` = 'red', `FALSE` = 'black')) +
  geom_vline(data = tibble(), aes(xintercept = 0), linetype = 2) +
  guides(color = guide_legend('Is Statistically Significant?')) +
  theme(
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = 'top' # ,
    # legend.box = element_rec()
  ) +
  labs(
    title = 'Play-Level Pick Route Logistic Regression Model Coefficients',
    y = NULL, x = 'Estimate +- 1.96 Standard Error'
  )
viz_pick_play_prob_coefs
do_save_plot(viz_pick_play_prob_coefs)

# analyze defense intersections ----
# Only want the ball snap and end rush frames, not the other event frames.
events_end_rush <- .get_events_end_rush()
features_min_init <-
  new_features %>%
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
    has_same_init_defender = if_else(nfl_id_d_robust == nfl_id_d_robust_init, TRUE, FALSE)
  ) %>%
  ungroup() %>%
  mutate(across(has_same_init_defender, ~if_else(sec == 0, NA, .x))) %>%
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
      select(game_id, play_id, pass_result, epa)
  ) %>%
  left_join(
    pbp %>%
      select(game_id, play_id, wpa_nflfastr = wpa, epa_nflfastr = epa)
  ) %>% 
  distinct()
features_lag

# Taking the one-non-snap frame of each play
features_w_pick_info <- features_lag %>% filter(sec > 0)

# # TODO: Need whether targeted or not
# features_w_pick_info %>% 
#   filter(had_intersect, is_lo) %>% 
#   group_by(has_same_init_defender, pass_result, is_lo) %>% 
#   summarize(
#     n = n(),
#     across(c(epa, wpa_nflfastr, epa_nflfastr), mean, na.rm = TRUE)
#   ) %>% 
#   ungroup() %>% 
#   group_by(has_same_init_defender, is_lo) %>% 
#   mutate(
#     frac = n / sum(n)
#   ) %>% 
#   ungroup()


# example viz data ----
# jersey_numbers <-
#   players_from_tracking %>%
#   distinct(game_id, nfl_id, display_name, position, jersey_number)

pick_plays <-
  pick_play_meta_init %>%
  # Now get `epa` numbers from `plays`.
  inner_join(
    plays %>%
      select(
        game_id,
        play_id,
        pass_result,
        epa,
        nfl_id_target = target_nfl_id
      ),
    by = c('game_id', 'play_id')
  ) %>% 
  left_join(
    pbp %>%
      select(game_id, play_id, wpa_nflfastr = wpa, epa_nflfastr = epa),
    by = c('game_id', 'play_id')
  ) %>%
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
  left_join(
    routes,
    by = c('game_id', 'play_id', 'nfl_id')
  ) %>%
  left_join(
    routes %>%
      rename_with(~sprintf('%s_intersect', .x), c(nfl_id, route)),
    by = c('game_id', 'play_id', 'nfl_id_intersect')
  ) %>%
  # There are 2 plays that don't match up. Just drop them with the inner_join
  inner_join(
    features_w_pick_info %>%
      select(
        game_id,
        play_id,
        nfl_id,
        nfl_id_intersect,
        nfl_id_d_robust,
        nfl_id_d_robust_init,
        is_lo,
        sec = sec_intersect,
        has_same_init_defender,
        def_had_intersect = had_intersect # This is more useful if we were comparing to all plays, but it's just always TRUE for pick plays
      )
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
pick_plays

pick_plays %>% 
  count(nfl_id_d_robust, display_name_d_robust, sort = TRUE)

pick_plays %>% 
  count(nfl_id_d_robust_init, display_name_d_robust_init, sort = TRUE)

pick_plays_by_receiver <-
  pick_plays %>% 
  count(nfl_id, display_name, is_lo, sort = TRUE)
pick_plays_by_receiver

plot_picks_by_receiver <- function(data) {
  data %>%
    inner_join(
      data %>% 
        group_by(nfl_id, display_name) %>% 
        summarize(total = sum(n)) %>% 
        ungroup() %>% 
        mutate(
          rnk = row_number(desc(total)),
        ) %>% 
        filter(rnk <= 20L)
    ) %>% 
    mutate(
      across(display_name, ~fct_reorder(.x, -rnk))
    ) %>% 
    arrange(rnk) %>% 
    ggplot() +
    aes(x = n, y = display_name) +
    geom_col(aes(fill = is_lo)) +
    scale_fill_manual(values = c(`TRUE` = 'blue', `FALSE` = 'grey50')) +
    guides(fill = guide_legend('Is Underneath Route Runner?')) +
    theme(
      panel.grid.major.y = element_blank(),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.position = 'top'
    ) +
    labs(
      # title = '# of Pick Route Combinations Involved With',
      x = '# of Plays',
      y = NULL
    )
}

viz_picks_by_receiver <-
  pick_plays_by_receiver %>%
  plot_picks_by_receiver() +
  labs(
    title = '# of Pick Route Combinations Involved With'
  )
do_save_plot(viz_picks_by_receiver)

pick_plays_by_receiver_target <-
  pick_plays %>% 
  # Drop the NA target rows.
  filter(!is.na(display_name_target)) %>% 
  count(nfl_id = nfl_id_target, display_name = display_name_target, is_lo, sort = TRUE)
pick_plays_by_receiver_target

viz_picks_by_receiver_target <-
  pick_plays_by_receiver_target %>%
  plot_picks_by_receiver() +
  labs(
    title = '# of Targets When Involved In Pick Route Combination'
  )
viz_picks_by_receiver_target
do_save_plot(viz_picks_by_receiver_target)

pick_plays %>% 
  count(nfl_id_d)


Matching::Match(
  Y = plays_w_pick_info$epa,
  Tr = plays_w_pick_info$is_pick_play,
  X = fitted(fit_pick_play_prob$fit$fit$fit),
  ties = FALSE,
  estimand = 'ATT'
)

pick_plays_agg <-
  pick_plays %>%
  mutate(
    pass_complete = if_else(pass_result == 'C', TRUE, FALSE),
    across(target_is_intersect, ~.x %>% as.logical() ),
    across(target_is_intersect, ~if_else(is.na(.x), FALSE, .x))
  ) %>%
  # filter(has_same_init_defender, target_is_intersect, is_lo) %>%
  group_by(has_same_init_defender, target_is_intersect, is_lo, pass_complete) %>%
  summarize(
    n = n(),
    across(c(epa, wpa_nflfastr, epa_nflfastr), mean, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(has_same_init_defender, target_is_intersect, is_lo) %>%
  mutate(
    frac = n / sum(n)
  ) %>%
  ungroup()
pick_plays_agg

pick_plays_agg %>% 
  filter(pass_complete) %>% 
  select(-frac) %>% 
  pivot_longer(
    -c(has_same_init_defender, target_is_intersect, is_lo, pass_complete)
  ) %>% 
  ggplot() +
  aes(x = has_same_init_defender, y = value) +
  geom_col() +
  facet_wrap(~name, scales = 'free')

pick_plays %>% 
  count(is_lo, target_is_intersect, has_same_init_defender)

# Pick an example play from here
pick_play_meta_viz <-
  pick_plays %>%
  mutate(pass_complete = if_else(pass_result == 'C', TRUE, FALSE)) %>%
  filter(!is.na(target_is_intersect)) %>%
  group_by(sec, pass_complete, is_lo, target_is_intersect, has_same_init_defender) %>%
  mutate(prnk = percent_rank(epa)) %>%
  filter(prnk == min(prnk) | prnk == max(prnk)) %>%
  ungroup() %>%
  mutate(high_epa = if_else(prnk == 1, TRUE, FALSE)) %>%
  filter(high_epa == pass_complete) %>%
  arrange(sec, pass_complete, is_lo, target_is_intersect) %>%
  filter(is_lo) %>%
  inner_join(plays %>% select(game_id, play_id, yards_gained = play_result)) %>% 
  mutate(
    lab = glue::glue('Pick between {display_name} ({jersey_number}, {position}) and {display_name_intersect} ({jersey_number_intersect}, {position_intersect}) between {sec-0.5} and {sec} seconds.
                     Target: {display_name_target} ({jersey_number_target}, {position_target}). Play result: {pass_result}. Yards gained: {yards_gained}.
                     BDB EPA: {scales::number(epa, accuracy = 0.01)}, nflfastR EPA: {scales::number(epa_nflfastr, accuracy = 0.01)}, nflfastR WPA: {scales::number(wpa_nflfastr, accuracy = 0.01)}'),
    path = file.path(
      .get_dir_data(), 
      sprintf(
        'is_pick_play=%s-sec=%1.1f-pass_complete=%s-is_lo=%s-target_is_intersect=%s-high_epa=%s-%s-%s.png', 'Y', 
        sec, 
        ifelse(pass_complete, 'Y', 'N'), 
        ifelse(is_lo, 'Y', 'N'), 
        ifelse(target_is_intersect, 'Y', 'N'), 
        ifelse(high_epa, 'Y', 'N'), game_id, play_id)
    )
  )
pick_play_meta_viz

example_pick_plays <-
  list(
    pick_play_meta_viz %>% 
      filter(!high_epa) %>% 
      slice_min(epa, with_ties = FALSE) %>% 
      mutate(descr = 'highest_epa'),
    pick_play_meta_viz %>% 
      filter(high_epa) %>% 
      slice_max(epa, with_ties = FALSE) %>% 
      mutate(descr = 'lowest_epa')
  ) %>% 
  reduce(bind_rows) %>% 
  mutate(
    path = file.path(.get_dir_figs(), sprintf('%s_pick_play.png', descr))
  )

res_viz <-
  # pick_play_meta_viz %>%
  # filter(target_is_intersect & is_lo & sec >= 1 & sec <= 3) %>%
  example_pick_plays %>% 
  mutate(
    viz = pmap(
      list(game_id, play_id, lab),
      ~plot_play(game_id = ..1, play_id = ..2, save = FALSE) +
        labs(subtitle = ..3),
    ),
    res = map2(viz, path, ~ggsave(filename = ..2, plot = ..1, unit = 'in', height = 10, width = 10))
  )

# pick_play_agg <-
#   pick_plays %>%
#   # Drop the plays where the targeted receiver is NA.
#   # drop_na() %>%
#   # filter(!is.na(target_is_intersect)) %>%
#   mutate(across(pass_result, ~if_else(.x != 'C', 'I', .x))) %>%
#   group_by(target_is_intersect, sec, pass_result) %>%
#   summarize(
#     n = n(),
#     across(c(ends_with('_nflfastr'), epa), mean, na.rm = TRUE)
#   ) %>%
#   ungroup() # %>%
# pick_play_agg
