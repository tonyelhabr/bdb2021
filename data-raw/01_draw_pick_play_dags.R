
library(tidyverse)
library(ggdag)
theme_set(ggdag::theme_dag())

dag_epa_pick_simple <-
  ggdag::dagify(
    epa ~ epa_predictors,
    epa ~ is_pick_play, #  + targeted_receiver + defensive_coverage
    is_pick_play ~ epa_predictors,
    exposure = 'is_pick_play',
    outcome = 'epa',
    labels = c(
      'epa' = 'EPA',
      'is_pick_play' = 'Is Pick Play',
      # 'wp' = 'Win Probability',
      # 'defensive_skill' = 'Defensive Skill'
      'epa_predictors' = 'EPA Predictors'
    )
  )

dag_epa_pick_complex <-
  ggdag::dagify(
    epa ~ epa_predictors,
    epa ~ player_tracking,
    epa ~ is_pick_play, #  + targeted_receiver + defensive_coverage
    is_pick_play ~ epa_predictors,
    exposure = 'is_pick_play',
    outcome = 'epa',
    labels = c(
      'epa' = 'EPA',
      'is_pick_play' = 'Is Pick Play',
      'player_tracking' = 'Player Tracking',
      # 'wp' = 'Win Probability',
      # 'defensive_skill' = 'Defensive Skill'
      'epa_predictors' = 'EPA Predictors'
    )
  )

dag_pick_simple <-
  ggdag::dagify(
    epa ~ is_target + is_lo + defensive_coverage,
    is_target ~ defensive_coverage,
    exposure = 'defensive_coverage',
    outcome = 'epa',
    labels = c(
      'epa' = 'EPA',
      'defensive_coverage' = 'Defensive Coverage (i.e. Man or Zone)',
      'is_target' = 'Targeted Receiver is Part of Pick',
      'is_lo' = 'Receiver is Under Route in Pick'
    )
  )

dag_epa_pick_simple_tidy <-
  dag_epa_pick_simple %>%
  tidy_dagitty() %>%
  node_status()
dag_epa_pick_simple_tidy

dag_epa_pick_complex_tidy <-
  dag_epa_pick_complex %>%
  tidy_dagitty() %>%
  node_status()
dag_epa_pick_complex_tidy

status_colors <-
  c(
    exposure = '#0074D9',
    outcome = '#FF4136',
    latent = 'grey50'
  )
na_color <- 'grey20'

viz_epa_pick_complex <-
  dag_epa_pick_complex_tidy %>%
  # filter(name != 'player_tracking') %>%
  ggplot() +
  aes(x = x, y = y, xend = xend, yend = yend) +
  geom_dag_edges() +
  geom_dag_point(aes(color = status)) +
  geom_dag_label_repel(
    aes(label = label, fill = status),
    seed =  42,
    color = 'white',
    fontface = 'bold'
  ) +
  scale_color_manual(values = status_colors, na.value = na_color) +
  scale_fill_manual(values = status_colors, na.value = na_color) +
  guides(color = FALSE, fill = FALSE) +
  theme_dag()
viz_epa_pick_complex




dag_epa_pick_simple %>% ggdag::ggdag()
dag_epa_pick_simple %>% ggdag::ggdag_paths(text = FALSE, use_labels = 'label', shadow = TRUE)
dag_epa_pick_simple %>% ggdag::ggdag_adjustment_set(text = FALSE, use_labels = 'label', shadow = TRUE)
dag_epa_pick_simple %>% ggdag::dag_paths()

dag_epa_pick_simple %>%
  dag_paths(paths_only = FALSE) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, col = path, alpha = path)) +
  geom_dag_edges_link(
    aes(
      edge_alpha = path,
      edge_colour = path,
      start_cap = ggraph::circle(3, 'mm'),
      end_cap = ggraph::circle(3, 'mm')
    )
  ) +
  geom_dag_point(size = 4) +
  facet_wrap(~forcats::fct_inorder(as.factor(set), ordered = TRUE)) +
  scale_alpha_manual(
    drop = FALSE,
    values = c('open path' = 1),
    na.value = .35,
    breaks = 'open path'
  ) +
  ggraph::scale_edge_alpha_manual(
    drop = FALSE,
    values = c('open path' = 1),
    na.value = .35,
    breaks = 'open path'
  ) +
  ggraph::scale_edge_colour_hue(drop = FALSE, breaks = 'open path') +
  scale_color_hue(drop = FALSE, breaks = 'open path') +
  expand_plot(
    expand_x = expansion(c(0.25, 0.25)),
    expand_y = expansion(c(0.1, 0.1))
  ) +
  theme(legend.position = 'none')

dag_epa_pick_simple %>%
  tidy_dagitty() %>%
  dag_paths()
dag_epa_pick_simple %>%
  ggdag_paths()
