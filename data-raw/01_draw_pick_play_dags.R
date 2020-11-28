
library(tidyverse)
library(ggdag)

pick_play_dag <-
  ggdag::dagify(
    epa ~ is_pick_play + epa_predictors + defensive_skill,
    is_pick_play ~ epa_predictors,
    exposure = 'is_pick_play',
    outcome = 'epa',
    labels = c(
      'is_pick_play' = 'Pick Play',
      'epa' = 'EPA',
      # 'wp' = 'Win Probability'
      'epa_predictors' = 'EPA Predictors',
      'defensive_skill' = 'Defensive Skill'
    )
  )
theme_set(ggdag::theme_dag())
pick_play_dag %>% ggdag::ggdag()
pick_play_dag %>% ggdag::ggdag_paths(text = FALSE, use_labels = 'label', shadow = TRUE)
pick_play_dag %>% ggdag::ggdag_adjustment_set(text = FALSE, use_labels = 'label', shadow = TRUE)
pick_play_dag %>% ggdag::dag_paths()
pick_play_epa_dag <- dagitty::dagitty()

pick_play_dag %>%
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

pick_play_dag %>%
  tidy_dagitty() %>%
  dag_paths()
pick_play_dag %>%
  ggdag_paths()
