
do_plot_eda <- function(plays = import_plays()) {
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
  save_plot(viz_epa_swarm)
  
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
  save_plot(viz_intersections_after_n_sec)
}