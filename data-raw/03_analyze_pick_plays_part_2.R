
# setup ----
# extrafont::loadfonts(device = 'win', quiet = TRUE)
library(tidyverse)
library(bdb2021)
data('all_plays', pacakage = 'bdb2021')
pick_plays <- all_plays %>% filter(has_intersect)

.agg_plays_by <- function(data, ...) {
  data %>% 
    group_by(has_intersect, is_lo, ...) %>% 
    summarize(
      n = n(),
      across(matches('^[ew]pa'), list(sum = sum, mean = mean), na.rm = TRUE)
    ) %>% 
    ungroup()
}

.agg_all_plays_by <- partial(.agg_plays_by, data = all_plays, ... =)
.agg_pick_plays_by <- partial(.agg_plays_by, data = pick_plays, ... =)

if(FALSE) {
  
  all_plays_by_defender <-
    .agg_all_plays_by(
      is_target,
      has_same_init_defender,
      nfl_id_d_robust,
      display_name_d_robust,
      nfl_id_d_robust_init,
      display_name_d_robust_init
    )
  all_plays_by_defender
  
  all_plays_by_defender <-
    all_plays_by_defender %>%
    group_by(
      nfl_id = nfl_id_d_robust,
      display_name = display_name_d_robust,
      is_target,
      has_intersect,
      has_same_init_defender
    ) %>%
    summarize(
      n = sum(n),
      across(matches('^[ew]pa.*sum$'), sum, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    group_by(nfl_id, display_name) %>%
    mutate(
      total = sum(n)
    ) %>%
    ungroup() %>%
    mutate(frac = n / total) %>% 
    arrange(desc(n))
  all_plays_by_defender
  
  # Have to re-adjust total numbers to just the filtering criteria.
  all_defenders_intersect_adj <-
    all_plays_by_defender %>% 
    filter(is_target) %>% 
    group_by(nfl_id, display_name, is_target) %>% 
    mutate(
      total = sum(n)
    ) %>% 
    ungroup() %>%
    mutate(frac = n / total) %>% 
    pivot_wider(
      names_from = c(has_intersect),
      values_from = c(n, frac, matches('^[ew]pa')),
      values_fill = list(n = 0L, 0)
    ) %>% 
    filter(total >= 10)
  all_defenders_intersect_adj
  
  defenders_intersect_adj_top_epa_t <-
    bind_rows(
      all_defenders_intersect_adj %>% 
        arrange(epa_sum_TRUE) %>% 
        head(5) %>%
        mutate(top = TRUE),
      all_defenders_intersect_adj %>% 
        arrange(-epa_sum_TRUE) %>% 
        head(5) %>% 
        mutate(top = FALSE)
    ) %>% 
    select(nfl_id, top)
  defenders_intersect_adj_top_epa_t
  
  defenders_intersect_adj_top_epa_f <-
    bind_rows(
      all_defenders_intersect_adj %>% 
        anti_join(defenders_intersect_adj_top_epa_t) %>% 
        arrange(epa_sum_FALSE) %>% 
        head(5) %>%
        mutate(top = TRUE),
      all_defenders_intersect_adj %>% 
        anti_join(defenders_intersect_adj_top_epa_t) %>% 
        arrange(-epa_sum_FALSE) %>% 
        head(5) %>% 
        mutate(top = FALSE)
    ) %>% 
    select(nfl_id, top)
  defenders_intersect_adj_top_epa_f
  
  defenders_intersect_adj_top_epa <-
    bind_rows(
      defenders_intersect_adj_top_epa_t %>% 
        mutate(pick = TRUE),
      defenders_intersect_adj_top_epa_f %>% 
        mutate(pick = FALSE)
    )
  defenders_intersect_adj_top_epa
  
  pts <- function (x) {
    as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
  }
  
  .common_defenders_intersect_adj_layers <- function(...) {
    list(
      ...,
      aes(y = epa_sum_FALSE, x = epa_sum_TRUE),
      geom_abline(
        data = tibble(intercept = seq(-30, 30, by = 10), slope = -1),
        aes(intercept = intercept, slope = slope),
        linetype = 2
      ),
      geom_point(
        aes(size = total), alpha = 0.2
      ),
      ggrepel::geom_text_repel(
        data = . %>% semi_join(defenders_intersect_adj_top_epa %>% filter(pick, top)),
        aes(label = display_name),
        show.legend = FALSE,
        family = 'Karla',
        color = 'dodgerblue'
      )
    )
  }
  viz_defenders_intersect_adj <-
    all_defenders_intersect_adj %>% 
    .common_defenders_intersect_adj_layers() +
    ggrepel::geom_text_repel(
      data = . %>% semi_join(defenders_intersect_adj_top_epa %>% filter(pick, top)),
      aes(label = display_name),
      show.legend = FALSE,
      family = 'Karla',
      color = 'dodgerblue'
    ) +
    geom_point(
      data = . %>% semi_join(defenders_intersect_adj_top_epa %>% filter(pick, !top)),
      aes(size = total),
      show.legend = FALSE,
      color = 'darkorange'
    ) +
    ggrepel::geom_text_repel(
      data = . %>% semi_join(defenders_intersect_adj_top_epa %>% filter(pick, !top)),
      aes(label = display_name),
      show.legend = FALSE,
      family = 'Karla',
      color = 'darkorange'
    ) +
    geom_point(
      data = . %>% semi_join(defenders_intersect_adj_top_epa %>% filter(!pick, top)),
      aes(size = total),
      show.legend = FALSE,
      color = 'indianred'
    ) +
    ggrepel::geom_text_repel(
      data = . %>% semi_join(defenders_intersect_adj_top_epa %>% filter(!pick, top)),
      aes(label = display_name),
      show.legend = FALSE,
      family = 'Karla',
      color = 'indianred'
    ) +
    geom_point(
      data = . %>% semi_join(defenders_intersect_adj_top_epa %>% filter(!pick, !top)),
      aes(size = total),
      show.legend = FALSE,
      color = 'forestgreen'
    ) +
    ggrepel::geom_text_repel(
      data = . %>% semi_join(defenders_intersect_adj_top_epa %>% filter(!pick, !top)),
      aes(label = display_name),
      show.legend = FALSE,
      family = 'Karla',
      color = 'forestgreen'
    ) +
    geom_text(
      aes(x = 5.1, y = 45, label = 'Best on pick plays'), color = 'dodgerblue', size = pts(14), hjust = 0
    ) +
    geom_text(
      aes(x = 5.1, y = 42, label = 'Worst on pick plays'), color = 'darkorange', size = pts(14), hjust = 0
    ) +
    geom_text(
      aes(x = 5.1, y = 39, label = 'Best on non-pick plays'), color = 'indianred', size = pts(14), hjust = 0
    ) +
    geom_text(
      aes(x = 5.1, y = 36, label = 'Worst on non-pick plays'), color = 'forestgreen', size = pts(14), hjust = 0
    ) +
    guides(
      size = guide_legend(title = '# of total plays', override.aes = list(alpha = 1))
    ) +
    # coord_equal(xlim = c(-30, 30)) +
    theme(
      plot.caption = element_text(size = 10),
      legend.position = 'top'
    ) +
    labs(
      title = 'Aggreggate EPA when covering targeted receiver',
      caption = 'Minimum of 10 pick plays covered.\nPlot does not differentiate based on type of coverage (e.g. man). Defender at time of thow is used.',
      y = 'EPA on non-pick plays',
      x = 'EPA on pick plays'
    )
  viz_defenders_intersect_adj
  do_save_plot(viz_defenders_intersect_adj)
  
  # Do the same thing, but splitting out by type of coverage (`has_same_init_defender`)
  defenders_intersect_adj_by_coverage <-
    all_plays_by_defender %>% 
    filter(is_target) %>% 
    group_by(nfl_id, display_name, is_target, has_same_init_defender) %>% 
    mutate(
      total = sum(n)
    ) %>% 
    ungroup() %>%
    mutate(frac = n / total) %>% 
    pivot_wider(
      names_from = c(has_intersect),
      values_from = c(n, frac, matches('^[ew]pa')),
      values_fill = list(n = 0L, 0)
    ) %>% 
    filter(total >= 10)
  defenders_intersect_adj_by_coverage
  
  viz_defenders_intersect_adj_by_coverage <-
    defenders_intersect_adj_by_coverage %>% 
    ggplot() +
    .common_defenders_intersect_adj_layers() +
    ggrepel::geom_text_repel(
      data = . %>% semi_join(defenders_intersect_adj_top_epa %>% filter(pick, top)),
      aes(label = display_name),
      show.legend = FALSE,
      family = 'Karla',
      color = 'dodgerblue'
    ) +
    geom_point(
      data = . %>% semi_join(defenders_intersect_adj_top_epa %>% filter(pick, !top)),
      aes(size = total),
      show.legend = FALSE,
      color = 'darkorange'
    ) +
    ggrepel::geom_text_repel(
      data = . %>% semi_join(defenders_intersect_adj_top_epa %>% filter(pick, !top)),
      aes(label = display_name),
      show.legend = FALSE,
      family = 'Karla',
      color = 'darkorange'
    ) +
    geom_point(
      data = . %>% semi_join(defenders_intersect_adj_top_epa %>% filter(!pick, top)),
      aes(size = total),
      show.legend = FALSE,
      color = 'indianred'
    ) +
    ggrepel::geom_text_repel(
      data = . %>% semi_join(defenders_intersect_adj_top_epa %>% filter(!pick, top)),
      aes(label = display_name),
      show.legend = FALSE,
      family = 'Karla',
      color = 'indianred'
    ) +
    geom_point(
      data = . %>% semi_join(defenders_intersect_adj_top_epa %>% filter(!pick, !top)),
      aes(size = total),
      show.legend = FALSE,
      color = 'forestgreen'
    ) +
    ggrepel::geom_text_repel(
      data = . %>% semi_join(defenders_intersect_adj_top_epa %>% filter(!pick, !top)),
      aes(label = display_name),
      show.legend = FALSE,
      family = 'Karla',
      color = 'forestgreen'
    ) +
    theme(
      plot.caption = element_text(size = 10),
      legend.position = 'top'
    ) +
    labs(
      title = 'Aggreggate EPA when covering targeted receiver',
      subtitle = 'Differntiated by coverage type',
      caption = 'Minimum of 10 pick plays covered.',
      y = 'EPA on non-pick plays',
      x = 'EPA on pick plays'
    ) +
    facet_wrap(~has_same_init_defender)
  viz_defenders_intersect_adj_by_coverage
}

# by receiver ----

# all_plays_by_receiver <- .agg_all_plays_by(nfl_id, display_name)
pick_plays_by_receiver <- .agg_all_plays_by(nfl_id, display_name)

.n_top <- 20L
# .plot_picks_by_reciever_layers <- function(...) {
#   list(
#     aes(x = ..., y = display_name),
#     # geom_col(aes(fill = is_lo)),
#     scale_fill_manual(values = c(`TRUE` = 'dodgerblue', `FALSE` = 'darkorange')),
#     guides(fill = guide_legend('Is Underneath Route Runner?')),
#     theme(
#       panel.grid.major.y = element_blank(),
#       legend.text = element_text(size = 12),
#       legend.title = element_text(size = 12),
#       legend.position = 'top'
#     ),
#     labs(
#       x = '# of Plays',
#       y = NULL
#     )
#   )
# }
viz_picks_by_receiver <-
  pick_plays_by_receiver %>%
  inner_join(
    pick_plays_by_receiver %>% 
      group_by(nfl_id, display_name) %>% 
      summarize(total = sum(n)) %>% 
      ungroup() %>% 
      mutate(
        rnk = row_number(desc(total)),
      ) %>% 
      filter(rnk <= .n_top)
  ) %>% 
  mutate(
    across(display_name, ~fct_reorder(.x, -rnk))
  ) %>% 
  arrange(rnk) %>% 
  # TODO: Replace these with above function.
  ggplot() +
  aes(x = n, y = display_name) +
  geom_col(aes(fill = is_lo)) +
  scale_fill_manual(values = c(`TRUE` = 'dodgerblue', `FALSE` = 'darkorange')) +
  guides(fill = guide_legend('Is Underneath Route Runner?')) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = 'top'
  ) +
  labs(
    title = '# of Pick Route Combinations Involved With'
    x = '# of Plays',
    y = NULL
  )
viz_picks_by_receiver
do_save_plot(viz_picks_by_receiver)

pick_plays_by_receiver_target <-
  .agg_pick_plays_by(
    nfl_id = nfl_id_target, 
    display_name = display_name_target
  ) %>% 
  filter(!is.na(display_name_target)) 
pick_plays_by_receiver_target

# TODO: Don't highlight is lo in this since it's not what is being ephasized.
viz_picks_by_receiver_target <-
  pick_plays_by_receiver_target %>%
  inner_join(
    pick_plays_by_receiver_target %>% 
      group_by(nfl_id, display_name) %>% 
      summarize(total = sum(n)) %>% 
      ungroup() %>% 
      mutate(
        rnk = row_number(desc(total)),
      ) %>% 
      filter(rnk <= .n_top)
  ) %>% 
  mutate(
    across(display_name, ~fct_reorder(.x, -rnk))
  ) %>% 
  arrange(rnk) %>% 
  # TODO: Replace these with above function.
  ggplot() +
  aes(x = n, y = display_name) +
  geom_col() +
  theme(
    panel.grid.major.y = element_blank(),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = 'top'
  ) +
  labs(
    title = '# of targets When Involved In Pick Route Combination'
    x = '# of Plays',
    y = NULL
  )
viz_picks_by_receiver_target
do_save_plot(viz_picks_by_receiver_target)

pick_plays_by_receiver <-
  all_plays_by_receiver %>% 
  group_by(nfl_id, display_name, has_intersect) %>% 
  summarize(
    n = sum(n)
  ) %>% 
  ungroup() %>% 
  group_by(nfl_id, display_name) %>% 
  mutate(
    total = sum(n),
    frac = n / total
  ) %>% 
  ungroup()
pick_plays_by_receiver

viz_frac_by_receiver <-
  pick_plays_by_receiver %>% 
  # select(-total) %>% 
  pivot_wider(
    names_from = has_intersect,
    values_from = c(n, frac)
  ) %>% 
  ggplot() +
  aes(x = total, y = n_TRUE) +
  geom_point() +
  geom_smooth(method = 'lm', formula = formula(y ~ x + 0), se = FALSE, color = 'black', linetype = 2) +
  geom_point(
    data = 
      pick_plays_by_receiver_top,
    aes(x = total, y = n),
    color = 'red'
  ) +
  ggrepel::geom_text_repel(
    data = 
      pick_plays_by_receiver_top,
    aes(x = total, y = n, label = display_name),
    family = 'Karla',
    segment.color = 'red',
    segment.size = 0.2,
    color = 'red'
  ) +
  theme(
    plot.caption = element_text(size = 10),
  ) +
  labs(
    title = 'Relative Number of Pick Routes Ran',
    caption = 'Players with highest ratio of pick plays annotated (minimum 200 plays).\nLinear regression fit shown as dotted line',
    x = '# of plays',
    y = '# of plays Involved in pick route combination'
  )
viz_frac_by_receiver
save_plot(viz_frac_by_receiver)

# TODO
Matching::Match(
  Y = plays_w_pick_info$epa,
  Tr = plays_w_pick_info$is_pick_play,
  X = fitted(fit_pick_play_prob$fit$fit$fit),
  ties = FALSE,
  estimand = 'ATT'
)


# pick_plays_agg <-
#   all_plays %>%
#   filter(has_intersect) %>% 
#   mutate(
#     pass_complete = if_else(pass_result == 'C', TRUE, FALSE),
#     across(target_is_intersect, ~.x %>% as.logical() ),
#     across(target_is_intersect, ~if_else(is.na(.x), FALSE, .x))
#   ) %>%
#   # filter(has_same_init_defender, target_is_intersect, is_lo) %>%
#   group_by(has_intersect, has_same_init_defender, target_is_intersect, is_lo, pass_complete) %>%
#   summarize(
#     n = n(),
#     across(c(epa, wpa_nflfastr, epa_nflfastr), mean, na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   group_by(has_same_init_defender, target_is_intersect, is_lo) %>%
#   mutate(
#     frac = n / sum(n)
#   ) %>%
#   ungroup()
# pick_plays_agg

# Pick an example play from here
pick_plays_meta_viz <-
  pick_plays %>% 
  mutate(pass_complete = if_else(pass_result == 'C', TRUE, FALSE)) %>%
  filter(!is.na(target_is_intersect)) %>%
  group_by(sec_intersect, pass_complete, is_lo, target_is_intersect, has_same_init_defender) %>%
  mutate(prnk = percent_rank(epa)) %>%
  filter(prnk == min(prnk) | prnk == max(prnk)) %>%
  ungroup() %>%
  mutate(high_epa = if_else(prnk == 1, TRUE, FALSE)) %>%
  filter(high_epa == pass_complete) %>%
  arrange(sec_intersect, pass_complete, is_lo, target_is_intersect) %>%
  filter(is_lo) %>%
  inner_join(plays %>% select(game_id, play_id, yards_gained = play_result)) %>% 
  mutate(
    lab = glue::glue('Pick between {display_name} ({jersey_number}, {position}) and {display_name_intersect} ({jersey_number_intersect}, {position_intersect}) between {sec_intersect-0.5} and {sec_intersect} seconds.
                     target: {display_name_target} ({jersey_number_target}, {position_target}). Play result: {pass_result}. Yards gained: {yards_gained}.
                     BDB EPA: {scales::number(epa, accuracy = 0.01)}, nflfastR EPA: {scales::number(epa_nflfastr, accuracy = 0.01)}, nflfastR WPA: {scales::number(wpa_nflfastr, accuracy = 0.01)}'),
    path = file.path(
      bdb2021:::get_bdb_dir_figs(), 
      sprintf(
        'is_pick_play=%s-sec=%1.1f-pass_complete=%s-is_lo=%s-target_is_intersect=%s-high_epa=%s-%s-%s.png', 'Y', 
        sec_intersect, 
        ifelse(pass_complete, 'Y', 'N'), 
        ifelse(is_lo, 'Y', 'N'), 
        ifelse(target_is_intersect, 'Y', 'N'), 
        ifelse(high_epa, 'Y', 'N'), game_id, play_id)
    )
  )
pick_plays_meta_viz

primary_example_pick_plays <-
  list(
    pick_plays_meta_viz %>% 
      filter(high_epa) %>% 
      slice_max(epa, with_ties = FALSE) %>% 
      mutate(descr = 'highest_epa'),
    pick_plays_meta_viz %>% 
      filter(!high_epa) %>% 
      slice_min(epa, with_ties = FALSE) %>% 
      mutate(descr = 'lowest_epa'),
    pick_plays_meta_viz %>% 
      filter(high_epa) %>% 
      filter(sec_intersect == min(sec_intersect)) %>% 
      slice_max(epa, with_ties = FALSE) %>% 
      mutate(descr = 'y_buffer')
  ) %>% 
  reduce(bind_rows) %>% 
  mutate(
    path = file.path(dirname(path), sprintf('%s_pick_play.png', descr))
  )
primary_example_pick_plays

pick_plays_meta_viz_wo_primary <-
  pick_plays_meta_viz %>% 
  anti_join(primary_example_pick_plays %>% select(game_id, play_id))

secondary_example_pick_plays <-
  list(
    pick_plays_meta_viz_wo_primary %>% 
      filter(high_epa) %>% 
      filter(has_same_init_defender) %>% 
      slice_max(epa, with_ties = FALSE) %>% 
      mutate(descr = 'highest_epa_w_same_defender'),
    pick_plays_meta_viz_wo_primary %>% 
      filter(high_epa) %>% 
      filter(!has_same_init_defender) %>% 
      slice_max(epa, with_ties = FALSE) %>% 
      mutate(descr = 'highest_epa_w_diff_defender'),
    pick_plays_meta_viz_wo_primary %>% 
      filter(!high_epa) %>% 
      filter(has_same_init_defender) %>% 
      slice_min(epa, with_ties = FALSE) %>% 
      mutate(descr = 'lowest_epa_w_same_defender'),
    pick_plays_meta_viz_wo_primary %>% 
      filter(!high_epa) %>% 
      filter(!has_same_init_defender) %>% 
      slice_min(epa, with_ties = FALSE) %>% 
      mutate(descr = 'lowest_epa_w_diff_defender')
  ) %>% 
  reduce(bind_rows) %>% 
  mutate(
    path = file.path(dirname(path), sprintf('%s_pick_play.png', descr))
  )
secondary_example_pick_plays

res_viz <-
  # pick_plays_meta_viz %>%
  # filter(target_is_intersect & is_lo & sec >= 1 & sec <= 3) %>%
  list(
    primary_example_pick_plays,
    secondary_example_pick_plays
  ) %>% 
  reduce(bind_rows) %>% 
  # slice(3) %>% 
  mutate(
    viz = pmap(
      list(game_id, play_id, lab),
      ~animate_play(game_id = ..1, play_id = ..2, save = FALSE) +
        labs(subtitle = ..3),
    ),
    res = map2(viz, path, ~ggsave(filename = ..2, plot = ..1)) # , unit = 'in', height = 10, width = 10))
  )

# pick_play_agg <-
#   pick_plays %>%
#   # Drop the plays where the target receiver is NA.
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
