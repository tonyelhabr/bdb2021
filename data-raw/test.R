
library(tidyverse)
# data('closest_defenders')
# plot_play(game_id = 2018091001, play_id = 1970)

positions <- import_positions()
data('personnel_and_rushers', package = 'bdb2021')
week <- 1
tracking <- week %>% import_tracking(standardize = FALSE)
tracking
meta <- tibble(game_id = 2018091001, play_id = 1970)
# tracking <- tracking %>% inner_join(meta)
# tracking
qb <- tracking %>% dplyr::filter(position == 'QB')
tracking <- tracking %>% dplyr::filter(position != 'QB')
tracking <-
  tracking %>%
  dplyr::inner_join(
    qb %>%
      dplyr::select(.data$game_id, .data$play_id, .data$frame_id, qb_x = .data$x, qb_y = .data$y, qb_s = .data$s, qb_o = .data$o),
    by = c('frame_id', 'game_id', 'play_id')
  )
tracking

x_max <- 120
y_max <- 160 / 3
tracking <-
  tracking %>%
  dplyr::mutate(
    dplyr::across(c(.data$x, .data$ball_x, .data$qb_x, .data$los), ~ dplyr::if_else(.data$play_direction == 'left', !!x_max - .x, .x)),
    # Standardizing the x direction based on the los is best for doing general analysis,
    # but perhaps not for plotting.
    # across(c(x, ball_x), ~.x - los),
    dplyr::across(c(.data$y, .data$ball_y, .data$qb_y), ~ dplyr::if_else(.data$play_direction == 'left', !!y_max - .x, .x))
  )

at <- 'end_routes'
tracking_clipped <- tracking %>% clip_tracking_at_events(at = at)

frames <-
  tracking_clipped %>%
  filter_notable_events()
frames

personnel_and_rushers_week <-
  personnel_and_rushers %>%
  dplyr::filter(.data$week == !!week) %>%
  dplyr::filter(.data$n_rusher > 0L) %>%
  dplyr::mutate(rushers = purrr::map(.data$rushers, ~dplyr::select(.x, .data$nfl_id, .data$idx_closest_to_ball))) %>%
  dplyr::select(.data$game_id, .data$play_id, .data$n_rusher, .data$rushers)
personnel_and_rushers_week


.join_and_filter <- function(x1, x2, side = c('o', 'd')) {
  side <- match.arg(side)
  side_upp <- side %>% toupper()
  side_low <- side %>% tolower()
  col_x <- sprintf('x_%s', side_low)
  col_y <- sprintf('y_%s', side_low)
  col_dist <- sprintf('dist_%s', side_low)
  col_dist_ball <- sprintf('dist_ball_%s', side_low)
  col_dist_qb <- sprintf('dist_qb_%s', side_low)
  col_x_sym <- col_x %>% sym()
  col_y_sym <- col_y %>% sym()
  col_dist_sym <- col_dist %>% sym()
  col_dist_ball_sym <- col_dist_ball %>% sym()
  col_dist_qb_sym <- col_dist_qb %>% sym()

  x1 %>%
    left_join(
      x2 %>%
        filter(side == !!side_upp) %>%
        select(game_id, play_id, frame_id, nfl_id, x, y) %>%
        # Add `_o` or `_d` suffix to variables that aren't being used to join.
        rename_with(~sprintf('%s_%s', .x, side_low), -c(game_id, play_id, frame_id)),
      by = c('game_id', 'play_id', 'frame_id')
    ) %>%
    # Add a temporary distance column to filter down to other player who is closest.
    mutate(!!col_dist_sym := .dist(x, !!col_x_sym, y, !!col_y_sym)) %>%
    # If re-joining on defensive players, make sure not to choose the same player!
    filter(!!col_dist_sym > 0) %>%
    group_by(game_id, play_id, frame_id, nfl_id) %>%
    # Filter for just the player in `x2` who is closest to the player in `x1`.
    filter(!!col_dist_sym == min(!!col_dist_sym)) %>%
    ungroup() %>%
    # Drop the temporary column.
    # select(-dist) %>%
    mutate(
      !!col_dist_ball_sym := .dist(!!col_x_sym, ball_x, !!col_y_sym, ball_y),
      !!col_dist_qb_sym := .dist(!!col_x_sym, qb_x, !!col_y_sym, qb_y)
    ) # %>%
    # rename_with(~sprintf('%s_%s', .x, side_low), c(dist, dist_ball, dist_qb)) %>%
    # select(-one_of(c(col_x, col_y)))
}

dists <-
  tracking_clipped %>%
  filter(side == 'O') %>%
  select(
    game_id,
    play_id,
    frame_id,
    event,
    nfl_id,
    # display_name,
    # jersey_number,
    # position,
    x,
    y,
    ball_x,
    ball_y,
    qb_x,
    qb_y,
    los
  ) %>%
  mutate(
    dist_ball = .dist(x, ball_x, y, ball_y),
    dist_qb = .dist(x, qb_x, y, qb_y)
  ) %>%
  # Join and filter down to the closest offensive player.
  # .join_and_filter(tracking_clipped, 'o') %>%
  # ... to the closest defensive players.
  .join_and_filter(tracking_clipped, 'd')
dists

dists_first <-
  dists %>%
  group_by(game_id, play_id) %>%
  filter(frame_id == first(frame_id)) %>%
  ungroup() %>%
  mutate(idx_o = row_number(dist_d))
dists_first

dists <-
  dists %>%
  inner_join(dists_first %>% select(game_id, play_id, nfl_id, idx_o))
dists

dists_wide <-
  dists %>%
  # mutate(idx_d = idx_o) %>%
  select(-nfl_id) %>%
  select(-event) %>%
  # select(-ball_x, -ball_y, -qb_x, -qb_y, -los) %>%
  # select(-dist_d, -dist_ball_d, -dist_qb_d) %>%
  # select(game_id, play_id, frame_id, idx_o, x, y) %>%
  pivot_wider(
    names_from = c('idx_o'),
    values_from = c('x', 'y', 'dist_ball', 'dist_qb', 'nfl_id_d', 'dist_d', 'dist_ball_d', 'dist_qb_d')
  ) %>%
  relocate(game_id, play_id, frame_id, matches('_1'))
dists_wide

data('closest_defenders', package = 'bdb2021')
closest_defenders_unnested <-
  closest_defenders %>%
  inner_join(meta) %>%
  filter(!is_bad) %>%
  select(!is_bad) %>%
  unnest(min_distances) %>%
  filter(idx_closest == 1L) %>%
  select(-idx_closest)
closest_defenders_unnested

dists2 <-
  dists %>%
  semi_join(closest_defenders_unnested %>% select(game_id, play_id, frame_id)) %>%
  filter(frame_id == 11) %>%
  rename(nfl_id_o = nfl_id) %>%
  inner_join(
    closest_defenders_unnested %>%
      rename(nfl_id_d2 = nfl_id_d, x_d2 = x_d, y_d2 = y_d)
  ) %>%
  select(game_id, play_id, frame_id, nfl_id_o, nfl_id_d1 = nfl_id_d, nfl_id_d2, dist_d1 = dist_d, dist_d2 = dist, x_o, x_d1 = x_d, x_d2, y_o, y_d1 = y_d, y_d2)
dists2

# ----
# One strange source of bad data is play_id-frame_id combos where there is just 1 player.
frames_n <-
  frames %>%
  dplyr::count(.data$game_id, .data$play_id, .data$event, .data$frame_id)

closest_defenders_init <-
  frames %>%
  # head(100) %>%
  dplyr::anti_join(
    frames_n %>%
      filter(.data$n == 1L),
    by = c('game_id', 'play_id', 'event', 'frame_id')
  ) %>%
  dplyr::inner_join(
    positions %>%
      dplyr::select(.data$side, .data$position_category, .data$position),
    by = c('position', 'side')
  ) %>%
  dplyr::filter(.data$position != 'QB') %>%
  dplyr::select(-.data$position_category) %>%
  dplyr::select(
    .data$game_id,
    .data$play_id,
    .data$event,
    .data$frame_id,
    .data$nfl_id,
    .data$side,
    .data$x,
    .data$y
  ) %>%
  tidyr::nest(data = c(.data$nfl_id, .data$side, .data$x, .data$y)) %>%
  dplyr::left_join(
    personnel_and_rushers_week,
    by = c('game_id', 'play_id')
  ) %>%
  dplyr::mutate(
    o = purrr::map(.data$data, ~.x %>% .select_side('o')),
    d = purrr::map(.data$data, ~.x %>% .select_side('d')),
    d = purrr::pmap(list(.data$o, .data$d, .data$rushers), .fix_d)
  ) %>%
  dplyr::select(-data)

closest_defenders <-
  closest_defenders_init %>%
  dplyr::mutate(
    min_distances = purrr::map2(
      .data$o,
      .data$d,
      ~ compute_min_distances_possibly(o = ..1, d = ..2)
    ),
    is_bad = purrr::map_lgl(.data$min_distances, is.null)
  ) %>%
  dplyr::select(
    .data$game_id,
    .data$play_id,
    .data$event,
    .data$frame_id,
    .data$is_bad,
    .data$min_distances
  )
closest_defenders
