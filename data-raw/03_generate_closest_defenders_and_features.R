
library(tidyverse)
positions <- import_positions()
plays <- import_plays(drop_bad = TRUE)
data('personnel_and_rushers', package = 'bdb2021')
data('players_from_tracking', package = 'bdb2021')
data('routes', package = 'bdb2021')
routes <- routes %>% select(-week)
data('receiver_intersections_adj', package = 'bdb2021')

.compute_min_distances_possibly <- purrr::possibly(compute_min_distances, otherwise = NULL)

.select_side <- function(data, side = c('O', 'D'), ...) {
  side <- match.arg(side)
  data %>%
    filter(side == !!side) %>%
    select(
      .data$game_id,
      .data$play_id,
      .data$frame_id,
      .data$event,
      .data$nfl_id,
      .data$x,
      .data$y,
      .data$s,
      .data$a,
      .data$dis,
      .data$o,
      .data$dir,
      ...
    )
}

do_generate_features <-
  function(week = 1L,
           n_halfseconds = 7L,
           overwrite_features = TRUE,
           overwrite_min_dists_robust = FALSE,
           overwrite_min_dists_target = TRUE,
           ...) {
    
  path_min_dists <- file.path(get_bdb_dir_data(), sprintf('min_dists_robust_week%d.parquet', week))
  do_min_dists <- !file.exists(path_min_dists) | overwrite_min_dists_robust

  path_min_dists <- file.path(get_bdb_dir_data(), sprintf('min_dists_naive_week%d.parquet', week))
  do_min_dists_target <- !file.exists(path_min_dists) | overwrite_min_dists_target

  path_features <- file.path(get_bdb_dir_data(), sprintf('features_week%d.parquet', week))
  # browser()
  do_features <- !file.exists(path_features) | overwrite_features
  if(!do_features) {
    .display_info('Importing features for week {week} at {Sys.time()} and not re-generating.')
    features <- path_features %>% arrow::read_parquet()
    return(features)
  }

  if(!do_min_dists) {
    .display_info('Importing pre-calculated min dist data for {week} at {Sys.time()}.')
    min_dists_robust <- path_min_dists %>% arrow::read_parquet()
  }

  .display_info('Generating features for week {week} at {Sys.time()}.')
  tracking <- week %>% import_tracking(standardize = FALSE)
  qb <- tracking %>% dplyr::filter(position == 'QB')
  tracking <- tracking %>% dplyr::filter(position != 'QB')

  tracking <-
    tracking %>%
    dplyr::inner_join(
      qb %>%
        dplyr::select(
          .data$game_id,
          .data$play_id,
          .data$frame_id,
          qb_x = .data$x,
          qb_y = .data$y,
          qb_s = .data$s,
          qb_a = .data$a,
          qb_dis = .data$dis,
          qb_o = .data$o,
          qb_dir = .data$dir
        ),
      by = c('frame_id', 'game_id', 'play_id')
    ) %>%
    dplyr::left_join(
      plays %>%
        dplyr::select(.data$game_id, .data$play_id, .data$yards_to_go),
      by = c('game_id', 'play_id')
    ) %>%
    dplyr::mutate(fd = .data$los + dplyr::if_else(.data$play_direction == 'left', -1, 1) * .data$yards_to_go)
  tracking

  x_max <- 120
  y_max <- 160 / 3
  tracking <-
    tracking %>%
    dplyr::mutate(
      dplyr::across(c(.data$x, .data$ball_x, .data$qb_x, .data$los), ~ dplyr::if_else(.data$play_direction == 'left', !!x_max - .x, .x)),
      dplyr::across(c(.data$x, .data$ball_x, .data$qb_x), ~.x - los),
      dplyr::across(c(.data$y, .data$ball_y, .data$qb_y), ~ dplyr::if_else(.data$play_direction == 'left', !!y_max - .x, .x))
    )

  at <- 'throw'
  tracking_clipped <- tracking %>% clip_tracking_at_events(at = at)
  tracking_at_events <- tracking %>% filter_notable_events()

  snap_frames <- tracking %>% dplyr::filter(.data$event == 'ball_snap')

  snap_frame_ids <-
    snap_frames %>%
    dplyr::distinct(.data$game_id, .data$play_id, .data$frame_id)

  frames_halfseconds <-
    snap_frame_ids %>%
    dplyr::mutate(n_halfseconds = !!n_halfseconds) %>%
    tidyr::uncount(.data$n_halfseconds) %>%
    dplyr::select(-.data$n_halfseconds) %>%
    dplyr::group_by(.data$game_id, .data$play_id) %>%
    # Create half seconds.
    dplyr::mutate(
      sec = 0.5 * (dplyr::row_number() - 1L)
    ) %>%
    dplyr::ungroup() %>%
    # Technically this should be an integer, but we don't really need to coerce it.
    dplyr::mutate(
      frame_id = .data$frame_id + .data$sec * 10,
      event = sprintf('%1.1f sec', .data$sec)
    ) %>%
    dplyr::inner_join(
      tracking_clipped %>%
        dplyr::select(-.data$event),
      by = c('frame_id', 'game_id', 'play_id')
    )
  frames_halfseconds

  frames_events <-
    tracking_at_events %>%
    dplyr::filter(event != 'ball_snap') %>%
    dplyr::left_join(
      snap_frame_ids %>%
        dplyr::rename(frame_id_snap = .data$frame_id),
      by = c('game_id', 'play_id')
    ) %>%
    dplyr::mutate(sec = 0.1 * (.data$frame_id - .data$frame_id_snap)) %>%
    dplyr::select(-.data$frame_id_snap)
  frames_events

  frames <-
    list(frames_halfseconds, frames_events) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    # Easy way to get rid of some weird duplicates
    dplyr::distinct()

  personnel_and_rushers_week <-
    personnel_and_rushers %>%
    dplyr::filter(.data$week == !!week) %>%
    dplyr::filter(.data$n_rusher > 0L) %>%
    dplyr::mutate(rushers = purrr::map(.data$rushers, ~dplyr::select(.x, .data$nfl_id, .data$idx_closest_to_ball))) %>%
    dplyr::select(.data$game_id, .data$play_id, .data$n_rusher, .data$rushers)
  personnel_and_rushers_week

  # One strange source of bad data is play_id-frame_id combos where there is just 1 player.
  frames_n <-
    frames %>%
    dplyr::count(.data$game_id, .data$play_id, .data$frame_id, .data$event)

  frames_first <-
    frames %>%
    # dplyr::filter(.data$side == 'O') %>%
    dplyr::group_by(.data$game_id, .data$play_id, .data$side) %>%
    dplyr::filter(.data$frame_id == min(.data$frame_id)) %>%
    dplyr::ungroup()

  frames_first_n <-
    frames_first %>%
    dplyr::count(.data$game_id, .data$play_id, .data$side)

  frames_first_idx_o <-
    frames_first %>%
    # add_side_cols() %>%
    # drop_ineligible_pick_route_frames() %>%
    dplyr::filter(.data$side == 'O') %>%
    dplyr::group_by(.data$game_id, .data$play_id) %>%
    dplyr::mutate(
      dist_ball = .dist(.data$x, .data$ball_x, .data$y, .data$ball_y),
      idx_o = dplyr::row_number(.data$dist_ball)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$game_id, .data$play_id, .data$nfl_id, .data$idx_o)
  frames_first_idx_o

  frames_target <-
    frames_first_idx_o %>%
    dplyr::left_join(
      plays %>%
        dplyr::select(
          .data$game_id,
          .data$play_id,
          .data$target_nfl_id
        ),
      by = c('game_id', 'play_id')
    ) %>%
    dplyr::mutate(
      is_target = dplyr::if_else(.data$nfl_id == .data$target_nfl_id, 1L, 0L)
    )
  frames_target

  if(do_min_dists) {

    frames_target_idx_o <-
      frames_target %>%
      dplyr::filter(.data$nfl_id == .data$target_nfl_id) %>%
      dplyr::select(.data$game_id, .data$play_id, idx_o_target = .data$idx_o)
    frames_target_idx_o

    min_dists_nested_init <-
      frames %>%
      dplyr::select(
        .data$game_id,
        .data$play_id,
        .data$frame_id,
        .data$event,
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
        o = purrr::map(.data$data, ~.x %>% .filter_side('o')),
        d = purrr::map(.data$data, ~.x %>% .filter_side('d')),
        d = purrr::pmap(list(.data$o, .data$d, .data$rushers), .fix_d)
      ) %>%
      dplyr::select(-data)
    min_dists_nested_init
    
    min_dists_nested_robust <-
      min_dists_nested_init %>%
      dplyr::mutate(
        min_dists_robust = purrr::map2(
          .data$o,
          .data$d,
          ~.compute_min_distances_possibly(o = ..1, d = ..2, one_pass = TRUE)
        ),
        is_bad = purrr::map_lgl(.data$min_dists_robust, is.null)
      ) %>%
      dplyr::filter(!.data$is_bad) %>%
      dplyr::select(
        .data$game_id,
        .data$play_id,
        .data$frame_id,
        .data$event,
        .data$min_dists_robust
      )
    min_dists_nested_robust
    beepr::beep(3)

    min_dists_robust <-
      min_dists_nested_robust %>%
      tidyr::unnest(.data$min_dists_robust) %>%
      dplyr::select(
        .data$game_id,
        .data$play_id,
        .data$frame_id,
        .data$event,
        nfl_id = .data$nfl_id_o,
        .data$nfl_id_d,
        dist_d = .data$dist
      ) %>%
      dplyr::left_join(
        frames_target,
        by = c('game_id', 'play_id', 'nfl_id')
      ) %>%
      dplyr::left_join(
        frames_target_idx_o,
        by = c('game_id', 'play_id')
      )
    min_dists_robust %>% arrow::write_parquet(path_min_dists)

  }

  frames_o <-
    frames %>%
    .select_side(side = 'O')

  frames_o_renamed <-
    frames_o %>%
    rename_with(~sprintf('%s_%s', .x, 'o'), -c(.data$game_id, .data$play_id, .data$frame_id, .data$event))

  min_dists_naive_o <-
    frames_o %>%
    dplyr::left_join(
      frames_o_renamed,
      by = c('game_id', 'play_id', 'frame_id', 'event')
    ) %>%
    dplyr::mutate(dist_o = .dist(.data$x, .data$x_o, .data$y, .data$y_o)) %>%
    dplyr::filter(.data$dist_o > 0) %>%
    dplyr::group_by(.data$game_id, .data$play_id, .data$frame_id, .data$event, .data$nfl_id) %>%
    dplyr::filter(.data$dist_o == min(.data$dist_o)) %>%
    dplyr::ungroup()

  frames_qb <-
    frames %>%
    dplyr::distinct(
      .data$game_id,
      .data$play_id,
      .data$frame_id,
      .data$event,
      .data$qb_x,
      .data$qb_y,
      .data$qb_s,
      .data$qb_a,
      .data$qb_dis,
      .data$qb_o,
      .data$qb_dir
    )

  frames_d <-
    frames %>%
    .select_side(side = 'D')

  frames_d_renamed <-
    frames_d %>%
    rename_with(~sprintf('%s_%s', .x, 'd'), -c(game_id, play_id, frame_id, event))

  min_dists_naive_qb <-
    frames_qb %>%
    dplyr::left_join(
      frames_d %>%
        rename_with(~sprintf('%s_%s', .x, 'rusher'), -c(game_id, play_id, frame_id, event)),
      by = c('game_id', 'play_id', 'frame_id', 'event')
    ) %>%
    dplyr::mutate(dist_rusher = .dist(.data$qb_x, .data$x_rusher, .data$qb_y, .data$y_rusher)) %>%
    dplyr::group_by(.data$game_id, .data$play_id, .data$frame_id, .data$event) %>%
    dplyr::filter(.data$dist_rusher == min(.data$dist_rusher)) %>%
    dplyr::ungroup()

  min_dists_naive_d_init <-
    frames_o %>%
    dplyr::left_join(
      frames_d %>%
        dplyr::rename_with(~sprintf('%s_%s', .x, 'd'), -c(.data$game_id, .data$play_id, .data$frame_id, .data$event)),
      by = c('game_id', 'play_id', 'frame_id', 'event')
    ) %>%
    dplyr::mutate(dist_d = .dist(.data$x, .data$x_d, .data$y, .data$y_d)) %>%
    dplyr::group_by(.data$game_id, .data$play_id, .data$frame_id, .data$event, .data$nfl_id) %>%
    dplyr::mutate(idx_closest = dplyr::row_number(.data$dist_d)) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      .data$game_id,
      .data$play_id,
      .data$frame_id,
      .data$event,
      .data$nfl_id,
      dplyr::matches('_d'),
      .data$idx_closest
    )

  if(do_min_dists_target) {

    min_dists_naive_od_target <-
      frames_target %>%
      dplyr::filter(is_target == 1L) %>%
      dplyr::left_join(
        frames_o,
        by = c('game_id', 'play_id', 'nfl_id')
      ) %>%
      dplyr::left_join(
        frames_d_renamed,
        by = c('game_id', 'play_id', 'frame_id', 'event')
      ) %>%
      dplyr::mutate(dist_d = .dist(.data$x, .data$x_d, .data$y, .data$y_d)) %>%
      dplyr::group_by(.data$game_id, .data$play_id, .data$frame_id, .data$event, .data$nfl_id) %>%
      dplyr::filter(.data$dist_d == min(.data$dist_d)) %>%
      dplyr::ungroup() %>% 
      dplyr::left_join(
        frames_o_renamed,
        by = c('game_id', 'play_id', 'frame_id', 'event')
      ) %>%
      dplyr::mutate(dist_o = .dist(.data$x, .data$x_o, .data$y, .data$y_o)) %>%
      dplyr::filter(.data$dist_o > 0) %>%
      dplyr::group_by(.data$game_id, .data$play_id, .data$frame_id, .data$event, .data$nfl_id) %>%
      dplyr::filter(.data$dist_o == min(.data$dist_o)) %>%
      dplyr::ungroup() %>% 
      dplyr::select(
        .data$game_id,
        .data$play_id,
        .data$frame_id,
        .data$event,
        .data$idx_o,
        .data$nfl_id,
        .data$target_nfl_id,
        .data$is_target,
        .data$x,
        .data$y,
        .data$nfl_id_d,
        .data$x_d,
        .data$y_d,
        .data$nfl_id_o,
        .data$dist_d,
        .data$x_o,
        .data$y_o,
        .data$dist_o
      )
    
    min_dists_naive_od_target %>% arrow::write_parquet(path_min_dists %>% str_replace('_naive', '_naive_od'))
    
    # min_dists_naive_d_target <-
    #   frames_target %>%
    #   # dplyr::filter(is_target == 1L) %>%
    #   dplyr::left_join(
    #     frames_o,
    #     by = c('game_id', 'play_id', 'nfl_id')
    #   ) %>%
    #   dplyr::left_join(
    #     frames_d %>%
    #       dplyr::rename_with(~sprintf('%s_%s', .x, 'd'), -c(.data$game_id, .data$play_id, .data$frame_id, .data$event)),
    #     by = c('game_id', 'play_id', 'frame_id', 'event')
    #   ) %>%
    #   dplyr::mutate(dist_d = .dist(.data$x, .data$x_d, .data$y, .data$y_d)) %>%
    #   dplyr::select(
    #     .data$game_id,
    #     .data$play_id,
    #     .data$frame_id,
    #     .data$event,
    #     .data$idx_o,
    #     .data$nfl_id,
    #     .data$nfl_id_d,
    #     .data$target_nfl_id,
    #     .data$is_target,
    #     .data$x,
    #     .data$y,
    #     .data$x_d,
    #     .data$y_d,
    #     .data$dist_d
    #     # dplyr::matches('_d') # ,
    #     # .data$idx_closest
    #   )
    # 
    # min_dists_naive_d_target %>% arrow::write_parquet(path_min_dists)
  }

  features <-
    # Closest defender to each receiver.
    min_dists_robust %>%
    rename(nfl_id_target = target_nfl_id) %>%
    rename_with(~sprintf('%s_robust', .x), matches('_d$')) %>%
    # Add closest offensive players.
    inner_join(
      frames_d_renamed %>%
        rename_with(~sprintf('%s_robust', .x), matches('_d$')),
      by = c('game_id', 'play_id', 'frame_id', 'event', 'nfl_id_d_robust')
    ) %>%
    # Add closest other defender.
    dplyr::left_join(
      min_dists_naive_d_init %>%
        filter(idx_closest == 1L) %>%
        rename_with(~sprintf('%s1_naive', .x), matches('_d$')) %>%
        select(-idx_closest),
      by = c('game_id', 'play_id', 'frame_id', 'event', 'nfl_id')
    ) %>%
    dplyr::left_join(
      min_dists_naive_d_init %>%
        filter(idx_closest == 2L) %>%
        rename_with(~sprintf('%s2_naive', .x), matches('_d$')) %>%
        select(-idx_closest),
      by = c('game_id', 'play_id', 'frame_id', 'event', 'nfl_id')
    ) %>%
    dplyr::left_join(
      min_dists_naive_o,
      by = c('game_id', 'play_id', 'frame_id', 'event', 'nfl_id')
    ) %>%
    # Add closest defender to qb.
    dplyr::left_join(
      min_dists_naive_qb,
      by = c('game_id', 'play_id', 'frame_id', 'event')
    ) %>%
    # Add "static" info for each frame.
    dplyr::left_join(
      frames %>%
        dplyr::select(
          .data$game_id,
          .data$play_id,
          .data$frame_id,
          .data$event,
          .data$sec,
          .data$nfl_id,
          .data$ball_x,
          .data$ball_y,
          # .data$fd,
          .data$yards_to_go,
          .data$los
        ),
      by = c('game_id', 'play_id', 'frame_id', 'event', 'nfl_id')
    ) %>%
    dplyr::mutate(
      dist_ball = .dist(.data$x, .data$ball_x, .data$y, .data$ball_y),
      dist_ball_o = .dist(.data$x_o, .data$ball_x, .data$y_o, .data$ball_y),
      dist_ball_d_robust = .dist(.data$x_d_robust, .data$ball_x, .data$y_d_robust, .data$ball_y),
      dist_ball_d1_naive = .dist(.data$x_d1_naive, .data$ball_x, .data$y_d1_naive, .data$ball_y),
      dist_ball_d2_naive = .dist(.data$x_d2_naive, .data$ball_x, .data$y_d2_naive, .data$ball_y),
      dist_qb = .dist(.data$x, .data$qb_x, .data$y, .data$qb_y),
      dist_qb_o = .dist(.data$x_o, .data$qb_x, .data$y_o, .data$qb_y),
      dist_qb_d_robust = .dist(.data$x_d_robust, .data$qb_x, .data$y_d_robust, .data$qb_y),
      dist_qb_d1_naive = .dist(.data$x_d1_naive, .data$qb_x, .data$y_d1_naive, .data$qb_y),
      dist_qb_d2_naive = .dist(.data$x_d2_naive, .data$qb_x, .data$y_d2_naive, .data$qb_y),
      dist_qb_ball = .dist(.data$qb_x, .data$ball_x, .data$qb_y, .data$ball_y),
      dist_los = .data$x -  .data$los
    )
  features

  if(overwrite_features) {
    .display_info('Saving features for week {week} at {Sys.time()}.')
    arrow::write_parquet(features, path_features)
  }
  features
}

weeks <- 1:17L
# weeks <- 1
features <-
  weeks %>% 
  do_by_week(
    do_generate_features,
    overwrite_features = FALSE,
    overwrite_min_dists_robust = FALSE,
    overwrite_min_dists_target = TRUE
  )
features

features_n <-
  features %>%
  count(week, game_id, play_id, frame_id, event, sec)
features_n %>% filter(n != 5L) %>% count(n, name = 'nn')
features_n %>% filter(n > 6L)
bad_feature_ids <- features_n %>% filter(n > 6L) %>% distinct(game_id, play_id)
bad_feature_ids

features <-
  features %>%
  anti_join(bad_feature_ids)
features

.export_parquet <- function(x, .name = deparse(subtitute(x))) {
  arrow::write_parquet(x, file.path(get_bdb_dir_data(), .name))
}
.export_parquet(features)

min_dists_naive_od_target <-
  weeks %>%
  sprintf('min_dists_naive_od_target_week%d.parquet', .) %>%
  file.path(get_bdb_dir_data(), .) %>%
  map_dfr(arrow::read_parquet)
min_dists_naive_od_target
.export_parquet(min_dists_naive_od_target)

# min_dists_naive_d_target <-
#   weeks %>%
#   sprintf('min_dists_naive_d_target_week%d.parquet', .) %>%
#   file.path(get_bdb_dir_data(), .) %>%
#   map_dfr(arrow::read_parquet)
# min_dists_naive_d_target
# .export_parquet(min_dists_naive_d_target)
# beepr::beep(3)

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
sec_cutoff <- get_intersection_cutoff()
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
      mutate(before_cutoff = if_else(sec <= !!sec_cutoff, TRUE, FALSE)) %>% 
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
plays_w_pick_def_info
usethis::use_data(plays_w_pick_def_info, overwrite = TRUE)

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

plays_w_pick_info_features <-
  min_dists_naive_od_target_filt %>% 
  select(-target_nfl_id) %>% 
  inner_join(plays_w_pick_off_info_min) %>% 
  inner_join(plays_w_pick_def_info_min)
plays_w_pick_info_features
# arrow::write_parquet(features, file.path('inst', 'features.parquet'))
# arrow::write_parquet(features, file.path(get_bdb_dir_data(), 'plays_w_pick_info_features.parquet'))
usethis::use_data(plays_w_pick_info, overwrite = TRUE)
