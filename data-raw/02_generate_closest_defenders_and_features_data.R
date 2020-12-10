
library(tidyverse)
positions <- import_positions()
plays <- import_plays(drop_bad = TRUE)
data('personnel_and_rushers', package = 'bdb2021')

.compute_min_distances_possibly <- purrr::possibly(compute_min_distances, otherwise = NULL)

.fix_d <- function(o, d, rushers) {

  has_rushers <- !is.null(rushers)
  # TODO: Do something if there are more route runners than defenders?
  if(!has_rushers) {
    return(d)
  }
  n_rushers <- rushers %>% nrow()
  n_d <- d %>% nrow()
  n_o <- o %>% nrow()
  n_d_wo_rushers <- n_d - n_rushers
  if(n_d_wo_rushers >= n_o) {
    # If there will be at least as many defenders as offensive players after removing rushers.
    res <- dplyr::anti_join(d, rushers, by = 'nfl_id')
  } else {
    res_init <- dplyr::left_join(d, rushers, by = 'nfl_id')
    res <-
      res_init %>%
      dplyr::mutate(dplyr::across(.data$idx_closest_to_ball, ~dplyr::coalesce(.x, 0L))) %>%
      dplyr::mutate(rn = dplyr::row_number(.data$idx_closest_to_ball)) %>%
      dplyr::filter(.data$rn <= !!n_o) %>%
      dplyr::select(.data$nfl_id, .data$x, .data$y)
  }
  res
}

.filter_side <- function(data, side = c('o', 'd')) {
  side <- match.arg(side)
  data %>%
    dplyr::filter(.data$side == toupper(!!side)) %>%
    dplyr::select(-.data$side)
}

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

do_generate_features <- function(week = 1L, n_halfseconds = 7L, overwrite_features = TRUE, overwrite_min_dists = FALSE, overwrite_min_dists_target = TRUE, ...) {

  path_min_dists <- file.path(.get_dir_data(), sprintf('min_dists_robust_week%d.parquet', week))
  do_min_dists <- !file.exists(path_min_dists) | overwrite_min_dists

  path_min_dists_target <- file.path(.get_dir_data(), sprintf('min_dists_naive_target_week%d.parquet', week))
  do_min_dists_target <- !file.exists(path_min_dists_target) | overwrite_min_dists_target

  path_features <- file.path(.get_dir_data(), sprintf('features_week%d.parquet', week))
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

  # Some plays have 2 ball snaps?!?
  # tracking %>% filter(game_id == 2018091605L, play_id == 2715L) %>% filter(event != 'None') %>% count(frame_id, event)

  # tracking <-
  #   tracking %>%
  #   dplyr::semi_join(
  #     plays %>%
  #       dplyr::select(.data$game_id, .data$play_id),
  #     by = c('game_id', 'play_id')
  #   )

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
  
  # pick_plays_wo_features %>% head(6) %>% distinct(play_id) -> play_ids
  # tracking_at_events %>% inner_join(play_ids) %>% count(play_id)

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

  # frames_clean <-
  #   frames %>%
  #   # Get rid of the entire play.
  #   dplyr::anti_join(
  #     frames_first_n %>%
  #       dplyr::filter(.data$n < 5L | .data$n > 11L),
  #     by = c('game_id', 'play_id')
  #   ) %>%
  #   dplyr::anti_join(
  #     frames_first_n %>%
  #       dplyr::filter(.data$side == 'O') %>%
  #       # Plays starting with less than 5 have a defensive player playing on offense.
  #       # Plays starting with more than 5 have an offensive player playing on defense.
  #       dplyr::filter(.data$n != 5L),
  #     by = c('game_id', 'play_id')
  #   )

  frames_clean <- frames

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
    # Play with a receiver pass to a QB, which will end up having NA idx_o_target. Typically, idx_o_target is NA (along with all nfl_id's for a play) if the QB is sacked.
    # plays %>% filter(game_id == 2018090600, play_id == 2736) %>% glimpse()
    # frames_target %>% distinct(game_id, play_id, target_nfl_id)
    # frames_target %>%
    #   dplyr::filter(.data$nfl_id == .data$target_nfl_id)
    # frames_target %>%
    #   group_by(game_id, play_id) %>%
    #   summarize(is_target = sum(is_target)) %>%
    #   ungroup() %>%
    #   filter(is_target == 0L) %>%
    #   left_join(frames_target %>% distinct(game_id, play_id, target_nfl_id)) %>%
    #   left_join(
    #     players_from_tracking %>%
    #       rename(target_nfl_id = nfl_id)
    #   ) %>%
    #   left_join(plays %>% select(game_id, play_id, play_description)) %>%
    #   select(game_id, play_id, display_name, position, play_description) -> z

    # frames_clean %>%
    #   filter(game_id == 2018090600L, play_id == 776L) %>%
    #   filter(side == 'D') %>%
    #   distinct(nfl_id)

    min_dists_nested_init <-
      frames_clean %>%
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
    # min_dists_robust_temp
    min_dists_robust %>% arrow::write_parquet(path_min_dists)

    # min_dists_robust %>%
    #   dplyr::select(
    #     .data$game_id,
    #     .data$play_id,
    #     .data$frame_id,
    #     .data$event,
    #     .data$nfl_id,
    #     .data$nfl_id_d,
    #     .data$dist_d
    #   ) %>%
    #   dplyr::left_join(
    #     frames_target,
    #     by = c('game_id', 'play_id', 'nfl_id')
    #   ) %>%
    #   # filter(is.na(target_nfl_id)) %>%
    #   dplyr::left_join(
    #     frames_target_idx_o,
    #     by = c('game_id', 'play_id')
    #   ) %>%
    #   filter(!is.na(target_nfl_id) & is.na(idx_o_target))

  }

  frames_clean_o <-
    frames_clean %>%
    .select_side(side = 'O')

  frames_clean_o_renamed <-
    frames_clean_o %>%
    rename_with(~sprintf('%s_%s', .x, 'o'), -c(.data$game_id, .data$play_id, .data$frame_id, .data$event))

  min_dists_naive_o <-
    frames_clean_o %>%
    dplyr::left_join(
      frames_clean_o_renamed,
      by = c('game_id', 'play_id', 'frame_id', 'event')
    ) %>%
    dplyr::mutate(dist_o = .dist(.data$x, .data$x_o, .data$y, .data$y_o)) %>%
    dplyr::filter(.data$dist_o > 0) %>%
    dplyr::group_by(.data$game_id, .data$play_id, .data$frame_id, .data$event, .data$nfl_id) %>%
    dplyr::filter(.data$dist_o == min(.data$dist_o)) %>%
    dplyr::ungroup()

  frames_clean_qb <-
    frames_clean %>%
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

  frames_clean_d <-
    frames_clean %>%
    .select_side(side = 'D')

  frames_clean_d_renamed <-
    frames_clean_d %>%
    rename_with(~sprintf('%s_%s', .x, 'd'), -c(game_id, play_id, frame_id, event))

  min_dists_naive_qb <-
    frames_clean_qb %>%
    dplyr::left_join(
      frames_clean_d %>%
        rename_with(~sprintf('%s_%s', .x, 'rusher'), -c(game_id, play_id, frame_id, event)),
      by = c('game_id', 'play_id', 'frame_id', 'event')
    ) %>%
    dplyr::mutate(dist_rusher = .dist(.data$qb_x, .data$x_rusher, .data$qb_y, .data$y_rusher)) %>%
    dplyr::group_by(.data$game_id, .data$play_id, .data$frame_id, .data$event) %>%
    dplyr::filter(.data$dist_rusher == min(.data$dist_rusher)) %>%
    dplyr::ungroup()

  min_dists_naive_d_init <-
    frames_clean_o %>%
    dplyr::left_join(
      frames_clean_d %>%
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
    min_dists_naive_d_target <-
      frames_target %>%
      # dplyr::filter(is_target == 1L) %>%
      dplyr::left_join(
        frames_clean_o,
        by = c('game_id', 'play_id', 'nfl_id')
      ) %>%
      dplyr::left_join(
        frames_clean_d %>%
          dplyr::rename_with(~sprintf('%s_%s', .x, 'd'), -c(.data$game_id, .data$play_id, .data$frame_id, .data$event)),
        by = c('game_id', 'play_id', 'frame_id', 'event')
      ) %>%
      dplyr::mutate(dist_d = .dist(.data$x, .data$x_d, .data$y, .data$y_d)) %>%
      # dplyr::group_by(.data$game_id, .data$play_id, .data$frame_id, .data$event, .data$nfl_id) %>%
      # dplyr::mutate(idx_closest = dplyr::row_number(.data$dist_d)) %>%
      # dplyr::ungroup() %>%
      dplyr::select(
        .data$game_id,
        .data$play_id,
        .data$frame_id,
        .data$event,
        .data$idx_o,
        .data$nfl_id,
        .data$nfl_id_d,
        .data$target_nfl_id,
        .data$is_target,
        .data$x,
        .data$y,
        .data$x_d,
        .data$y_d,
        .data$dist_d
        # dplyr::matches('_d') # ,
        # .data$idx_closest
      )

    min_dists_naive_d_target %>% arrow::write_parquet(path_min_dists_target)
  }

  features <-
    # Closest defender to each receiver.
    min_dists_robust %>%
    rename(nfl_id_target = target_nfl_id) %>%
    rename_with(~sprintf('%s_robust', .x), matches('_d$')) %>%
    # Add closest offensive players.
    inner_join(
      frames_clean_d_renamed %>%
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
      frames_clean %>%
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

# weeks <- 1:17L
weeks <- 1:17L
features <- weeks %>% do_by_week(do_generate_features, overwrite_features = TRUE, overwrite_min_dists = TRUE, overwrite_min_dists_target = TRUE)
features
features %>% filter(is.na(nfl_id_d_robust))

features_n <-
  features %>%
  count(week, game_id, play_id, frame_id, event, sec)
features_n %>% filter(n != 5L) %>% count(n, name = 'nn')
features_n %>% filter(n > 6L)
bad_feature_ids <- features_n %>% filter(n > 6L) %>% distinct(game_id, play_id)
bad_feature_ids

new_features <-
  features %>%
  anti_join(bad_feature_ids)
new_features
arrow::write_parquet(features, file.path(.get_dir_data(), 'new_features.parquet'))


min_dists_naive_target <-
  weeks %>%
  sprintf('min_dists_naive_target_week%d.parquet', .) %>%
  file.path(.get_dir_data(), .) %>%
  map_dfr(arrow::read_parquet)
min_dists_naive_target
arrow::write_parquet(min_dists_naive_target, file.path(.get_dir_data(), 'min_dists_naive_target.parquet'))
beepr::beep(3)