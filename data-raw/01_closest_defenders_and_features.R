
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
      .data$nfl_id,
      .data$x,
      .data$y,
      .data$s,
      .data$a,
      .data$dir,
      ...
    )
}

do_generate_features <- function(week = 1L, n_halfseconds = 7L, overwrite = FALSE, ...) {

  # frames <-
  #   prep_do_by_week(
  #     week = week,
  #     .msg = 'Identifying closest defenders',
  #     ...
  #   )
  .display_info('Generating features for week {week} at {Sys.time()}.')
  tracking <- week %>% import_tracking(standardize = FALSE)

  tracking <-
    tracking %>%
    dplyr::semi_join(
      plays %>%
        dplyr::select(.data$game_id, .data$play_id),
      by = c('game_id', 'play_id')
    )

  qb <- tracking %>% dplyr::filter(position == 'QB')
  tracking <- tracking %>% dplyr::filter(position != 'QB')

  tracking <-
    tracking %>%
    dplyr::inner_join(
      qb %>%
        dplyr::select(.data$game_id, .data$play_id, .data$frame_id, qb_x = .data$x, qb_y = .data$y, qb_s = .data$s, qb_o = .data$o),
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

  # Make this faster by clipping before x,y standardization first?
  at <- 'throw'
  tracking_clipped <- tracking %>% clip_tracking_at_events(at = at)

  snap_frames <- tracking %>% dplyr::filter(.data$event == 'ball_snap')

  snap_frame_ids <- snap_frames %>% dplyr::distinct(.data$game_id, .data$play_id, .data$frame_id)

  frames <-
    snap_frame_ids %>%
    dplyr::mutate(n = !!n_halfseconds) %>%
    tidyr::uncount(.data$n) %>%
    dplyr::group_by(.data$game_id, .data$play_id) %>%
    # Create half seconds.
    dplyr::mutate(
      sec = 0.5 * (dplyr::row_number() - 1L)
    ) %>%
    dplyr::ungroup() %>%
    # Technically this should be an integer, but we don't really need to coerce it.
    dplyr::mutate(
      frame_id = .data$frame_id + .data$sec * 10,
    ) %>%
    dplyr::inner_join(
      tracking_clipped %>%
        dplyr::select(-.data$event),
      by = c('frame_id', 'game_id', 'play_id')
    )
  frames

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
    dplyr::count(.data$game_id, .data$play_id, .data$frame_id)

  frames_n_o <-
    frames %>%
    filter(side == 'O') %>%
    group_by(game_id, play_id) %>%
    filter(frame_id == min(frame_id)) %>%
    ungroup() %>%
    count(game_id, play_id)
  frames_n_o

  frames_clean <-
    frames %>%
    dplyr::anti_join(
      frames_n %>%
        dplyr::filter(.data$n == 1L),
      by = c('game_id', 'play_id', 'frame_id')
    ) %>%
    dplyr::anti_join(
      frames_n_o %>%
        # Plays with less than 5 have a defensive player playing on offense.
        # Plays with more than 5 have an offensive player playing on defense.
        dplyr::filter(.data$n != 5L),
      by = c('game_id', 'play_id')
    )

  path_min_dists <- file.path('inst', sprintf('min_dists_robust_week%d.parquet', week))
  if(!file.exists(path_min_dists) | overwrite) {

    frames_first <-
      frames_clean %>%
      filter(side == 'O') %>%
      group_by(game_id, play_id) %>%
      filter(frame_id == min(frame_id)) %>%
      ungroup() %>%
      group_by(game_id, play_id) %>%
      mutate(
        dist_ball = .dist(x, ball_x, y, ball_y),
        idx_o = row_number(dist_ball)
      ) %>%
      ungroup() %>%
      select(-dist_ball) %>%
      select(game_id, play_id, nfl_id, idx_o)
    frames_first

    frames_target <-
      frames_first %>%
      dplyr::left_join(
        plays %>%
          dplyr::select(
            .data$game_id,
            .data$play_id,
            .data$target_nfl_id
          ),
        by = c('game_id', 'play_id')
      ) %>%
      mutate(
        is_target = dplyr::if_else(.data$nfl_id == .data$target_nfl_id, 1L, 0L)
      )
    frames_target

    frames_target_idx_o <-
      frames_target %>%
      filter(.data$nfl_id == .data$target_nfl_id) %>%
      select(.data$game_id, .data$play_id, idx_o_target = .data$idx_o)
    frames_target_idx_o

    min_dists_nested_init <-
      frames_clean %>%
      dplyr::select(
        .data$game_id,
        .data$play_id,
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
        o = purrr::map(.data$data, ~.x %>% .filter_side('o')),
        d = purrr::map(.data$data, ~.x %>% .filter_side('d')),
        d = purrr::pmap(list(.data$o, .data$d, .data$rushers), .fix_d)
      ) %>%
      dplyr::select(-data)

    min_dists_nested_robust <-
      min_dists_nested_init %>%
      dplyr::mutate(
        min_dists_robust = purrr::map2(
          .data$o,
          .data$d,
          ~.compute_min_distances_possibly(o = ..1, d = ..2, one_pass = TRUE)
        ),
        is_bad_robust = purrr::map_lgl(.data$min_dists_robust, is.null)
      ) %>%
      dplyr::filter(!is_bad_robust) %>%
      dplyr::select(-is_bad_robust) %>%
      dplyr::select(
        .data$game_id,
        .data$play_id,
        .data$frame_id,
        # .data$is_bad_robust,
        .data$min_dists_robust
      )
    beepr::beep(3)

    min_dists_robust <-
      min_dists_nested_robust %>%
      tidyr::unnest(.data$min_dists_robust) %>%
      dplyr::select(
        .data$game_id,
        .data$play_id,
        .data$frame_id,
        nfl_id = .data$nfl_id_o,
        nfl_id_d = .data$nfl_id_d,
        dist_d = .data$dist
      ) %>%
      # dplyr::left_join(
      #   frames_first,
      #   by = c('game_id', 'play_id', 'nfl_id')
      # ) %>%
      dplyr::left_join(
        frames_target,
        by = c('game_id', 'play_id', 'nfl_id')
      ) %>%
      dplyr::left_join(
        frames_target_idx_o,
        by = c('game_id', 'play_id')
      )

    min_dists_robust %>% arrow::write_parquet(path_min_dists)
  } else {
    min_dists_robust <- path_min_dists %>% arrow::read_parquet()
  }

  frames_clean_o <-
    frames_clean %>%
    .select_side(side = 'O')

  frames_clean_o_renamed <-
    frames_clean_o %>%
    rename_with(~sprintf('%s_%s', .x, 'o'), -c(game_id, play_id, frame_id))

  min_dists_naive_o <-
    frames_clean_o %>%
    left_join(
      frames_clean_o_renamed,
      by = c('game_id', 'play_id', 'frame_id')
    ) %>%
    mutate(dist_o = .dist(.data$x, .data$x_o, .data$y, .data$y_o)) %>%
    filter(.data$dist_o > 0) %>%
    group_by(.data$game_id, .data$play_id, .data$frame_id, .data$nfl_id) %>%
    filter(.data$dist_o == min(.data$dist_o)) %>%
    ungroup()

  frames_clean_qb <-
    frames_clean %>%
    distinct(.data$game_id, .data$play_id, .data$frame_id, .data$qb_x, .data$qb_y)

  frames_clean_d <-
    frames_clean %>%
    .select_side(side = 'D')

  frames_clean_d_renamed <-
    frames_clean_d %>%
    rename_with(~sprintf('%s_%s', .x, 'd'), -c(game_id, play_id, frame_id))

  min_dists_naive_qb <-
    frames_clean_qb %>%
    left_join(
      frames_clean_d %>%
        rename_with(~sprintf('%s_%s', .x, 'rusher'), -c(game_id, play_id, frame_id)),
      by = c('game_id', 'play_id', 'frame_id')
    ) %>%
    mutate(dist_rusher = .dist(.data$qb_x, .data$x_rusher, .data$qb_y, .data$y_rusher)) %>%
    group_by(.data$game_id, .data$play_id, .data$frame_id) %>%
    filter(.data$dist_rusher == min(.data$dist_rusher)) %>%
    ungroup()

  features <-
    # Closest defender to each receiver.
    min_dists_robust %>%
    # Add closest offensive players.
    dplyr::left_join(
      min_dists_naive_o,
      by = c('game_id', 'play_id', 'frame_id', 'nfl_id')
    ) %>%
    # Add closest defender to qb.
    dplyr::left_join(
      min_dists_naive_qb,
      by = c('game_id', 'play_id', 'frame_id')
    ) %>%
    # Add closest other defender.
    dplyr::left_join(
      frames_clean_d_renamed,
      by = c('game_id', 'play_id', 'frame_id', 'nfl_id_d')
    ) %>%
    # Add "static" info for each frame.
    dplyr::left_join(
      frames_clean %>%
        dplyr::select(
          .data$game_id,
          .data$play_id,
          .data$frame_id,
          .data$sec,
          .data$nfl_id,
          .data$ball_x,
          .data$ball_y,
          # .data$fd,
          .data$yards_to_go,
          .data$los
        ),
      by = c('game_id', 'play_id', 'frame_id', 'nfl_id')
    ) %>%
    dplyr::mutate(
      dist_ball_o = .dist(.data$x_o, .data$ball_x, .data$y_o, .data$ball_y),
      dist_ball_d = .dist(.data$x_d, .data$ball_x, .data$y_d, .data$ball_y),
      dist_qb_o = .dist(.data$x_o, .data$qb_x, .data$y_o, .data$qb_y),
      dist_qb_d = .dist(.data$x_d, .data$qb_x, .data$y_d, .data$qb_y),
      # Probably only need this for the `nfl_id`, not for `nfl_id_o` or `nlf_id_d`.
      # dist_fd = .data$fd - .data$x,
      dist_ball = .dist(.data$x, .data$ball_x, .data$y, .data$ball_y),
      dist_qb = .dist(.data$x, .data$qb_x, .data$y, .data$qb_y),
      dist_los = .data$los - .data$x
    ) %>%
    dplyr::select(-.data$target_nfl_id)
  features

  # nms <- features %>% names()
  # cols_ball <- nms %>% str_subset('^ball_')
  # cols_qb <- nms %>% str_subset('^qb_')
  # cols_rusher <- nms %>% str_subset('^rusher_')
  # cols_static <-
  #   c(
  #     'game_id',
  #     'play_id',
  #     'frame_id',
  #     'sec',
  #     'los',
  #     'yards_to_go',
  #     'idx_o_target',
  #     cols_ball,
  #     cols_qb,
  #     cols_rusher
  #   )
  #
  # features_wide <-
  #   features %>%
  #   # head(5) %>%
  #   pivot_wider(
  #     names_from = c('idx_o'),
  #     values_from  = setdiff(nms, cols_static)
  #   )
  # features_wide
  #
  # rec <-
  #   features_wide %>%
  #   recipes::recipe(formula(is_target ~ .), data = .) %>%
  #   recipes::update_role(
  #     dplyr::matches('^nfl_id_'),
  #     dplyr::matches('_id$'),
  #     new_role = 'id'
  #   )
  # rec

  # res %>%
  #   mutate(s_ratio_d = s / coalesce(s_d, 0.01)) %>%
  #   mutate(across(s_ratio_d, ~case_when(s_ratio_d > 10 ~ 10, s_ratio_d < -10 ~ -10, TRUE ~ .x))) %>%
  #   ggplot() +
  #   aes(x = s_ratio_d) +
  #   geom_histogram()

  # min_dists <-
  #   dplyr::inner_join(
  #     min_dists_naive_w_idx,
  #     min_dists_robust
  #   )
  #
  # min_dists %>%
  #   head(1) %>%
  #   dplyr::select(.data$min_dists_naive, .data$min_dists_robust) %>%
  #   dplyr::mutate(
  #     min_dists_naive =
  #       purrr::map(
  #         .data$min_dists_naive,
  #         ~dplyr::rename_all(..1, ~sprintf('%s_naive', .x))
  #       )
  #   ) %>%
  #   tidyr::unnest(cols = c(.data$min_dists_naive, .data$min_dists_robust))

  arrow::write_parquet(features, file.path('inst', sprintf('features_week%d.parquet', week)))
  # save(features_long, file = file.path('inst', sprintf('target_prob_features_week%d.rds', week)), envir = parent.frame(), compress = 'bzip2', version = 2)
}

weeks <- 1:17L
# weeks <- 1L
features <- weeks %>% do_by_week(do_generate_features)
# usethis::use_data(features, overwrite = TRUE)
arrow::write_parquet(features, file.path('inst', 'features.parquet'))
