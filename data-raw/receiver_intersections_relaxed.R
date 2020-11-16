## code to prepare `receiver_intersections_relaxed` dataset goes here

# games <- bdb2021::import_games()
# plays <- bdb2021::import_plays()

identify_intersection_possibly <- purrr::possibly(bdb2021::identify_intersection, otherwise = NULL)

identify_intersections_until <- function(data, sec = 0.5, .verbose = bdb2021:::.get_verbose()) {

  bdb2021:::.display_info('Identifying intersections up through {sec} seconds at {Sys.time()}.', .verbose = .verbose)

  intersections_init <-
    data %>%
    # Need half-second frames before `sec`, so shouldn't filter those out.
    dplyr::filter(.data$sec <= !!sec) %>%
    dplyr::select(.data$game_id, .data$play_id, .data$n_route, .data$nfl_id, .data$frame_id, .data$x, .data$y) %>%
    tidyr::nest(data = c(.data$nfl_id, .data$frame_id, .data$x, .data$y)) %>%
    dplyr::mutate(
      intersection = purrr::map(data, identify_intersection_possibly)
    )

  bad_intersections <-
    intersections_init %>%
    dplyr::mutate(
      is_bad = purrr::map_lgl(.data$intersection, ~is.null(.x))
    ) %>%
    dplyr::filter(.data$is_bad) %>%
    dplyr::select(-.data$is_bad, -.data$intersection)

  intersections <-
    intersections_init %>%
    dplyr::anti_join(
      bad_intersections,
      by = c('game_id', 'play_id', 'n_route', 'data')
    ) %>%
    dplyr::mutate(
      has_intersection = purrr::map_lgl(.data$intersection, ~nrow(.x) > 0),
    ) %>%
    # dplyr::filter(.data$has_intersection) %>%
    # dplyr::select(-.data$data, -.data$has_intersection) %>%
    dplyr::select(-.data$data) %>%
    # Note that can't do `purrr::map_int()`, otherwise there is an error.
    dplyr::mutate(n_intersection = purrr::map_dbl(.data$intersection, ~nrow(.x) / 2L) %>% as.integer())

  res <-
    intersections %>%
    dplyr::mutate(eligible = TRUE) %>%
    dplyr::select(
      .data$game_id,
      .data$play_id,
      .data$n_route,
      .data$eligible,
      .data$has_intersection,
      .data$n_intersection,
      .data$intersection
    ) %>%
    dplyr::arrange(.data$game_id, .data$play_id)
  res
}

do_identify_receiver_intersections <- function(week, ..., n_halfseconds = 7L, .verbose = bdb2021:::.get_verbose()) {

  bdb2021:::.display_info('Identifying intersections for week {week} at {Sys.time()}.', .verbose = .verbose)

  tracking <- week %>% bdb2021::import_week()
  tracking <- tracking %>% bdb2021::add_side_cols()

  tracking_at_throw <- tracking %>% bdb2021::clip_tracking_at_events('throw')

  snap_frames <- tracking %>% dplyr::filter(.data$event == 'ball_snap')

  snap_frame_ids <- snap_frames %>% dplyr::distinct(game_id, play_id, frame_id)
  seconds_frames <-
    snap_frame_ids %>%
    # Only need up to 0.5 * `n_nalfseconds` seconds (e.g. 0, 0.5, 1, ..., 3.5 if `n_halfseconds = 7L`).
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
      tracking_at_throw %>%
        dplyr::select(-.data$event),
      by = c('frame_id', 'game_id', 'play_id')
    )

  receivers_at_snap <- snap_frames %>% bdb2021::add_idx_y_col()

  receivers_at_seconds <-
    seconds_frames %>%
    # Use the `y_side` from the snap time, not at a given seconds' time.
    dplyr::select(-.data$x_side, -.data$y_side) %>%
    dplyr::inner_join(
      seconds_frames %>%
        # Filter to ball snap time.
        dplyr::filter(.data$sec == 0) %>%
        # Only consider receivers not in the backfield nor starting in the middle
        # of the field.
        bdb2021::drop_ineligible_pick_route_frames() %>%
        dplyr::group_by(.data$game_id, .data$play_id, .data$frame_id) %>%
        dplyr::mutate(n_route = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::select(.data$game_id, .data$play_id, .data$nfl_id, .data$n_route),
      by = c('game_id', 'play_id', 'nfl_id')
    )

  intersections <-
    tibble::tibble(
      sec = seq(0.5, by = 0.5, length.out = n_halfseconds)
    ) %>%
    dplyr::mutate(
      data =
        purrr::map(
          .data$sec,
          ~identify_intersections_until(
            receivers_at_seconds,
            sec = .x,
            .verbose = .verbose
          )
        )
    ) %>%
    tidyr::unnest(data)
  intersections
}

# weeks <- 1L:17L
weeks <- 1L
receiver_intersections_relaxed <-
  tibble::tibble(week = !!weeks) %>%
  dplyr::mutate(data = purrr::map(.data$week, do_identify_receiver_intersections))
receiver_intersections

usethis::use_data(receiver_intersections_relaxed, overwrite = TRUE)
