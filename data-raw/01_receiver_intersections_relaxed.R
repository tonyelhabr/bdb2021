
identify_intersection_possibly <- purrr::possibly(identify_intersection, otherwise = NULL)

identify_intersections_until <- function(data, sec = 0.5) {

  .display_info('Identifying intersections up through {sec} seconds at {Sys.time()}.')
  meta <- tibble::tibble(game_id = 2018090905L, play_id = 585L) # Week 1 example of an pick before 0.5 seconds
  data <- receivers %>% inner_join(meta)
  intersections_init %>% select(data) %>% unnest(data) -> data
  intersections_init <-
    data %>%
    # Need half-second frames before `sec`, so shouldn't filter those out.
    dplyr::filter(.data$sec <= !!sec) %>%
    dplyr::select(.data$game_id, .data$play_id, .data$n_route, .data$nfl_id, .data$frame_id, .data$x, .data$y) %>%
    tidyr::nest(data = c(.data$nfl_id, .data$frame_id, .data$x, .data$y)) %>%
    dplyr::mutate(
      # intersection = purrr::map(data, identify_intersection_possibly)
      intersection = purrr::map(data, identify_intersection)
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

do_identify_receiver_intersections <- function(n_halfseconds = 7L, ...) {

  frames <- prep_do_by_week(n_halfseconds = n_halfseconds, .msg = 'Identifying closest receivers', ...)
  frames <- frames %>% add_side_cols()

  receivers <-
    frames %>%
    # Use the `y_side` from the snap time, not at a given seconds' time.
    dplyr::select(-.data$x_side, -.data$y_side) %>%
    dplyr::inner_join(
      frames %>%
        # Filter to ball snap time.
        dplyr::filter(.data$sec == 0) %>%
        # Only consider receivers not in the backfield nor starting in the middle
        # of the field.
        drop_ineligible_pick_route_frames() %>%
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
            receivers,
            sec = .x
          )
        )
    ) %>%
    tidyr::unnest(.data$data)
  intersections
}

weeks <- 1:17L
# weeks <- 1L
receiver_intersections_relaxed <- weeks %>% do_by_week(f = do_identify_receiver_intersections)

# Adjust seconds so that pick plays are more correctly categorized---by half second split and not "up until x seconds".

pick_play_ids_adj <-
  receiver_intersections_relaxed %>%
  dplyr::filter(has_intersection) %>%
  tidyr::unnest(intersection) %>%
  # dplyr::filter(nfl_id < nfl_id_intersect) %>%
  dplyr::select(
    .data$week,
    .data$game_id,
    .data$play_id,
    .data$n_route,
    .data$n_intersection,
    .data$nfl_id,
    .data$nfl_id_intersect,
    .data$sec
  ) %>%
  dplyr::group_by(.data$game_id, .data$play_id, nfl_id, nfl_id_intersect) %>%
  dplyr::filter(.data$sec == min(.data$sec)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(.data$week, .data$game_id, .data$play_id, .data$sec, .data$nfl_id, .data$nfl_id_intersect)
pick_play_ids_adj

receiver_intersections_relaxed_adj <-
  pick_play_ids_adj %>%
  dplyr::semi_join(
    receiver_intersections_relaxed,
    by = c('week', 'game_id', 'play_id', 'n_route', 'n_intersection', 'sec')
  )

receiver_intersections_relaxed_adj <-
  receiver_intersections_relaxed_adj %>%
  inner_join(
    features %>%
      select(game_id, play_id, sec, nfl_id, x, y),
    by = c('game_id', 'play_id', 'nfl_id', 'sec')
  ) %>%
  inner_join(
    features %>%
      select(game_id, play_id, sec, nfl_id, x, y) %>%
      rename_with(~sprintf('%s_intersect', .x), c(nfl_id, x, y)),
    by = c('game_id', 'play_id', 'nfl_id_intersect', 'sec')
  ) %>%
  mutate(
    # is_high = if_else(x > x_intersect, TRUE, FALSE)
    x_diff = x - x_intersect
  ) %>%
  group_by(game_id, play_id, sec) %>%
  mutate(
    x_idx = row_number(x_diff),
    x_idx_frac = x_idx / max(x_idx),
  ) %>%
  ungroup() %>%
  mutate(is_lo = x_idx_frac <= 0.5) %>%
  select(-x_idx_frac)
receiver_intersections_relaxed_adj

usethis::use_data(receiver_intersections_relaxed, overwrite = TRUE)
usethis::use_data(pick_play_ids_adj, overwrite = TRUE)
usethis::use_data(receiver_intersections_relaxed_adj, overwrite = TRUE)
