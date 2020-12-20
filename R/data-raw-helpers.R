
prep_do_by_week <- function(week = 1L, n_halfseconds = 7L, at = 'throw', ..., .msg = 'Doing thing') {

  .display_info('{.msg} for week {week} at {Sys.time()}.')

  tracking <- week %>% import_tracking()
  # tracking <- tracking %>% bdb2021::add_side_cols()

  tracking_clipped <- tracking %>% clip_tracking_at_events(at = at)

  snap_frames <- tracking %>% dplyr::filter(.data$event == 'ball_snap')

  snap_frame_ids <- 
    snap_frames %>% 
    dplyr::distinct(.data$game_id, .data$play_id, .data$frame_id)

  frames <-
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
      tracking_clipped %>%
        dplyr::select(-.data$event),
      by = c('frame_id', 'game_id', 'play_id')
    )
  frames
}

do_by_week <- function(weeks = 1L, f, ...) {
  res <-
    tibble::tibble(week = weeks) %>%
    dplyr::mutate(data = purrr::map(.data$week, f, ...)) %>%
    tidyr::unnest(.data$data)
  res
}
