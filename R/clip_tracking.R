
.switch_events_end <- function(at = c('throw', 'end_routes', 'end_rush')) {
  at <- match.arg(at)
  switch(
    at,
    throw = .get_events_throw(),
    end_routes = .get_events_end_routes(),
    end_rush = .get_events_end_rush()
  )
}

# Probably need to put this in some other file since it doesn't go perfectly with all these `read_*` functions.
# When `group = 0`, it's pre-snap. When `group = 2`, it's `events`.
# An alternate `init_cnd` might be `quos(frame_id == 1)`. (You don't technically need to use `quos()` if it's just one condition; just `quo()` would suffice.) A base R alternative to the `quos()`-`!!!` combo would be `expr()`-`eval()`.
clip_tracking_at_events <-
  function(tracking,
           at = 'throw',
           init_cnd = dplyr::quos(.data$event == 'ball_snap')) {
    events <- .switch_events_end(at)
    assertthat::assert_that(rlang::is_quosures(init_cnd))
    tracking %>%
      dplyr::group_by(.data$game_id, .data$play_id, .data$nfl_id) %>%
      dplyr::mutate(
        group =
          dplyr::case_when(
            !!!init_cnd ~ 1L,
            dplyr::lag(.data$event) %in% !!events ~ 1L,
            TRUE ~ 0L
          ),
        group = cumsum(.data$group)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$group == 1L) %>%
      dplyr::select(-.data$group)
  }
