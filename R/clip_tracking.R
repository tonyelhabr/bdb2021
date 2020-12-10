
.switch_events_end <- function(at = c('throw', 'end_routes', 'end_rush')) {
  at <- match.arg(at)
  switch(
    at,
    throw = .get_events_throw(),
    end_routes = .get_events_end_routes(),
    end_rush = .get_events_end_rush()
  )
}

#' Clip tracking data at events
#'
#' @param tracking Tracking data frame
#' @param at character vector indicating which `event` at which to clip each play
#' @param init_cnd a quosure for a filtering condition used to start the clip. This is ball snap by default. Only change this if you know what you're doing.
#' @export
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
