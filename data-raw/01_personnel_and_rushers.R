
do_identify_personnel_and_rushers <- function(week = 1L, at = 'end_rush', ...) {

  .display_info('Identifying personnel and rushers for week {week} at {Sys.time()}.')

  tracking <- week %>% import_tracking()

  tracking_clipped <- tracking %>% clip_tracking_at_events(at = at)

  end_frames <-
    tracking_clipped %>%
    dplyr::group_by(.data$game_id, .data$play_id) %>%
    dplyr::filter(.data$frame_id == max(.data$frame_id)) %>%
    dplyr::ungroup()
  end_frames

  def_end_frames <-
    end_frames %>%
    dplyr::filter(.data$side == 'D')

  ball_end_frames <-
    end_frames %>%
    dplyr::distinct(game_id, play_id, frame_id, ball_x, ball_y)

  potential_rushers <-
    def_end_frames %>%
    dplyr::filter(.data$x < .data$los) %>%
    dplyr::select(game_id, play_id, frame_id, nfl_id, x, y)
  potential_rushers

  potential_rushers_ranked <-
    potential_rushers %>%
    dplyr::select(game_id, play_id, nfl_id, x, y) %>%
    dplyr::inner_join(
      ball_end_frames,
      by = c('game_id', 'play_id')
    ) %>%
    dplyr::mutate(
      dist_to_ball_abs = .dist(x, ball_x, y, ball_y) %>% abs()
    ) %>%
    dplyr::arrange(game_id, play_id, dist_to_ball_abs) %>%
    dplyr::group_by(game_id, play_id) %>%
    dplyr::mutate(
      idx_closest_to_ball = dplyr::row_number(dist_to_ball_abs)
    )

  def_agg <-
    def_end_frames %>%
    dplyr::select(.data$game_id, .data$play_id) %>%
    dplyr::group_by(.data$game_id, .data$play_id) %>%
    dplyr::summarize(n_d = dplyr::n()) %>%
    dplyr::ungroup()

  end_frames %>%
    dplyr::filter(.data$side == 'O' & .data$position != 'QB') %>%
    dplyr::count(position)

  pos_n <-
    end_frames %>%
    dplyr::left_join(
      positions %>%
        select(.data$side, .data$position, .data$position_category) %>%
        mutate(across(.data$position_category, tolower))
    ) %>%
    dplyr::count(.data$game_id, .data$play_id, .data$position_category) %>%
    tidyr::pivot_wider(
      names_from = .data$position_category,
      values_from = .data$n,
      names_prefix = 'n_'
    )

  off_agg <-
    end_frames %>%
    dplyr::filter(.data$side == 'O' & .data$position != 'QB') %>%
    dplyr::group_by(.data$game_id, .data$play_id) %>%
    dplyr::summarize(
      n_route = sum(!is.na(.data$route))
    ) %>%
    dplyr::ungroup()

  personnel_and_rushers <-
    pos_n %>%
    dplyr::left_join(
      off_agg,
      by = c('game_id', 'play_id')
    ) %>%
    dplyr::left_join(
      def_agg,
      by = c('game_id', 'play_id')
    ) %>%
    dplyr::left_join(
      potential_rushers_ranked %>%
        tidyr::nest(
          rushers = -c(.data$game_id, .data$play_id)
        ),
      by = c('game_id', 'play_id')
    ) %>%
    dplyr::mutate(
      # has_rusher = map_lgl(rushers, ~!is.null(.x))
      rushers = purrr::map_if(rushers, is.null, ~tibble::tibble()),
      n_rusher = purrr::map_int(rushers, ~nrow(.x))
    ) %>%
    dplyr::relocate(rushers, .after = dplyr::last_col())
  personnel_and_rushers

}

weeks <- 1:17L
personnel_and_rushers <- weeks %>% do_by_week(f = do_identify_personnel_and_rushers)
usethis::use_data(personnel_and_rushers, overwrite = TRUE)
