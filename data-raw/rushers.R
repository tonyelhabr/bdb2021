
do_identify_rushers <- function(week = 1L, at = 'end_rush', ...) {

  .display_info('Identifying rushers for week {week} at {Sys.time()}.')

  tracking <- week %>% import_week()
  # tracking <- tracking %>% bdb2021::add_side_cols()

  tracking_clipped <- tracking %>% clip_tracking_at_events(at = at)

  end_frames <-
    tracking_clipped %>%
    dplyr::group_by(.data$game_id, .data$play_id) %>%
    dplyr::filter(.data$frame_id == max(.data$frame_id)) %>%
    dplyr::ungroup()
  end_frames

  def_end_frames <- end_frames %>% dplyr::filter(.data$side == 'D')

  # qb_end_frames <- end_frames %>% filter(position == 'QB')
  # ball_end_frames <- end_frames %>% select(game_id, play_id, ball_x, ball_y)
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

  # def_end_frames %>%
  #   filter((x + 1) < los) %>%
  #   count(game_id, play_id, event, sort = TRUE) %>%
  #   head(10) %>%
  #   mutate(plot = map2(game_id, play_id, ~plot_play(game_id = ..1, play_id = ..2, save = TRUE, dir = 'data-raw', tracking = tracking)))
  #
  # positive_sacks <-
  #   end_frames %>%
  #   dplyr::filter(position == 'QB') %>%
  #   filter(x > los) %>%
  #   select(game_id, play_id) %>%
  #   inner_join(plays %>% select(game_id, play_id, play_result, pass_result, play_description))
  # positive_sacks

  rushers <-
    potential_rushers_ranked %>%
    tidyr::nest(rushers = -c(game_id, play_id))
  rushers

}

weeks <- 1:17L
rushers <- weeks %>% do_by_week(f = do_identify_rushers)
usethis::use_data(rushers, overwrite = TRUE)
