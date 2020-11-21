
library(tidyverse)
positions <- import_positions()
data('personnel_and_rushers', package = 'bdb2021')

compute_min_distances_possibly <- purrr::possibly(compute_min_distances, otherwise = NULL)
do_identify_closest_defenders_ohsnap <- function(week = 1L, at = 'end_routes', ...) {

  .display_info('Identifying closest defenders for week {week} at {Sys.time()}.')

  tracking <- week %>% import_tracking()

  tracking_clipped <- tracking %>% clip_tracking_at_events(at = at)

  frames <-
   tracking_clipped %>%
   # dplyr::group_by(.data$game_id, .data$play_id) %>%
   # dplyr::filter(.data$fame_id == min(.data$frame_id) | .data$frame_id == max(.data$frame_id)) %>%
   # dplyr::ungroup()
   filter_notable_events()
  frames

  personnel_and_rushers_week <-
    personnel_and_rushers %>%
    dplyr::filter(.data$week == !!week) %>%
    dplyr::filter(.data$n_rusher > 0L) %>%
    dplyr::mutate(rushers = purrr::map(.data$rushers, ~dplyr::select(.x, .data$nfl_id, .data$idx_closest_to_ball))) %>%
    dplyr::select(.data$game_id, .data$play_id, .data$n_rusher, .data$rushers)
  personnel_and_rushers_week

  .fix_d <- function(o, d, rushers) {
    # browser()
    # n_rushers <- rushers %>% nrow()
    # if(n_rushers == 0L) {
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

  .select_side <- function(data, side = c('o', 'd')) {
    side <- match.arg(side)
    data %>%
      dplyr::filter(.data$side == toupper(!!side)) %>%
      dplyr::select(-.data$side)
  }

  # One strange source of bad data is play_id-frame_id combos where there is just 1 player.
  frames_n <-
    frames %>%
    dplyr::count(.data$game_id, .data$play_id, .data$event, .data$frame_id)

  closest_defenders_init <-
    frames %>%
    # head(100) %>%
    dplyr::anti_join(
      frames_n %>%
        dplyr::filter(.data$n == 1L),
      by = c('game_id', 'play_id', 'event', 'frame_id')
    ) %>%
    # dplyr::inner_join(
    #   positions %>%
    #     dplyr::select(.data$side, .data$position_category, .data$position),
    #   by = c('position', 'side')
    # ) %>%
    # dplyr::filter(.data$position != 'QB') %>%
    dplyr::select(
      .data$game_id,
      .data$play_id,
      .data$event,
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
      o = purrr::map(.data$data, ~.x %>% .select_side('o')),
      d = purrr::map(.data$data, ~.x %>% .select_side('d')),
      d = purrr::pmap(list(.data$o, .data$d, .data$rushers), .fix_d)
    ) %>%
    dplyr::select(-data)

  closest_defenders <-
    closest_defenders_init %>%
    slice(1) %>%
    dplyr::mutate(
      min_distances = purrr::map2(
        .data$o,
        .data$d,
        ~ compute_min_distances(o = ..1, d = ..2)
      ),
      is_bad = purrr::map_lgl(.data$min_distances, is.null)
    ) %>%
    dplyr::select(
      .data$game_id,
      .data$play_id,
      .data$event,
      .data$frame_id,
      .data$is_bad,
      .data$min_distances
    )
  closest_defenders
}

weeks <- 1:17L
# weeks <- 1L
closest_defenders_at_events <- weeks %>% do_by_week(do_identify_closest_defenders_ohsnap)
usethis::use_data(closest_defenders_at_events, overwrite = TRUE)
closest_defenders_at_events %>% write_rds('closest_defenders_at_events.rds')
# bad_closest_plays <- closest_defenders_at_events %>% filter(is_bad) %>% distinct(week, game_id, play_id)
# bad_closest_plays %>% count(week)
# bad_closest_plays %>% inner_join(bad_plays)
# plays <- import_plays()
# bad_plays <-
#   plays %>%
#   filter((n_k > 0L | n_p > 0L)) %>%
#   select(game_id, play_id)
# plays_filt <-
#   plays %>%
#   anti_join(
#     plays %>%
#       filter((n_k > 0L | n_p > 0L)) %>%
#       select(game_id, play_id),
#     by = c('game_id', 'play_id')
#   ) %>%
#   select(game_id, play_id)
# closest_defenders_at_events %>%
#   sample_n(1) %>%
#   unnest(min_distances)
