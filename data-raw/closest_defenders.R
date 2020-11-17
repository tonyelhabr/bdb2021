
library(tidyverse)
data('rushers', package = 'bdb2021')

compute_min_distances_possibly <- purrr::possibly(compute_min_distances, otherwise = NULL)
do_identify_closest_defenders <- function(week = 1L, ...) {


  frames <-
    prep_do_by_week(
      week = week,
      .msg = 'Identifying closest defenders',
      ...
    )

  rushers_week <-
    rushers %>%
    dplyr::filter(week == !!week) %>%
    dplyr::select(-week) %>%
    tidyr::unnest(data) %>%
    # unnest(data)
    dplyr::rename(rushers = data) %>%
    dplyr::mutate(rushers = purrr::map(rushers, ~dplyr::select(.x, nfl_id, idx_closest_to_ball)))
  rushers_week %>% tidyr::unnest(rushers)

  .fix_d <- function(o, d, rushers) {
    n_rushers <- rushers %>% nrow()
    if(n_rushers == 0L) {
      return(d)
    }
    n_d <- d %>% nrow()
    n_o <- o %>% nrow()
    n_d_wo_rushers <- n_d - n_rushers
    if(n_d_wo_rushers >= n_o) {
      res <- dplyr::anti_join(d, rushers, by = 'nfl_id')
    } else {
      res_init <- dplyr::left_join(d, rushers, by = 'nfl_id')
      res <-
        res_init %>%
        dplyr::mutate(dplyr::across(idx_closest_to_ball, ~dplyr::coalesce(.x, 0L))) %>%
        dplyr::mutate(rn = dplyr::row_number(idx_closest_to_ball)) %>%
        dplyr::filter(rn <= !!n_o) %>%
        dplyr::select(nfl_id, x, y)
    }
    res
  }

  min_dists <-
    frames %>%
    # head(1000) %>%
    dplyr::inner_join(
      positions %>%
        dplyr::select(side, position_category, position),
      by = c('position', 'side')
    ) %>%
    dplyr::filter(position != 'QB') %>%
    dplyr::select(-position_category) %>%
    dplyr::select(game_id, play_id, frame_id, nfl_id, side, x, y) %>%
    tidyr::nest(data = c(nfl_id, side, x, y)) %>%
    dplyr::left_join(rushers_week, by = c('game_id', 'play_id')) %>%
    dplyr::mutate(
      has_rusher = purrr::map_lgl(rushers, ~!is.null(.x)),
      rushers = purrr::map_if(rushers, is.null, ~tibble::tibble())
    ) %>%
    dplyr::mutate(
      o = purrr::map(data, ~.x %>% dplyr::filter(side == 'O') %>% dplyr::select(-side)),
      d = purrr::map(data, ~.x %>% dplyr::filter(side == 'D') %>% dplyr::select(-side)),
      d = purrr::pmap(list(o, d, rushers), .fix_d)
    ) %>%
    dplyr::select(-data)

  min_dists <-
    min_dists %>%
    dplyr::mutate(
      min_distances = purrr::map2(o, d, ~compute_min_distances_possibly(o = ..1, d = ..2)), #  %>% pluck('result')),
      # data = map(data, ~compute_min_distances(.x)),
      is_bad = purrr::map_lgl(min_distances, is.null)
    ) %>%
    dplyr::select(game_id, play_id, frame_id, is_bad, min_distances)
  # min_dists_init %>% slice(1) %>% select(min_distances) %>% unnest(min_distances)
  #   is_bad <- min_dists_init %>% filter(is_bad)
  #   is_bad %>% count(game_id, play_id)
  #   is_bad_debug <- is_bad %>% tail(1)
  #   o <- is_bad_debug %>% select(o) %>% unnest(o)
  #   d <- is_bad_debug %>% select(d) %>% unnest(d)
  #   tracking %>%
  #     semi_join(is_bad_debug %>% select(game_id, play_id, frame_id)) %>%
  #     inner_join(
  #       positions %>%
  #         select(side, position_category, position),
  #       by = c('position', 'side')
  #     )
  #   debugonce(compute_min_distances)
  #   compute_min_distances(o, d)
  min_dists
}

weeks <- 1:17L
# weeks <- 1L
closest_defenders <- weeks %>% do_by_week(do_identify_closest_defenders)
usethis::use_data(closest_defenders, overwrite = TRUE)
