
library(tidyverse)
positions <- import_positions()
plays <- import_plays(drop_bad = TRUE)

data('personnel_and_rushers', package = 'bdb2021')

# .join_and_filter <- function(x1, x2, side = c('O', 'D')) {
#   side <- match.arg(side)
#   side_upp <- side %>% toupper()
#   side_low <- side %>% tolower()
#   col_x <- sprintf('x_%s', side_low)
#   col_y <- sprintf('y_%s', side_low)
#   col_dist <- sprintf('dist_%s', side_low)
#   col_dist_ball <- sprintf('dist_ball_%s', side_low)
#   col_dist_qb <- sprintf('dist_qb_%s', side_low)
#   col_x_sym <- col_x %>% sym()
#   col_y_sym <- col_y %>% sym()
#   col_dist_sym <- col_dist %>% sym()
#   col_dist_ball_sym <- col_dist_ball %>% sym()
#   col_dist_qb_sym <- col_dist_qb %>% sym()
#
#   x1 %>%
#     left_join(
#       x2 %>%
#         filter(side == !!side_upp) %>%
#         select(game_id, play_id, frame_id, nfl_id, x, y) %>%
#         # Add `_o` or `_d` suffix to variables that aren't being used to join.
#         rename_with(~sprintf('%s_%s', .x, side_low), -c(game_id, play_id, frame_id)),
#       by = c('game_id', 'play_id', 'frame_id')
#     ) %>%
#     # Add a temporary distance column to filter down to other player who is closest.
#     mutate(!!col_dist_sym := .dist(x, !!col_x_sym, y, !!col_y_sym)) %>%
#     # If re-joining on defensive players, make sure not to choose the same player!
#     filter(!!col_dist_sym > 0) %>%
#     group_by(game_id, play_id, frame_id, nfl_id) %>%
#     # Filter for just the player in `x2` who is closest to the player in `x1`.
#     filter(!!col_dist_sym == min(!!col_dist_sym)) %>%
#     ungroup() %>%
#     # Drop the temporary column.
#     # select(-dist) %>%
#     mutate(
#       !!col_dist_ball_sym := .dist(!!col_x_sym, ball_x, !!col_y_sym, ball_y),
#       !!col_dist_qb_sym := .dist(!!col_x_sym, qb_x, !!col_y_sym, qb_y)
#     ) # %>%
#   # rename_with(~sprintf('%s_%s', .x, side_low), c(dist, dist_ball, dist_qb)) %>%
#   # select(-one_of(c(col_x, col_y)))
# }
#

compute_min_distances_possibly <- purrr::possibly(compute_min_distances, otherwise = NULL)
do_identify_min_dists <- function(week = 1L, n_halfseconds = 7L, ...) {

  # frames <-
  #   prep_do_by_week(
  #     week = week,
  #     .msg = 'Identifying closest defenders',
  #     ...
  #   )

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

  snap_frame_ids <- snap_frames %>% dplyr::distinct(game_id, play_id, frame_id)

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

  # One strange source of bad data is play_id-frame_id combos where there is just 1 player.
  frames_n <-
    frames %>%
    dplyr::count(.data$game_id, .data$play_id, .data$frame_id)

  frames_clean <-
    frames %>%
    dplyr::anti_join(
      frames_n %>%
        dplyr::filter(.data$n == 1L),
      by = c('game_id', 'play_id', 'frame_id')
    )

  min_dists_init <-
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

  save(min_dists_robust, file = file.path('data', sprintf('min_dists_robust_week%d', week)), envir = parent.frame(), compress = 'bzip2', version = 2)
  arrow::write_parquet(min_dists_robust, file.path('data', sprintf('min_dists_robust_week%d.parquet', week)))
  # min_dists_naive_init <-
  #   min_dists_init %>%
  #   dplyr::mutate(
  #     data =
  #       dplyr::map2(
  #         .data$o, .data$d,
  #         ~dplyr::bind_rows(
  #           ..1 %>% dplyr::mutate(.data$side = 'O'),
  #           ..2 %>% dplyr::mutate(.data$side = 'D')
  #         )
  #       )
  #   ) %>%
  #   dplyr::select(.data$game_id, .data$play_id, .data$frame_id, .data$data) %>%
  #   dplyr::unnest(.data$data)
  #
  # min_dists_naive <-
  #   min_dists_naive_init %>%
  #   dplyr::filter(side == 'O') %>%
  #   dplyr::left_join(
  #     frames %>%
  #       dplyr::select(
  #         .data$game_id,
  #         .data$play_id,
  #         .data$frame_id,
  #         .data$nfl_id,
  #         .data$ball_x,
  #         .data$ball_y,
  #         .data$qb_x,
  #         .data$qb_y
  #       ),
  #     by = c('game_id', 'play_id', 'frame_id', 'nfl_id')
  #   ) %>%
  #   dplyr::mutate(
  #     dist_ball = .dist(.data$x, .data$ball_x, .data$y, .data$ball_y),
  #     dist_qb = .dist(.data$x, .data$qb_x, .data$y, .data$qb_y)
  #   ) %>%
  #   # Join and filter down to the closest offensive player.
  #   # .join_and_filter(tracking_clipped, 'o') %>%
  #   # ... to the closest defensive players.
  #   .join_and_filter(min_dists_naive_init, 'D')
  #
  #   min_dists_naive_first <-
  #     min_dists_naive %>%
  #     group_by(game_id, play_id) %>%
  #     filter(frame_id == first(frame_id)) %>%
  #     mutate(idx_o = row_number(dist_ball)) %>%
  #     ungroup()
  #   min_dists_naive_first
  #
  #   min_dists_naive_w_idx <-
  #     min_dists_naive %>%
  #     inner_join(
  #       min_dists_naive_first %>%
  #         select(game_id, play_id, nfl_id, idx_o),
  #       by = c("game_id", "play_id", "nfl_id")
  #     ) %>%
  #     rename_with(~sprintf('%s_o', .x), c(nfl_id, x, y)) %>%
  #     nest(min_dists_naive = -c(game_id, play_id, frame_id))

  min_dists_robust <-
    min_dists_init %>%
    dplyr::mutate(
      min_dists_robust = purrr::map2(
        .data$o,
        .data$d,
        ~ compute_min_distances_possibly(o = ..1, d = ..2, one_pass = TRUE)
      ),
      is_bad_robust = purrr::map_lgl(.data$min_dists_robust, is.null)
    ) %>%
    # dplyr::filter(!is_bad_robust) %>%
    # dplyr::select(-is_bad_robust) %>%
    dplyr::select(
      .data$game_id,
      .data$play_id,
      .data$frame_id,
      .data$is_bad_robust,
      .data$min_dists_robust
    )

  # min_dists_robust %>% filter(is_bad_robust) %>% inner_join(min_dists_init)
  # tracking_clipped %>%
  #   filter(game_id == 2018090600, play_id == 256) %>%
  #   # pull(play_description)
  #   filter(frame_id == min(frame_id)) %>%
  #   relocate(frame_id) %>%
  #   filter(side == 'O')

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

  min_dists_naive_o <-
    frames_clean %>%
    .select_side(side = 'O') %>%
    left_join(
      frames_clean %>%
        .select_side(side = 'O') %>%
        # Add `_o` or `_d` suffix to variables that aren't being used to join.
        rename_with(~sprintf('%s_%s', .x, 'o'), -c(game_id, play_id, frame_id)),
      by = c('game_id', 'play_id', 'frame_id')
    ) %>%
    # Add a temporary distance column to filter down to other player who is closest.
    mutate(dist_o = .dist(.data$x, .data$x_o, .data$y, .data$y_o)) %>%
    # If re-joining on defensive players, make sure not to choose the same player!
    filter(.data$dist_o > 0) %>%
    group_by(.data$game_id, .data$play_id, .data$frame_id, .data$nfl_id) %>%
    # Filter for just the player in `x2` who is closest to the player in `x1`.
    filter(.data$dist_o == min(.data$dist_o)) %>%
    ungroup()

  res <-
    min_dists_robust %>%
    tidyr::unnest(.data$min_dists_robust) %>%
    dplyr::select(
      .data$game_id,
      .data$play_id,
      .data$frame_id,
      nfl_id = .data$nfl_id_o,
      nfl_id_d = .data$nfl_id_d,
      dist_d = .data$dist
    ) %>%
    dplyr::left_join(
      min_dists_naive_o,
      by = c('game_id', 'play_id', 'frame_id', 'nfl_id')
    ) %>%
    dplyr::left_join(
      frames_clean %>%
        .select_side(side = 'D') %>%
        # Add `_o` or `_d` suffix to variables that aren't being used to join.
        dplyr::rename_with(~sprintf('%s_%s', .x, 'd'), -c(game_id, play_id, frame_id)),
      by = c('game_id', 'play_id', 'frame_id', 'nfl_id_d')
    ) %>%
    dplyr::left_join(
      frames_clean %>%
        dplyr::select(
          .data$game_id,
          .data$play_id,
          .data$frame_id,
          .data$nfl_id,
          .data$ball_x,
          .data$ball_y,
          .data$qb_x,
          .data$qb_y,
          .data$qb_o,
          # .data$fd,
          .data$yards_to_go,
          .data$los
        ),
      by = c('game_id', 'play_id', 'frame_id', 'nfl_id')
    ) %>%
    dplyr::left_join(
      plays %>%
        dplyr::select(
          .data$game_id,
          .data$play_id,
          .data$target_nfl_id
        ),
      by = c('game_id', 'play_id')
    ) %>%
    dplyr::mutate(
      dist_ball_o = .dist(.data$x_o, .data$ball_x, .data$y_o, .data$ball_y),
      dist_ball_d = .dist(.data$x_d, .data$ball_x, .data$y_d, .data$ball_y),
      dist_qb_o = .dist(.data$x_o, .data$qb_x, .data$y_o, .data$qb_y),
      dist_qb_d = .dist(.data$x_d, .data$qb_x, .data$y_d, .data$qb_y),
      # dist_fd = .data$fd - .data$x,
      dist_los = .data$los - .data$x,
      is_target = dplyr::if_else(.data$nfl_id == .data$target_nfl_id, 1L, 0L)
    ) %>%
    dplyr::select(-.data$target_nfl_id)
  res
  cols_pivot <- res %>% names() %>% str_subset('_[od]$')
  cols_pivot
  cols_static <- c('game_id', 'play_id', 'frame_id', 'ball_x', 'ball_y', 'qb_x', 'qb_y', 'qb_o', 'los')
  res %>%
    mutate(idx_o = row_number()) %>%
    head(5) %>%
    # select(one_of(c('idx_o', cols_pivot, 'is_target'))) %>%
    pivot_wider(
      names_from = idx_o,
      values_from  = setdiff(names(res), cols_static) # c('nfl_id', 'is_target', cols_pivot)
    )
  rec <-
    res %>%
    recipes::recipe(formula(is_target ~ .), data = .)
  res %>%
    mutate(s_ratio_d = s / coalesce(s_d, 0.01)) %>%
    mutate(across(s_ratio_d, ~case_when(s_ratio_d > 10 ~ 10, s_ratio_d < -10 ~ -10, TRUE ~ .x))) %>%
    ggplot() +
    aes(x = s_ratio_d) +
    geom_histogram()

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

  # usethis::use_data(min_dists, overwrite = TRUE, name = sprintf('min_dists_week%d'))
  save(min_dists, file = file.path('data', sprintf('min_dists_week%d', week)), envir = parent.frame(), compress = 'bzip2', version = 2)
}

weeks <- 1:17L
# weeks <- 1L
min_dists <- weeks %>% do_by_week(do_identify_min_dists)
usethis::use_data(min_dists, overwrite = TRUE)
