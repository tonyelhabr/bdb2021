
# Note that this script is specifically for providing Asmae and Marshall catch probability data.
# It requires that each week's min_dists_robust is already generated and saved in a parquet file.
# It does not have the same resolution has the target probability script. (It only checks for notable events, not for halfseconds.)
library(tidyverse)
positions <- import_positions()
plays <- import_plays(drop_bad = TRUE)
data('personnel_and_rushers', package = 'bdb2021')

.select_side <- function(data, side = c('O', 'D'), ...) {
  side <- match.arg(side)
  data %>%
    filter(side == !!side) %>%
    select(
      .data$game_id,
      .data$play_id,
      .data$frame_id,
      .data$event,
      .data$nfl_id,
      .data$x,
      .data$y,
      .data$s,
      .data$a,
      .data$dis,
      .data$o,
      .data$dir,
      ...
    )
}

do_generate_features_at_events <- function(week = 1L, overwrite_features = FALSE, ...) {

  # path_min_dists <- file.path('inst', sprintf('min_dists_robust_week%d.parquet', week))

  path_features <- file.path('inst', sprintf('min_dists_features_week%d.parquet', week))
  # browser()
  if(file.exists(path_features) & !overwrite_features) {
    .display_info('Importing features for week {week} at {Sys.time()} and not re-generating.')
    features <- path_features %>% arrow::read_parquet()
    return(features)
  }

  # .display_info('Importing pre-calculated min dist data for {week} at {Sys.time()}.')
  # min_dists_robust <- path_min_dists %>% arrow::read_parquet()
  # min_dists_robust <-
  #   min_dists_robust %>%
  #   mutate(across(event, ~if_else(.x == '0.0 sec', 'ball_snap', .x))) %>%
  #   filter(event %>% str_detect('sec$', negate = TRUE))

  .display_info('Generating features for week {week} at {Sys.time()}.')
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
        dplyr::select(
          .data$game_id,
          .data$play_id,
          .data$frame_id,
          qb_x = .data$x,
          qb_y = .data$y,
          qb_s = .data$s,
          qb_a = .data$a,
          qb_dis = .data$dis,
          qb_o = .data$o,
          qb_dir = .data$dir
        ),
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

  frames <- tracking %>% filter_notable_events()

  snap_frames <- tracking %>% dplyr::filter(.data$event == 'ball_snap')

  snap_frame_ids <- snap_frames %>% dplyr::distinct(.data$game_id, .data$play_id, .data$frame_id)

  personnel_and_rushers_week <-
    personnel_and_rushers %>%
    dplyr::filter(.data$week == !!week) %>%
    dplyr::filter(.data$n_rusher > 0L) %>%
    dplyr::mutate(rushers = purrr::map(.data$rushers, ~dplyr::select(.x, .data$nfl_id, .data$idx_closest_to_ball))) %>%
    dplyr::select(.data$game_id, .data$play_id, .data$n_rusher, .data$rushers)
  personnel_and_rushers_week

  # One strange source of bad data is play_id-frame_id combos where there is just 1 player.
  frames_n <-
    frames %>%
    dplyr::count(.data$game_id, .data$play_id, .data$frame_id, .data$event)

  frames_first_o <-
    frames %>%
    dplyr::filter(.data$side == 'O') %>%
    dplyr::group_by(.data$game_id, .data$play_id) %>%
    dplyr::filter(.data$frame_id == min(.data$frame_id)) %>%
    dplyr::ungroup()

  frames_n_o <-
    frames_first_o %>%
    dplyr::count(.data$game_id, .data$play_id)

  # frames_clean <-
  #   frames %>%
  #   # Get rid of the entire play.
  #   dplyr::anti_join(
  #     frames_n %>%
  #       dplyr::filter(.data$n == 1L | .data$n > 20L),
  #     by = c('game_id', 'play_id')
  #   ) %>%
  #   dplyr::anti_join(
  #     frames_n_o %>%
  #       # Plays starting with less than 5 have a defensive player playing on offense.
  #       # Plays starting with more than 5 have an offensive player playing on defense.
  #       dplyr::filter(.data$n != 5L),
  #     by = c('game_id', 'play_id')
  #   )
  frames_clean <- frames

  frames_clean_o <-
    frames_clean %>%
    .select_side(side = 'O')

  frames_clean_o_renamed <-
    frames_clean_o %>%
    rename_with(~sprintf('%s_%s', .x, 'o'), -c(.data$game_id, .data$play_id, .data$frame_id, .data$event))

  min_dists_naive_o <-
    frames_clean_o %>%
    dplyr::left_join(
      frames_clean_o_renamed,
      by = c('game_id', 'play_id', 'frame_id', 'event')
    ) %>%
    dplyr::mutate(dist_o = .dist(.data$x, .data$x_o, .data$y, .data$y_o)) %>%
    dplyr::filter(.data$dist_o > 0) %>%
    dplyr::group_by(.data$game_id, .data$play_id, .data$frame_id, .data$event, .data$nfl_id) %>%
    dplyr::filter(.data$dist_o == min(.data$dist_o)) %>%
    dplyr::ungroup() %>%
    select(game_id, play_id, frame_id, event, nfl_id, matches('_o'))

  frames_clean_qb <-
    frames_clean %>%
    dplyr::distinct(
      .data$game_id,
      .data$play_id,
      .data$frame_id,
      .data$event,
      .data$qb_x,
      .data$qb_y,
      .data$qb_s,
      .data$qb_a,
      .data$qb_dis,
      .data$qb_o,
      .data$qb_dir
    )

  frames_clean_d <-
    frames_clean %>%
    .select_side(side = 'D')

  frames_clean_d_renamed <-
    frames_clean_d %>%
    rename_with(~sprintf('%s_%s', .x, 'd'), -c(game_id, play_id, frame_id, event))

  min_dists_naive_qb <-
    frames_clean_qb %>%
    dplyr::left_join(
      frames_clean_d %>%
        rename_with(~sprintf('%s_%s', .x, 'rusher'), -c(game_id, play_id, frame_id, event)),
      by = c('game_id', 'play_id', 'frame_id', 'event')
    ) %>%
    dplyr::mutate(dist_rusher = .dist(.data$qb_x, .data$x_rusher, .data$qb_y, .data$y_rusher)) %>%
    dplyr::group_by(.data$game_id, .data$play_id, .data$frame_id, .data$event) %>%
    dplyr::filter(.data$dist_rusher == min(.data$dist_rusher)) %>%
    dplyr::ungroup()

  min_dists_naive_d_init <-
    frames_clean_o %>%
    dplyr::left_join(
      frames_clean_d %>%
        rename_with(~sprintf('%s_%s', .x, 'd'), -c(game_id, play_id, frame_id, event)),
      by = c('game_id', 'play_id', 'frame_id', 'event')
    ) %>%
    dplyr::mutate(dist_d = .dist(.data$x, .data$x_d, .data$y, .data$y_d)) %>%
    dplyr::group_by(.data$game_id, .data$play_id, .data$frame_id, .data$event, nfl_id) %>%
    mutate(idx_closest = row_number(dist_d)) %>%
    ungroup() %>%
    select(game_id, play_id, frame_id, event, nfl_id, matches('_d'), idx_closest)

  features <-
    # min_dists_robust %>%
    # select(-matches('idx_o'), -target_nfl_id) %>%
    # rename_with(~sprintf('%s_robust', .x), matches('_d$')) %>%
    frames_clean_o %>%
    # full_join(
    #   frames_clean_d_renamed %>%
    #     rename_with(~sprintf('%s_robust', .x), matches('_d$')),
    #   by = c('game_id', 'play_id', 'frame_id', 'event', 'nfl_id_d_robust')
    # ) %>%
    # Add closest other defender.
    dplyr::full_join(
      min_dists_naive_d_init %>%
        filter(idx_closest == 1L) %>%
        rename_with(~sprintf('%s1_naive', .x), matches('_d$')) %>%
        select(-idx_closest),
      by = c('game_id', 'play_id', 'frame_id', 'event', 'nfl_id')
    ) %>%
    # count(game_id, play_id, frame_id, event, nfl_id, sort = T)
    dplyr::full_join(
      min_dists_naive_d_init %>%
        filter(idx_closest == 2L) %>%
        rename_with(~sprintf('%s2_naive', .x), matches('_d$')) %>%
        select(-idx_closest),
      by = c('game_id', 'play_id', 'frame_id', 'event', 'nfl_id')
    ) %>%
    # Add closest offensive players.
    dplyr::left_join(
      min_dists_naive_o,
      by = c('game_id', 'play_id', 'frame_id', 'event', 'nfl_id')
    ) %>%
    # Add closest defender to qb.
    dplyr::left_join(
      min_dists_naive_qb,
      by = c('game_id', 'play_id', 'frame_id', 'event')
    ) %>%
    # Add "static" info for each frame.
    dplyr::left_join(
      frames_clean %>%
        dplyr::select(
          .data$game_id,
          .data$play_id,
          .data$frame_id,
          .data$event,
          .data$nfl_id,
          .data$ball_x,
          .data$ball_y,
          # .data$fd,
          .data$yards_to_go,
          .data$los
        ),
      by = c('game_id', 'play_id', 'frame_id', 'event', 'nfl_id')
    ) %>%
    dplyr::mutate(
      dist_ball = .dist(.data$x, .data$ball_x, .data$y, .data$ball_y),
      dist_ball_o = .dist(.data$x_o, .data$ball_x, .data$y_o, .data$ball_y),
      # dist_ball_d_robust = .dist(.data$x_d_robust, .data$ball_x, .data$y_d_robust, .data$ball_y),
      dist_ball_d1_naive = .dist(.data$x_d1_naive, .data$ball_x, .data$y_d1_naive, .data$ball_y),
      dist_ball_d2_naive = .dist(.data$x_d2_naive, .data$ball_x, .data$y_d2_naive, .data$ball_y),
      dist_qb = .dist(.data$x, .data$qb_x, .data$y, .data$qb_y),
      dist_qb_o = .dist(.data$x_o, .data$qb_x, .data$y_o, .data$qb_y),
      # dist_ball_d_robust = .dist(.data$x_d_robust, .data$qb_x, .data$y_d_robust, .data$qb_y),
      dist_ball_d1_naive = .dist(.data$x_d1_naive, .data$qb_x, .data$y_d1_naive, .data$qb_y),
      dist_ball_d2_naive = .dist(.data$x_d2_naive, .data$qb_x, .data$y_d2_naive, .data$qb_y),
      dist_los = .data$x -  .data$los
    )
  features
  arrow::write_parquet(features, path_features)
}

# weeks <- 1:17L
weeks <- c(1:17L)
features <- weeks %>% do_by_week(do_generate_features_at_events, overwrite_features = TRUE)
# features
# features %>% count(week, game_id, play_id, frame_id, event, sort = TRUE)
# # arrow::write_parquet(features, file.path('inst', 'features.parquet'))
# # features <- file.path('inst', 'features.parquet') %>% arrow::read_parquet()
# # features %>% filter(week == 2L)
# # # TODO: Add this removal of bad plays to the generate features script.
# features_n <-
#   features %>%
#   count(week, game_id, play_id, frame_id, event)
#
# bad_feature_ids <- features_n %>% filter(n != 5L) %>% distinct(game_id, play_id)
# bad_feature_ids
#
# # tracking <- import_tracking(3) # Some weird stuff in week 3 (game_id == 2018092300, play_id == 740; game_id == 2018092301, play_id == 477)
# features <-
#   features %>%
#   anti_join(bad_feature_ids)
# features
features <- features %>% select(-matches('idx_o'))
features
arrow::write_parquet(features, file.path('..', 'oh_snap', 'data', 'min_dists_features.parquet'))
# Not sure why I have stuff beyond 3.5 seconds into the play. I thought I explicitly cut that out (with `n_halfseconds`)?
# features_min <-
#   features %>%
#   anti_join(bad_features %>% distinct(game_id, play_id)) %>%
#   filter(sec < 3.5) %>%
#   select(week, game_id, play_id, frame_id, sec, nfl_id, nfl_id_d, dist_d)
# features_min

