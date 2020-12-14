
#' @export
retrieve_target_prob_features <- memoise::memoise({function(features) {
  # nms <- features %>% names()
  # cols_ball <- nms %>% str_subset('^ball_')
  # cols_qb <- nms %>% str_subset('^qb_')
  # cols_od <- nms %>% str_subset('_[od]$|_[od]_')
  # cols_rusher <- nms %>% str_subset('rusher') %>% str_subset('nfl_id', negate = TRUE)
  
  # cols_ball <- sprintf('ball_', c('x', 'y'))
  # cols_qb <- sprintf('qb_', c('x', 'y', 'o'))
  # cols_od <- 
  #   c(
  #     sprintf('x_%d', 1:5), 
  #     sprintf('x_d_%d', 1:5), 
  #     sprintf('y_%d', 1:5), 
  #     sprintf('y_d_%d', 1:5)
  #   )
  cols_rusher <- sprintf('%s_rusher', c('x', 'y', 'dist'))
  col_y <- 'idx_o_target'
  
  cols_id <-
    c(
      # 'week',
      'game_id',
      'play_id',
      'frame_id'
    )
  
  # , 'event', 'event_lag1'
  cols_static_value <-
    c(
      'los',
      'qb_o',
      'qb_x',
      'qb_y',
      'ball_x',
      'ball_y',
      'sec',
      # 'event',
      # 'event_lag1',
      cols_rusher
    )
  
  cols_id_model <-
    c(
      'event',
      'event_lag1',
      'idx'
    )
  
  cols_static <-
    c(
      cols_id,
      cols_id_model,
      cols_static_value
    )
  
  cols_pivot_name <- 'idx_o'
  
  cols_pivot_value <-
    c(
      'x',
      'y',
      'o',
      'o_d1_naive',
      'dist_ball',
      'dist_d1_naive',
      'dist_ball_d1_naive'
    )
  
  cols_keep <-
    c(
      col_y,
      cols_static,
      cols_pivot_name,
      cols_pivot_value
    )
  rgx_pivot_value <- sprintf('^(%s)_([1-5]|[1-5]_diff1)$', paste(cols_pivot_value, collapse = '|', sep = ''))
  
  list(
    col_y = col_y,
    cols_id = cols_id,
    cols_id_model = cols_id_model,
    cols_static_value = cols_static_value,
    cols_static = cols_static,
    cols_pivot_name = cols_pivot_name,
    cols_pivot_value = cols_pivot_value,
    cols_keep = cols_keep,
    rgx_pivot_value = rgx_pivot_value
  )
}})

#' @export
prep_target_prob_data <- function(features, all_frames = TRUE, drop_bad = !all_frames, max_sec = 3) {
  # all_frames = TRUE; drop_bad = !all_frames; max_sec = 3
  events_end_rush <- .get_events_end_rush()
  
  if(!all_frames) {
    suppressWarnings(
      features <- 
        features %>% 
        dplyr::mutate(
          sec = dplyr::case_when(
            event %>% stringr::str_detect('sec$') ~ event %>% stringr::str_remove(' sec') %>% as.double(),
            TRUE ~ NA_real_
          )
        ) %>% 
        dplyr::filter(sec <= !!max_sec | event %in% events_end_rush)
    )
  }
  
  if(drop_bad) {
    
    features_events <-
      features %>%
      dplyr::mutate(dplyr::across(event, ~dplyr::if_else(.x == '0.0 sec', 'ball_snap', .x))) %>%
      dplyr::filter(event %>% stringr::str_detect('sec$', negate = TRUE)) %>%
      dplyr::distinct(game_id, play_id, event, frame_id) %>%
      dplyr::group_by(game_id, play_id, event) %>%
      dplyr::filter(frame_id == min(frame_id)) %>%
      dplyr::ungroup()
    
    play_ids_to_drop <-
      features_events %>%
      dplyr::filter(
        event %in% c('pass_shovel', 'qb_spike', 'pass_tipped')
      ) %>%
      dplyr::distinct(game_id, play_id)
    play_ids_to_drop
    
    features <-
      features %>%
      dplyr::anti_join(
        play_ids_to_drop,
        by = c('game_id', 'play_id')
      )
    features
  }
  
  events_lag1 <-
    features %>%
    dplyr::distinct(game_id, play_id, frame_id, event) %>%
    dplyr::group_by(game_id, play_id) %>%
    dplyr::arrange(frame_id, .by_group = TRUE) %>%
    dplyr::mutate(event_lag1 = dplyr::lag(event)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(event_lag1, ~dplyr::coalesce(.x, 'none')))
  
  features <-
    features %>%
    dplyr::left_join(
      events_lag1,
      by = c('game_id', 'play_id', 'frame_id', 'event')
    )

  if(!all_frames) {
    features <-
      features %>%
      dplyr::filter(event %>% stringr::str_detect('sec$') | event == 'pass_forward')
  }
  
  cols_lst <- features %>% retrieve_target_prob_features()
  
  routes <- bdb2021::routes
  
  features_wide <-
    features %>%
    dplyr::group_by(game_id, play_id, frame_id, nfl_id) %>%
    dplyr::mutate(rn = dplyr::row_number()) %>%
    dplyr::filter(rn == max(rn)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-rn) %>%
    dplyr::left_join(
      routes %>% dplyr::select(-.data$week),
      by = c('game_id', 'play_id', 'nfl_id')
    ) %>% 
    dplyr::filter(idx_o <= 5L) %>% 
    dplyr::mutate(
      dplyr::across(dplyr::matches('^x_|_x'), ~los + .x),
      dplyr::across(dplyr::any_of(cols_lst$cols_pivot_value), ~dplyr::if_else(is.na(route), 9999, .x)),
      # across(where(is.double), ~if_else(is.na(.x), 9999,~ .x)),
      dplyr::across(idx_o_target, ~dplyr::if_else(.x > 5L, NA_integer_, .x)),
      dplyr::across(idx_o_target, ~dplyr::coalesce(.x, 9L) %>% factor())
    ) %>%
    # idx won't be in here
    dplyr::select(dplyr::any_of(cols_lst$cols_keep)) %>%
    tidyr::pivot_wider(
      names_from = dplyr::all_of(cols_lst$cols_pivot_name),
      values_from = dplyr::all_of(cols_lst$cols_pivot_value)
    ) %>%
    dplyr::mutate(
      dplyr::across(where(is.double), ~dplyr::if_else(is.na(.x), 9999, .x))
    ) %>% 
    # dplyr::group_by(game_id, play_id) %>% 
    # dplyr::arrange(frame_id, .by_group = TRUE) %>% 
    # dplyr::mutate(
    #   dplyr::across(where(is.double), list(diff1 = ~(.x - dplyr::lag(.x)))),
    #   dplyr::across(dplyr::matches('_diff1$'), ~dplyr::coalesce(.x, 0)),
    # ) %>% 
    # dplyr::ungroup() %>% 
    dplyr::mutate(idx = dplyr::row_number()) %>%
    dplyr::relocate(idx)
  features_wide
  
  features_wide_min <-
    features_wide %>%
    dplyr::select(
      dplyr::all_of(cols_lst$col_y),
      dplyr::all_of(cols_lst$cols_id),
      dplyr::all_of(cols_lst$cols_id_model),
      dplyr::all_of(cols_lst$cols_static_value),
      dplyr::matches(cols_lst$rgx_pivot_value)
    )
  features_wide_min
}

#' Bind probabilities to data set
#' 
#' For use in single play
#'
#' @export
predict_target_probability <- function(fit, set) {
  cols_lst <- retrieve_target_prob_features()
  dplyr::bind_cols(
    fit %>% stats::predict(set),
    fit %>% stats::predict(set, type = 'prob')
  ) %>%
    dplyr::bind_cols(
      set %>% 
        dplyr::select(dplyr::one_of(c(cols_lst$cols_id, cols_lst$cols_id_model, cols_lst$col_y)))
    ) %>%
    # TODO
    dplyr::pivot_longer(
      dplyr::matches('^[.]pred_'),
      names_to = idx_o_target,
      values_to = 'prob'
    )
}

#' Bind probabilities to data set
#' 
#' For use in big boy fit creation
#'
#' @export
rebind_fit_probs <- function(fit, set, nm = deparse(substitute(set))) {
  cols_lst <- retrieve_target_prob_features()
  dplyr::bind_cols(
    fit %>% stats::predict(set),
    fit %>% stats::predict(set, type = 'prob')
  ) %>%
    dplyr::mutate(.set = !!nm) %>%
    dplyr::relocate(.set) %>%
    dplyr::bind_cols(
      set %>% 
        dplyr::select(dplyr::one_of(c(cols_lst$cols_id, cols_lst$cols_id_model, cols_lst$col_y)))
    )
}

#' Fit target probabilities
#' 
#' For use with big boy data
#' 
#' @export
fit_target_prob_split <- function(trn, tst, fmla, min_n = 2, mtry = 42, trees = 500, suffix = '', overwrite = FALSE) {
  
  .display_info('Fitting rf for `min_n = {min_n}`, `mtry = {mtry}`, `trees = {trees}` at {Sys.time()}.')
  path_suffix <- sprintf('%s-min_n=%d-mtry=%d-trees=%d', suffix, min_n, mtry, trees)
  .path <- function(prefix, ext) {
    file.path(get_bdb_dir_data(), sprintf('%s-%s.%s', prefix, path_suffix, ext))
  }
  
  path_fit <- .path('fit', ext = 'rds')
  path_probs <- .path('probs', ext = 'parquet')
  path_acc <- .path('acc', ext = 'csv')
  fit_exists <- path_fit %>% file.exists()
  probs_exist <- path_probs %>% file.exists()
  acc_exists <- path_acc %>% file.exists()
  
  if(all(fit_exists, probs_exist, acc_exists, !overwrite)) {
    .display_info('Skipping fitting and importing from "{path_acc}".')
    acc <- path_acc %>% readr::read_csv()
    return(acc)
  }

  cols_lst <- retrieve_target_prob_features()
  rec <-
    recipes::recipe(fmla, data = trn) %>%
    recipes::update_role(
      #A idx,
      dplyr::all_of(cols_lst$cols_id),
      dplyr::all_of(cols_lst$cols_id_model),
      new_role = 'extra'
    )
  spec <-
    parsnip::rand_forest(
      trees = !!trees,
      min_n = !!min_n,
      mtry = !!mtry
    ) %>%
    parsnip::set_mode('classification') %>%
    # parsnip::set_engine('ranger', importance = 'permutation')
    parsnip::set_engine('ranger')
  spec
  
  wf <-
    workflows::workflow() %>%
    workflows::add_recipe(rec) %>%
    workflows::add_model(spec)
  
  fit_ran <- FALSE
  if(!all(fit_exists, !overwrite)) {
    fit <- parsnip::fit(wf, trn)
    # readr::write_rds(fit, path_fit)
    fit_ran <- TRUE
  } else {
    .display_info('Skipping fitting.')
  }
  
  probs_ran <- FALSE
  if(!all(probs_exist, !overwrite)) {
    if(!fit_ran) {
      fit <- path_fit %>% read_rds()
    }
    probs <-
      dplyr::bind_rows(
        rebind_fit_probs(fit, trn, 'trn'),
        rebind_fit_probs(fit, tst, 'tst')
      )
    arrow::write_parquet(probs, path_probs)
    probs_ran <- TRUE
  } else {
    .display_info('Skipping predictions.')
  }
  
  if(!all(acc_exists, !overwrite)) {
    if(!fit_ran) {
      fit <- path_fit %>% readr::read_rds()
    }
    if(!probs_ran) {
      probs <- path_probs %>% arrow::read_parquet()
    }
    all <- 
      list(trn, tst) %>% 
      purrr::reduce(dplyr::bind_rows) %>% 
      dplyr::select(idx, game_id, play_id, frame_id, event)
    
    acc <-
      probs %>%
      dplyr::left_join(all) %>% 
      tidyr::nest(data = -c(.set, event)) %>%
      dplyr::mutate(res = purrr::map(data, ~yardstick::accuracy(.x, idx_o_target, .pred_class))) %>%
      dplyr::select(-data) %>%
      tidyr::unnest(res) %>%
      dplyr::arrange(.set, event)
    
    readr::write_csv(acc, path_acc)
  } else {
    acc <- path_acc %>% readr::read_csv()
  }
  acc
}

.select_side <- function(data, side = c('O', 'D'), ...) {
  side <- match.arg(side)
  data %>%
    dplyr::filter(side == !!side) %>%
    dplyr::select(
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

#' @export
generate_target_prob_features <- 
  function(game_id = 2018090600,
           play_id = 75,
           plays = import_plays(),
           games = import_games(),
           positions = import_positions(),
           week = NULL,
           tracking = NULL,
           ...) {
    
    .display_info('Generating target prob features for `game_id = {game_id}`, `play_id = {play_id}`.')
    meta <- tibble::tibble(game_id = game_id, play_id = play_id)
    
    has_week <- !is.null(week)
    if(!has_week) {
      game <- games %>% dplyr::filter(game_id == !!game_id)
      assertthat::assert_that(nrow(game) == 1L)
      week <- game$week
    }
    
    if(is.null(tracking)) {
      tracking <- import_tracking(week = week, positions = positions, standardize = FALSE)
    }
    
    tracking <-
      tracking %>%
      dplyr::inner_join(meta, by = c('game_id', 'play_id'))
    
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
    
    at <- 'throw'
    tracking_clipped <- tracking %>% clip_tracking_at_events(at = at)
    
    frames <-
      tracking_clipped %>%
      # Easy way to get rid of some weird duplicates
      dplyr::distinct()
    snap_frames <- frames %>% dplyr::filter(.data$event == 'ball_snap')
    
    snap_frame_ids <-
      snap_frames %>%
      dplyr::distinct(.data$game_id, .data$play_id, .data$frame_id)

    personnel_and_rushers <- bdb2021::personnel_and_rushers
    personnel_and_rushers_week <-
      personnel_and_rushers %>%
      dplyr::filter(.data$week == !!week) %>%
      dplyr::filter(.data$n_rusher > 0L) %>%
      dplyr::mutate(rushers = purrr::map(.data$rushers, ~dplyr::select(.x, .data$nfl_id, .data$idx_closest_to_ball))) %>%
      dplyr::select(.data$game_id, .data$play_id, .data$n_rusher, .data$rushers)
    personnel_and_rushers_week
    
    frames_first <-
      frames %>%
      # dplyr::filter(.data$side == 'O') %>%
      dplyr::group_by(.data$game_id, .data$play_id, .data$side) %>%
      dplyr::filter(.data$frame_id == min(.data$frame_id)) %>%
      dplyr::ungroup()
    
    frames_first_n <-
      frames_first %>%
      dplyr::count(.data$game_id, .data$play_id, .data$side)
    
    frames_first_idx_o <-
      frames_first %>%
      dplyr::filter(.data$side == 'O') %>%
      dplyr::group_by(.data$game_id, .data$play_id) %>%
      dplyr::mutate(
        dist_ball = .dist(.data$x, .data$ball_x, .data$y, .data$ball_y),
        idx_o = dplyr::row_number(.data$dist_ball)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$game_id, .data$play_id, .data$nfl_id, .data$idx_o)
    frames_first_idx_o
    
    frames_target <-
      frames_first_idx_o %>%
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
        is_target = dplyr::if_else(.data$nfl_id == .data$target_nfl_id, 1L, 0L)
      )
    frames_target
    
    frames_target_idx_o <-
      frames_target %>%
      dplyr::filter(.data$nfl_id == .data$target_nfl_id) %>%
      dplyr::select(.data$game_id, .data$play_id, idx_o_target = .data$idx_o)
    frames_target_idx_o
    
    frames_o <-
      frames %>%
      .select_side(side = 'O')
    
    frames_o_renamed <-
      frames_o %>%
      dplyr::rename_with(~sprintf('%s_%s', .x, 'o'), -c(.data$game_id, .data$play_id, .data$frame_id, .data$event))
    
    min_dists_naive_o <-
      frames_o %>%
      dplyr::left_join(
        frames_o_renamed,
        by = c('game_id', 'play_id', 'frame_id', 'event')
      ) %>%
      dplyr::mutate(dist_o = .dist(.data$x, .data$x_o, .data$y, .data$y_o)) %>%
      dplyr::filter(.data$dist_o > 0) %>%
      dplyr::group_by(.data$game_id, .data$play_id, .data$frame_id, .data$event, .data$nfl_id) %>%
      dplyr::filter(.data$dist_o == min(.data$dist_o)) %>%
      dplyr::ungroup() %>%
      dplyr::select(game_id, play_id, frame_id, event, nfl_id, dplyr::matches('_o'))
    
    frames_qb <-
      frames %>%
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
    
    frames_d <-
      frames %>%
      .select_side(side = 'D')
    
    frames_d_renamed <-
      frames_d %>%
      dplyr::rename_with(~sprintf('%s_%s', .x, 'd'), -c(game_id, play_id, frame_id, event))
    
    min_dists_naive_qb <-
      frames_qb %>%
      dplyr::left_join(
        frames_d %>%
          dplyr::rename_with(~sprintf('%s_%s', .x, 'rusher'), -c(game_id, play_id, frame_id, event)),
        by = c('game_id', 'play_id', 'frame_id', 'event')
      ) %>%
      dplyr::mutate(dist_rusher = .dist(.data$qb_x, .data$x_rusher, .data$qb_y, .data$y_rusher)) %>%
      dplyr::group_by(.data$game_id, .data$play_id, .data$frame_id, .data$event) %>%
      dplyr::filter(.data$dist_rusher == min(.data$dist_rusher)) %>%
      dplyr::ungroup()
    
    min_dists_naive_d_init <-
      frames_o %>%
      dplyr::left_join(
        frames_d %>%
          dplyr::rename_with(~sprintf('%s_%s', .x, 'd'), -c(.data$game_id, .data$play_id, .data$frame_id, .data$event)),
        by = c('game_id', 'play_id', 'frame_id', 'event')
      ) %>%
      dplyr::mutate(dist_d = .dist(.data$x, .data$x_d, .data$y, .data$y_d)) %>%
      dplyr::group_by(.data$game_id, .data$play_id, .data$frame_id, .data$event, .data$nfl_id) %>%
      dplyr::mutate(idx_closest = dplyr::row_number(.data$dist_d)) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        .data$game_id,
        .data$play_id,
        .data$frame_id,
        .data$event,
        .data$nfl_id,
        dplyr::matches('_d'),
        .data$idx_closest
      )
    
    features <-
      frames_o %>%
      dplyr::left_join(
        snap_frame_ids %>% dplyr::rename(frame_id_min = frame_id),
        by = c('game_id', 'play_id')
      ) %>% 
      dplyr::mutate(
        sec = 0.1 * (frame_id - frame_id_min)
      ) %>% 
      dplyr::select(-frame_id_min) %>% 
      dplyr::left_join(
        frames_target %>% dplyr::rename(nfl_id_target = target_nfl_id),
        by = c('game_id', 'play_id', 'nfl_id')
      ) %>%
      dplyr::left_join(
        frames_target_idx_o,
        by = c('game_id', 'play_id')
      ) %>% 
      # Add closest other defender.
      dplyr::left_join(
        min_dists_naive_d_init %>%
          dplyr::filter(idx_closest == 1L) %>%
          dplyr::rename_with(~sprintf('%s1_naive', .x), dplyr::matches('_d$')) %>%
          dplyr::select(-idx_closest),
        by = c('game_id', 'play_id', 'frame_id', 'event', 'nfl_id')
      ) %>%
      dplyr::left_join(
        min_dists_naive_o,
        by = c('game_id', 'play_id', 'frame_id', 'event', 'nfl_id')
      ) %>%
      # Add closest defender to qb.
      dplyr::left_join(
        min_dists_naive_qb,
        by = c('game_id', 'play_id', 'frame_id', 'event')
      ) %>%
      # Add 'static' info for each frame.
      dplyr::left_join(
        frames %>%
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
        dist_ball_d1_naive = .dist(.data$x_d1_naive, .data$ball_x, .data$y_d1_naive, .data$ball_y),
        dist_qb = .dist(.data$x, .data$qb_x, .data$y, .data$qb_y),
        dist_qb_o = .dist(.data$x_o, .data$qb_x, .data$y_o, .data$qb_y),
        dist_qb_d1_naive = .dist(.data$x_d1_naive, .data$qb_x, .data$y_d1_naive, .data$qb_y),
        dist_qb_ball = .dist(.data$qb_x, .data$ball_x, .data$qb_y, .data$ball_y),
        dist_los = .data$x -  .data$los
      )
    features
  }


