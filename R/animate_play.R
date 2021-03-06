
#' Ignore rushers
#' 
#' Ignore rushers on defensive side of ball (in `d`) in min distance calculation.
#' 
#' @details This should only be used if you know what you're doing.
#' @param o,d Offense and defense matrices
#' @param rushers matrix for rushers
#' @export
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

#' Filter by team
#' 
#' Filter to one side of the ball (offense or defense).
#' 
#' @details This should only be used if you know what you're doing.
#' @param data data.frame with `side` column that is either `"O"` or `"D"`
#' @param side Either `"o"` or `"d"`. (It's converted to upper in the function.)
#' @export
.filter_side <- function(data, side = c('o', 'd')) {
  side <- match.arg(side)
  data %>%
    dplyr::filter(.data$side == toupper(!!side)) %>%
    dplyr::select(-.data$side)
}

#' Save animation
#' 
#' @param object Plot/animation to save
#' @inheritParams animate_play
#' @param end_pause Number of frames to pause at the end. Measured in frames, not seconds!
#' @param renderer Renderer. See `{gganimate}`'s options.
#' @param ... Passed to `gganimate::animate()`.
#' @export
save_animation <-
  function(object,
           height = 600,
           width = width,
           fps = 10,
           end_pause = fps,
           dir = get_bdb_dir_figs(),
           file = deparse(substitute(object)),
           ext = 'gif',
           path = file.path(dir, sprintf('%s.%s', file, ext)),
           renderer = gganimate::gifski_renderer(path),
           ...) {

    res <-
      gganimate::animate(
        object,
        fps = fps,
        height = height,
        width = width,
        renderer = renderer,
        end_pause = end_pause,
        ...
      )
  }

#' Animate a play
#'
#' @param game_id Game id (number).
#' @param play_id Play id (number).
#' @param plays,games,positions Provided data sets that will be imported.
#' via `import_*()` functions if not explicitly provided
#' @param week If not explicitly provided, looked up by filtering to `game_id` and `play_id`. If explicitly provided and it doesn't correspond to `game_id` and `play_id`, chaos will ensue.
#' @inheritParams gg_field
#' @param team_colors Whether to use team colors or not. If not, uses red and blue.
#' @param colors Data set to use for colors
#' @param buffer Space to add to sidelines. Unit is yards.
#' @param nearest_defender Whether to add nearest defender lines.
#' @param clip Whether to clip tracking data at a certain point. See `clip_tracking_at_events()`.
#' @inheritParams clip_tracking_at_events
#' @param save Whether to save animation
#' @param dir,file,ext Directory, file name (without extension), and extension used to generate `path` if `path` is not explicitly provided.
#' @param path Path where to save plot/animation.
#' @param subtitle Subtitle to add to plot/animation.
#' @param end_pause How many seconds after full play to delay.
#' @param height,width Height and width of the animation in pixels, not inches!
#' @param fps Frames per second.
#' @param ... Parameters passed to `save_animation()`.
#' @export
animate_play <-
  function(game_id = 2018090600,
           play_id = 75,
           plays = import_plays(),
           games = import_games(),
           positions = import_positions(),
           week = NULL,
           tracking = NULL,
           team_colors = FALSE,
           colors = import_colors(),
           yardmin = NULL,
           yardmax = NULL,
           field_color = '#b3e3d6',
           line_color = 'black',
           sideline_color = 'white',
           endzone_color = NULL,
           buffer = NULL,
           nearest_defender = FALSE,
           clip = TRUE,
           at = 'end_routes', # .get_valid_at_events(),
           init_cnd = dplyr::quos(.data$frame_id == 1L),
           save = TRUE,
           dir = get_bdb_dir_figs(),
           file = sprintf('%s-%s.%s', game_id, play_id, ext),
           ext = 'gif',
           path = file.path(dir, file),
           subtitle = NULL,
           # width = 10,
           # height = 10,
           end_pause = 1,
           height = 600,
           width = 600,
           fps = 10,
           ...) {
    
    # game_id = 2018090905; play_id = 783
    .display_info('Animating `game_id = {game_id}`, `play_id = {play_id}`.')
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
    
    if(nrow(tracking) == 0L) {
      .display_error('Could not identify tracking data for `game_id = {game_id}`, `play_id = {play_id}`.')
    }
    
    play <- plays %>% dplyr::inner_join(meta, by = c('game_id', 'play_id'))
    assertthat::assert_that(nrow(play) == 1L)
    
    game <- games %>% dplyr::filter(.data$game_id == !!game_id)
    assertthat::assert_that(nrow(game) == 1L)
    
    target_id <- play$target_nfl_id
    has_target <- !is.na(target_id)
    
    if(!has_target) {
      .display_warning('No target receiver.')
      # return(ggplot())
      target <- tibble::tibble(display_name = '?', jersey_number = -1)
    } else {
      target <-
        tracking %>%
        dplyr::filter(.data$nfl_id == !!target_id) %>%
        dplyr::distinct(.data$display_name, .data$jersey_number)
      assertthat::assert_that(nrow(target) == 1L)
    }

    tracking_clipped <-
      tracking %>%
      clip_tracking_at_events(at = 'throw', init_cnd = dplyr::quos(.data$frame_id == 1L)) %>%
      dplyr::arrange(.data$game_id, .data$play_id, .data$nfl_id, .data$frame_id)
    
    if(nearest_defender) {
      personnel_and_rushers <- bdb2021::personnel_and_rushers
      
      personnel_and_rushers_min <-
        personnel_and_rushers %>%
        dplyr::inner_join(meta, by = c('game_id', 'play_id')) %>% 
        dplyr::filter(.data$n_rusher > 0L) %>%
        dplyr::select(.data$game_id, .data$play_id, .data$n_rusher, .data$rushers)
      
      if(nrow(personnel_and_rushers_min) > 0L) {
        personnel_and_rushers_min <-
          personnel_and_rushers_min %>% 
          dplyr::mutate(rushers = purrr::map(.data$rushers, ~dplyr::select(.x, .data$nfl_id, .data$idx_closest_to_ball)))
      }
      
      min_dists_nested_init <-
        tracking_clipped %>%
        dplyr::filter(!(.data$side == 'O' & is.na(.data$route))) %>% 
        dplyr::select(
          .data$game_id,
          .data$play_id,
          .data$frame_id,
          .data$event,
          .data$nfl_id,
          .data$side,
          .data$x,
          .data$y
        ) %>%
        tidyr::nest(data = c(.data$nfl_id, .data$side, .data$x, .data$y)) %>%
        dplyr::left_join(
          personnel_and_rushers_min,
          by = c('game_id', 'play_id')
        ) %>%
        dplyr::mutate(
          o = purrr::map(.data$data, ~.x %>% .filter_side('o')),
          d = purrr::map(.data$data, ~.x %>% .filter_side('d')),
          d = purrr::pmap(list(.data$o, .data$d, .data$rushers), .fix_d)
        ) %>%
        dplyr::select(-data)
      min_dists_nested_init
      
      .compute_min_distances_possibly <- purrr::possibly(compute_min_distances, otherwise = NULL)
      min_dists_nested_robust <-
        min_dists_nested_init %>%
        dplyr::mutate(
          min_dists_robust = purrr::map2(
            .data$o,
            .data$d,
            ~.compute_min_distances_possibly(o = ..1, d = ..2, one_pass = FALSE)
          ),
          is_bad = purrr::map_lgl(.data$min_dists_robust, is.null)
        ) %>%
        dplyr::filter(!.data$is_bad) %>%
        dplyr::select(
          .data$game_id,
          .data$play_id,
          .data$frame_id,
          .data$event,
          .data$min_dists_robust
        )
      min_dists_nested_robust
      
      min_dists_robust <-
        min_dists_nested_robust %>%
        tidyr::unnest(.data$min_dists_robust)
      min_dists_robust
    }

    # This is also being clipped, but later than how min distance and target prob is clipped.
    if(clip) {
      tracking <-
        tracking %>%
        clip_tracking_at_events(
          # init_cnd = dplyr::quos(.data$frame_id == 1L),
          init_cnd = init_cnd,
          at = at
        )
    }
    
    line_of_scrimmage <- play$absolute_yardline_number
    play_direction <- tracking$play_direction[[1]]
    sign <- ifelse(play_direction == 'left', -1, 1)
    first_down_line <- line_of_scrimmage + sign * play$yards_to_go
    
    target_tracking <-
      tracking %>%
      dplyr::filter(.data$nfl_id == !!target_id)
    
    nontarget_tracking <-
      tracking %>%
      dplyr::filter(.data$nfl_id != !!target_id)
    
    target_tracking_clipped <-
      tracking_clipped %>%
      dplyr::filter(.data$nfl_id == !!target_id)
    
    nontarget_tracking_clipped <-
      tracking_clipped %>%
      dplyr::filter(.data$nfl_id != !!target_id)
    
    ball <-
      tracking %>%
      dplyr::distinct(.data$game_id, .data$play_id, .data$frame_id, x = .data$ball_x, y = .data$ball_y) %>%
      dplyr::mutate(nfl_id = NA_integer_)
    
    if(is.null(yardmin) | is.null(yardmax)) {
      yardminmax <-
        tracking %>%
        dplyr::summarize(dplyr::across(.data$x, list(min = min, max = max)))
      
      if(is.null(yardmin)) {
        yardmin <- yardminmax$x_min
        yardmin <- .round_any(yardmin, 10, floor)
      }
      
      if(is.null(yardmax)) {
        yardmax <- yardminmax$x_max
        yardmax <- .round_any(yardmax, 10, ceiling)
      }
    }
    
    max_y <- 160 / 3
    if(is.null(buffer)) {
      yminmax <-
        tracking %>%
        dplyr::summarize(dplyr::across(.data$y, list(min = min, max = max)))
      ymin <- yminmax$y_min
      ymin <- .round_any(ymin, 1, floor)
      ymax <- yminmax$y_max
      ymax <- .round_any(ymax, 1, ceiling)
      ymin_bound <- 0 - ymin
      ymax_bound <- ymax - max_y
      buffer <- max(0, ymin_bound, ymax_bound)
    }
    
    home_team <- game$home_team_abbr
    away_team <- game$visitor_team_abbr
    
    home_team <- game$home_team_abbr
    away_team <- game$visitor_team_abbr
    
    if(team_colors) {

      home_color <- colors %>% dplyr::filter(.data$team == !!home_team) %>% dplyr::pull(.data$color)
      away_color <- colors %>% dplyr::filter(.data$team == !!away_team) %>% dplyr::pull(.data$color)
      if(play$possession_team == home_team) {
        offense_color <- home_color
        defense_color <- away_color
      } else {
        offense_color <- away_color
        defense_color <- home_color
      }
    } else {
      offense_color <- 'red'
      defense_color <- 'blue'
      if(play$possession_team == home_team) {
        home_color <- offense_color
        away_color <- defense_color
      } else {
        home_color <- defense_color
        away_color <- offense_color
      }
    }
    
    nontarget_tracking_between <-
      nontarget_tracking %>% 
      dplyr::anti_join(
        nontarget_tracking_clipped %>% 
          dplyr::select(.data$game_id, .data$play_id, .data$nfl_id, .data$frame_id),
        by = c('nfl_id', 'frame_id', 'game_id', 'play_id')
      )

    p <-
      # tracking %>%
      ggplot2::ggplot() +
      gg_field(
        yardmin = yardmin,
        yardmax = yardmax,
        buffer = buffer,
        field_color = field_color,
        line_color = line_color,
        sideline_color = sideline_color # ,
        # ...
      ) +
      ggplot2::aes(x = .data$x, y = .data$y) +
      ggplot2::geom_segment(
        data = tibble::tibble(x = !!line_of_scrimmage),
        inherit.aes = FALSE,
        ggplot2::aes(x = .data$x, y = 0, xend = .data$x, yend = !!max_y),
        size = 1.25
      ) +
      ggplot2::geom_segment(
        data = tibble::tibble(x = !!first_down_line),
        inherit.aes = FALSE,
        ggplot2::aes(x = .data$x, y = 0, xend = .data$x, yend = !!max_y),
        color = '#ffff7f',
        size = 2
      ) +
      ggplot2::geom_point(
        data = ball,
        inherit.aes = TRUE,
        size = 3,
        color = 'brown'
      ) +
      ggplot2::geom_path(
        data = nontarget_tracking_clipped %>% dplyr::select(-.data$frame_id),
        ggplot2::aes(color = .data$side, group = .data$nfl_id),
        size = 1,
        alpha = 0.3,
        show.legend = FALSE
      ) +
      ggplot2::geom_path(
        data = nontarget_tracking_between %>% dplyr::select(-.data$frame_id),
        ggplot2::aes(color = .data$side, group = .data$nfl_id),
        size = 1,
        alpha = 0.3,
        linetype = 2,
        show.legend = FALSE
      ) +
      ggplot2::geom_text(
        data = nontarget_tracking,
        ggplot2::aes(label = .data$jersey_number, color = .data$side),
        show.legend = FALSE,
        fontface = 'bold',
        size = pts(14)
      )
    
    if(has_target) {
      
      target_tracking_between <-
        target_tracking %>% 
        dplyr::anti_join(
          target_tracking_clipped %>% 
            dplyr::select(.data$game_id, .data$play_id, .data$nfl_id, .data$frame_id),
          by = c('nfl_id', 'frame_id', 'game_id', 'play_id')
        )

      p <-
        p +
        ggplot2::geom_path(
          data = target_tracking_clipped %>% dplyr::select(-.data$frame_id),
          ggplot2::aes(color = .data$side),
          size = 2,
          alpha = 0.3,
          show.legend = FALSE
        ) +
        ggplot2::geom_path(
          data = target_tracking_between %>% dplyr::select(-.data$frame_id),
          ggplot2::aes(color = .data$side),
          size = 2,
          alpha = 0.3,
          linetype = 2,
          show.legend = FALSE
        ) +
        ggplot2::geom_text(
          data = target_tracking,
          ggplot2::aes(label = .data$jersey_number, color = .data$side),
          show.legend = FALSE,
          fontface = 'bold',
          size = pts(14)
        )
    }

    if(nearest_defender) {
      p <-
        p +
        ggplot2::geom_segment(
          data = min_dists_robust,
          inherit.aes = FALSE,
          ggplot2::aes(x = .data$x_o, y = .data$y_o, xend = .data$x_d, yend = .data$y_d),
          color = 'black'
        )
    }
    
    p <-
      p +
      ggplot2::scale_color_manual(values = c('O' = offense_color, 'D' = defense_color))
    
    # Theme stuff
    p <-
      p +
      ggplot2::theme(
        # ...,
        plot.title = ggtext::element_markdown(),
        plot.title.position = 'plot',
        strip.background = ggplot2::element_rect(fill = NA),
        # strip.text = ggplot2::element_text()
        plot.caption = ggtext::element_markdown(
          # size = 12,
          hjust = 0,
          lineheight = 0
        ),
        # plot.caption = ggplot2::element_text(hjust = 0),
        plot.caption.position = 'plot'
      ) +
      ggplot2::labs(
        subtitle = subtitle,
        title = glue::glue("<b><span style='color:{away_color};'>{away_team}</span></b> @ <b><span style='color:{home_color};'>{home_team}</span></b>, Week {game$week}"),
        caption = glue::glue('Q{play$quarter}: {play$play_description}<br />
                             Intended receiver: {target$display_name} ({target$jersey_number})<br />
                             game_id = {game$game_id}, play_id = {play$play_id}'),
        x = NULL, y = NULL
      )
    anim <- p + gganimate::transition_manual(.data$frame_id)
    
    if(!save) {
      return(anim)
    }
    
    if(!dir.exists(dirname(path))) {
      dir.create(dirname(path))
    }
    
    seconds <- ball %>% nrow() %>% {. / 10}
    nframe <- (seconds + end_pause) * fps
    res <- 
      save_animation(
        anim,
        nframe = nframe,
        fps = fps,
        height = height,
        width = width,
        path = path,
        renderer = gganimate::gifski_renderer(file = path),
        end_pause = end_pause * fps
      )
  }
