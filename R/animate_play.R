
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


#' @export
.filter_side <- function(data, side = c('o', 'd')) {
  side <- match.arg(side)
  data %>%
    dplyr::filter(.data$side == toupper(!!side)) %>%
    dplyr::select(-.data$side)
}

#' @export
save_animation <-
  function(gg,
           file = deparse(substitute(gg)),
           ext = 'png',
           dir = .get_dir_figs(),
           path = file.path(dir, sprintf('%s.%s', file, ext)),
           height = 8,
           width = height,
           ...) {
    ggplot2::ggsave(plot = gg, filename = path, width = width, height = height, type = 'cairo', ...)
  }

#' Plot a play
#'
#' @description Plot a play
#' @param game_id `game_id`
#' @param play_id `play_id`
#' @examples
#' \dontrun{
#' plot_play()
#' }
#'
animate_play <-
  function(game_id = 2018090600,
           play_id = 75,
           plays = import_plays(),
           games = import_games(),
           positions = import_positions(),
           week = NULL,
           tracking = NULL,
           team_colors = FALSE,
           yardmin = NULL,
           yardmax = NULL,
           field_color = '#b3e3d6',
           line_color = 'black',
           sideline_color = 'white',
           endzone_color = NULL,
           buffer = NULL,
           nearest_defender = FALSE,
           target_probability = FALSE,
           save = TRUE,
           dir = .get_dir_figs(),
           filename = sprintf('%s-%s.gif', game_id, play_id),
           path = file.path(dir, filename),
           # width = 10,
           # height = 10,
           end_pause = 1,
           height = 600,
           width = 600,
           fps = 10,
           ...) {
    
    # game_id = 2018090905; play_id = 783
    # .display_info_var('Plotting {game_id}, {play_id}.')
    .display_info('Plotting `game_id = {game_id}`, `play_id = {play_id}`.')
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
    
    game <- games %>% dplyr::filter(game_id == !!game_id)
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
        dplyr::filter(nfl_id == !!target_id) %>%
        dplyr::distinct(display_name, jersey_number)
      assertthat::assert_that(nrow(target) == 1L)
    }
    
    if(nearest_defender | target_probability) {
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
    }
    
    if(nearest_defender) {
      tracking_clipped <-
        tracking %>%
        clip_tracking_at_events(at = 'end_routes', init_cnd = dplyr::quos(.data$frame_id == 1L)) %>%
        dplyr::arrange(game_id, play_id, nfl_id, frame_id)

      min_dists_nested_init <-
        tracking_clipped %>%
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
    
    if(target_probability) {
      # fit <- file.path(.get_dir_data(), 'fit-tp-final.rds') %>% read_rds()
      # fit
      features <- 
        generate_target_prob_features(
          game_id = game_id,
          play_id = play_id,
          tracking = tracking, 
          plays = plays, 
          players = players
        )
      features_model <- features %>% prep_target_prob_data(all_frames = TRUE)
      probs <- features_model %>% rebind_fit_probs()
    }
    
    line_of_scrimmage <- play$absolute_yardline_number
    play_direction <- tracking$play_direction[[1]]
    sign <- ifelse(play_direction == 'left', -1, 1)
    first_down_line <- line_of_scrimmage + sign * play$yards_to_go
    
    target_tracking <-
      tracking %>%
      dplyr::filter(nfl_id == !!target_id)
    
    nontarget_tracking <-
      tracking %>%
      dplyr::filter(nfl_id != !!target_id)
    
    ball <-
      tracking %>%
      dplyr::distinct(game_id, play_id, frame_id, x = ball_x, y = ball_y) %>%
      dplyr::mutate(nfl_id = NA_integer_)
    
    if(is.null(yardmin) | is.null(yardmax)) {
      yardminmax <-
        tracking %>%
        dplyr::summarize(dplyr::across(x, list(min = min, max = max)))
      
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
        dplyr::summarize(dplyr::across(y, list(min = min, max = max)))
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
      colors <- import_colors()
      home_color <- colors %>% dplyr::filter(team == home_team) %>% dplyr::pull(color)
      away_color <- colors %>% dplyr::filter(team == away_team) %>% dplyr::pull(color)
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
    
    p <-
      tracking %>%
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
      ggplot2::aes(x = x, y = y) +
      ggplot2::geom_segment(
        data = tibble::tibble(x = !!line_of_scrimmage),
        inherit.aes = FALSE,
        ggplot2::aes(x = x, y = 0, xend = x, yend = !!max_y),
        size = 1.25
      ) +
      ggplot2::geom_segment(
        data = tibble::tibble(x = !!first_down_line),
        inherit.aes = FALSE,
        ggplot2::aes(x = x, y = 0, xend = x, yend = !!max_y),
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
        data = nontarget_tracking %>% dplyr::select(-.data$frame_id),
        ggplot2::aes(color = side, group = nfl_id),
        size = 1,
        alpha = 0.5,
        show.legend = FALSE
      ) +
      ggplot2::geom_text(
        data = nontarget_tracking,
        ggplot2::aes(label = jersey_number, color = side),
        show.legend = FALSE,
        fontface = 'bold',
        size = pts(14)
      )
    
    if(has_target) {
      
      p <-
        p +
        ggplot2::geom_path(
          data = target_tracking %>% dplyr::select(-.data$frame_id),
          ggplot2::aes(color = side),
          size = 2,
          alpha = 0.5,
          show.legend = FALSE
        ) +
        ggplot2::geom_text(
          data = target_tracking,
          ggplot2::aes(label = jersey_number, color = side),
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
          ggplot2::aes(x = x_o, y = y_o, xend = x_d, yend = y_d),
          color = 'grey50'
        )
    }
    
    p <-
      p +
      ggplot2::scale_color_manual(values = c('O' = offense_color, 'D' = defense_color))
    
    # Theme stuff
    p <-
      p +
      ggplot2::theme(
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
        title = glue::glue("<b><span style='color:{away_color};'>{away_team}</span></b> @ <b><span style='color:{home_color};'>{home_team}</span></b>, Week {game$week}"),
        # subtitle = '',
        caption = glue::glue('Q{play$quarter}: {play$play_description}<br />
                             Intended receiver: {target$display_name} ({target$jersey_number})<br />
                             game_id = {game$game_id}, play_id = {play$play_id}'),
        x = NULL, y = NULL
      )
    anim <- p + gganimate::transition_manual(frame_id)
    
    if(!save) {
      return(anim)
    }
    
    if(!dir.exists(dirname(path))) {
      dir.create(dirname(path))
    }
    
    seconds <- ball %>% nrow() %>% {. / 10}
    nframe <- (seconds + end_pause) * fps
    
    res <-
      gganimate::animate(
        anim,
        nframe = nframe,
        fps = fps,
        height = height,
        width = width,
        renderer = gganimate::gifski_renderer(path),
        end_pause = end_pause * fps
      )
    res
  }
