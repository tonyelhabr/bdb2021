
#' @export
save_plot <-
  function(gg,
           file = deparse(substitute(gg)),
           ext = 'png',
           dir = get_bdb_dir_figs(),
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
#' @export
plot_play <-
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
           at = 'end_routes',
           save = TRUE,
           dir = get_bdb_dir_figs(),
           ext = 'png',
           filename = sprintf('%s-%s.%s', game_id, play_id, ext),
           path = file.path(dir, filename),
           subtitle = NULL,
           width = 10,
           height = 10,
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
      warning('No target receiver.', call. = FALSE)
      target <- tibble::tibble(display_name = '?', jersey_number = -1)
    } else {
      target <-
        tracking %>%
        filter(nfl_id == !!target_id) %>%
        distinct(display_name, jersey_number)
      assertthat::assert_that(nrow(target) == 1L)
    }

    tracking_clipped <-
      tracking %>%
      clip_tracking_at_events(at = at) %>%
      dplyr::arrange(game_id, play_id, nfl_id, frame_id)

    snap_frames <- tracking_clipped %>% dplyr::filter(frame_id == min(frame_id))
    notable_frames <- tracking_clipped %>% filter_notable_events()
    end_frames <- tracking_clipped %>% dplyr::filter(frame_id == max(frame_id))
    frames <- dplyr::bind_rows(snap_frames, end_frames)

    line_of_scrimmage <- play$absolute_yardline_number
    play_direction <- tracking$play_direction[[1]]
    sign <- ifelse(play_direction == 'left', -1, 1)
    first_down_line <- line_of_scrimmage + sign * play$yards_to_go

    target_tracking_clipped <-
      tracking_clipped %>%
      dplyr::filter(nfl_id == !!target_id)

    nontarget_tracking_clipped <-
      tracking_clipped %>%
      dplyr::filter(nfl_id != !!target_id)

    ball <-
      notable_frames %>%
      dplyr::distinct(game_id, play_id, frame_id, x = ball_x, y = ball_y) %>%
      dplyr::mutate(nfl_id = NA_integer_)

    if(is.null(yardmin) | is.null(yardmax)) {
      yardminmax <-
        tracking_clipped %>%
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
        tracking_clipped %>%
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
      frames %>%
      ggplot2::ggplot() +
      gg_field(
        yardmin = yardmin,
        yardmax = yardmax,
        buffer = buffer,
        field_color = field_color,
        line_color = line_color,
        sideline_color = sideline_color,
        ...
      ) +
      ggplot2::aes(x = x, y = y, group = nfl_id) +
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
        data = nontarget_tracking_clipped,
        ggplot2::aes(color = side),
        size = 1,
        alpha = 0.2,
        show.legend = FALSE
      )

    if(has_target) {

      p <-
        p +
        ggplot2::geom_path(
          data = target_tracking_clipped,
          aes(color = side),
          size = 2,
          alpha = 0.2,
          show.legend = FALSE
        )
    }

    p <-
      p +
      ggplot2::geom_text(
        data = snap_frames,
        ggplot2::aes(label = jersey_number, color = side),
        show.legend = FALSE,
        fontface = 'bold',
        size = pts(14)
      )

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
        subtitle = subtitle,
        title = glue::glue("<b><span style='color:{away_color};'>{away_team}</span></b> @ <b><span style='color:{home_color};'>{home_team}</span></b>, Week {game$week}"),
        # subtitle = '',
        caption = glue::glue('Q{play$quarter}: {play$play_description}<br />
                             Intended receiver: {target$display_name} ({target$jersey_number})<br />
                             game_id = {game$game_id}, play_id = {play$play_id}'),
        x = NULL, y = NULL
      )
    # print(p)

    if(!save) {
      return(p)
      # return(tracking)
    }

    if(!dir.exists(dirname(path))) {
      dir.create(dirname(path))
    }

    ggplot2::ggsave(
      plot = p,
      filename = path,
      width = width,
      height = height,
      type = 'cairo'
    )
    # tracking
  }