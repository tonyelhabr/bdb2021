
.get_dir <- function() {
  getOption('bdb2021.dir')
}

#' Import position categorization data
#'
#' @description Useful for \code{side} column (either \code{"O"} or \code{"D"})
#' @param dir Directory where file is stored, which is dependent on whether
#' the code is run locally or on Kaggle.
#' Best to change with \code{options(bdb2021.dir = <dir>)} so that other \code{import_}
#' functions have the same change.
#' @seealso \url{https://github.com/leesharpe/nfldata/blob/master/data/positions.csv}
#' @examples
#' \dontrun{
#' import_positions()
#' }
import_positions <- memoise::memoise({function(dir = .get_dir()) {
  path <- file.path(dir, 'positions.csv')
  positions <-
    path %>%
    vroom::vroom(
      skip = 1L,
      progress = FALSE,
      col_names = c('side', 'position_category', 'position_label', 'position'),
      col_types = vroom::cols(
        .default = vroom::col_character()
      )
    )
  positions
}})

#' Import team colors data
#'
#' @inheritParams import_positions
#' @seealso \url{https://github.com/leesharpe/nfldata/blob/master/data/teamcolors.csv}
#' @examples
#' \dontrun{
#' import_colors()
#' }
import_colors <- memoise::memoise({function(dir = .get_dir()) {
  path <- file.path(dir, 'teamcolors.csv')
  colors <-
    path %>%
    vroom::vroom(
      skip = 1L,
      progress = FALSE,
      col_names = c('team', 'color', sprintf('color%s', 2:4)),
      col_types = vroom::cols(
        .default = vroom::col_character()
      )
    )
  colors
}})

#' Import tracking data for a given week
#'
#' @description In addition to importing tracking data, moves \code{"Football"}
#' rows to columns \code{ball_x} and \code{ball_y} and adds \code{side}
#' column using \code{positions} data. If \code{standardize = TRUE},
#' then \code{los} column is also added.
#' @param week Number between 1 and 17
#' @param positions Positions data for adding \code{side} column
#' @param standardize Boolean. Whether to \code{x} and \code{y} based on line of scrimmage (\code{los}) and \code{play_direction}. Note that \code{los} column
#' is added if TRUE.
#' @inheritParams import_positions
#' @examples
#' \dontrun{
#' import_tracking(1)
#' }
import_tracking <- function(week = 1, positions = import_positions(), plays = import_plays(), standardize = TRUE) {
  path <- file.path(.get_dir(), sprintf('week%d.csv', week))
  tracking <-
    path %>%
    vroom::vroom(
      # skip = 1L,
      progress = FALSE,
      col_names = c('time', 'x', 'y', 's', 'a', 'dis', 'o', 'dir', 'event', 'nfl_id', 'display_name', 'jersey_number', 'position', 'frame_id', 'team', 'game_id', 'play_id', 'play_direction', 'route'),
      col_types = vroom::cols(
        time = vroom::col_datetime(format = ''),
        x = vroom::col_double(),
        y = vroom::col_double(),
        s = vroom::col_double(),
        a = vroom::col_double(),
        dis = vroom::col_double(),
        o = vroom::col_double(),
        dir = vroom::col_double(),
        event = vroom::col_character(),
        nfl_id = vroom::col_integer(),
        display_name = vroom::col_character(),
        jersey_number = vroom::col_integer(),
        position = vroom::col_character(),
        frame_id = vroom::col_integer(),
        team = vroom::col_character(),
        game_id = vroom::col_integer(),
        play_id = vroom::col_integer(),
        play_direction = vroom::col_character(),
        route = vroom::col_character()
      )
    )
  tracking <- tracking[-1, ]
  tracking <- tracking[, -1] # Never need the time column
  # tracking <- tracking %>% dplyr::select(-.data$time)

  tracking <-
    tracking %>%
    dplyr::left_join(
      positions %>%
        dplyr::select(.data$position, .data$side),
      by = 'position'
    )

  ball <- tracking %>% dplyr::filter(display_name == 'Football')
  qb <- tracking %>% dplyr::filter(position == 'QB')

  tracking <- tracking %>% dplyr::filter(display_name != 'Football')
  tracking <- tracking %>% dplyr::filter(position != 'QB')

  tracking <-
    tracking %>%
    dplyr::inner_join(
      ball %>%
        dplyr::select(game_id, play_id, frame_id, ball_x = x, ball_y = y),
      by = c('frame_id', 'game_id', 'play_id')
    ) %>%
    dplyr::inner_join(
      ball %>%
        dplyr::select(game_id, play_id, frame_id, qb_x = x, qb_y = y),
      by = c('frame_id', 'game_id', 'play_id')
    )
  tracking

  line_of_scrimmage <-
    tracking %>%
    dplyr::filter(.data$event == 'ball_snap') %>%
    dplyr::group_by(.data$game_id, .data$play_id) %>%
    dplyr::filter(dplyr::row_number() == 1L) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$game_id, .data$play_direction, .data$play_id, los = .data$ball_x) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      plays %>%
        dplyr::select(.data$game_id, .data$play_id, .data$yards_to_go),
      by = c('game_id', 'play_id')
    ) %>%
    dplyr::mutate(fd = .data$los + dplyr::if_else(.data$play_direction == 'left', -1, 1) * .data$yards_to_go) %>%
    dplyr::select(.data$yards_to_go, .data$play_direction)

  tracking <-
    tracking %>%
    dplyr::left_join(line_of_scrimmage, by = c('game_id', 'play_id'))

  if(!standardize) {
    return(tracking)
  }

  x_max <- 120
  y_max <- 160 / 3
  tracking <-
    tracking %>%
    dplyr::mutate(
      dplyr::across(c(.data$x, .data$ball_x, .data$qb_x, .data$los, .data$fd), ~ dplyr::if_else(.data$play_direction == 'left', !!x_max - .x, .x)),
      # Standardizing the x direction based on the los is best for doing general analysis,
      # but perhaps not for plotting.
      # across(c(x, ball_x), ~.x - los),
      dplyr::across(c(.data$y, .data$ball_y, .data$qb_y), ~ dplyr::if_else(.data$play_direction == 'left', !!y_max - .x, .x))
    )
  tracking
}

#' Import games data
#'
#' @inheritParams import_positions
#' @examples
#' \dontrun{
#' import_games()
#' }
import_games <- memoise::memoise({function(dir = .get_dir()) {
  path <- file.path(dir, 'games.csv')
  games <-
    path %>%
    vroom::vroom(
      skip = 1L,
      progress = FALSE,
      col_names = c('game_id', 'game_date', 'game_time_eastern', 'home_team_abbr', 'visitor_team_abbr', 'week'),
      col_types = vroom::cols(
        game_id = vroom::col_integer(),
        game_date = vroom::col_date(format = '%m/%d/%Y'),
        game_time_eastern = vroom::col_character(), # time(format = ''),
        home_team_abbr = vroom::col_character(),
        visitor_team_abbr = vroom::col_character(),
        week = vroom::col_integer()
      )
    )
  games
}})

#' Import players data
#'
#' @inheritParams import_positions
#' @examples
#' \dontrun{
#' import_players()
#' }
import_players <- memoise::memoise({function(dir = .get_dir()) {
  path <- file.path(dir, 'players.csv')
  # TODO: dob needs some fixinig
  players <-
    path %>%
    vroom::vroom(
      skip = 1L,
      progress = FALSE,
      col_names = c('nfl_id', 'height', 'weight', 'dob', 'college', 'position', 'display_name'),
      col_types = vroom::cols(
        .default = vroom::col_character(),
        nfl_id = vroom::col_integer(),
        weight = vroom::col_integer()
      )
    ) # %>%
  # dplyr::select(.data$nfl_id, .data$display_name, .data$position)
  players
}})

.extract_n_position <- function(x, position) {
  rgx <- sprintf('(^.*)([0-9])(\\s%s.*$)', toupper(position))
  x %>%
    stringr::str_replace_all(rgx, '\\2') %>%
    as.integer()
}

.drop_bad_plays <- function(plays) {
  plays %>%
    dplyr::anti_join(
      plays %>%
        dplyr::filter((.data$n_k > 0L | .data$n_p > 0L)) %>%
        dplyr::select(.data$game_id, .data$play_id),
      by = c('game_id', 'play_id')
    )
}

#' Import plays data
#'
#' @inheritParams import_positions
#' @examples
#' \dontrun{
#' import_plays()
#' }
import_plays <- memoise::memoise({function(dir = .get_dir(), drop_bad = FALSE) {
  path <- file.path(dir, 'plays.csv')
  plays <-
    path %>%
    vroom::vroom(
      skip = 1L,
      progress = FALSE,
      col_names = c('game_id', 'play_id', 'play_description', 'quarter', 'down', 'yards_to_go', 'possession_team', 'play_type', 'yardline_side', 'yardline_number', 'offense_formation', 'personnel_o', 'defenders_in_the_box', 'number_of_pass_rushers', 'personnel_d', 'type_dropback', 'pre_snap_visitor_score', 'pre_snap_home_score', 'game_clock', 'absolute_yardline_number', 'penalty_codes', 'penalty_jersey_numbers', 'pass_result', 'offense_play_result', 'play_result', 'epa', 'is_defensive_pi'),
      col_types = vroom::cols(
        .default = vroom::col_integer(),
        yards_to_go = vroom::col_integer(),
        play_description = vroom::col_character(),
        possession_team = vroom::col_character(),
        play_type = vroom::col_character(),
        yardline_side = vroom::col_character(),
        offense_formation = vroom::col_character(),
        personnel_o = vroom::col_character(),
        personnel_d = vroom::col_character(),
        type_dropback = vroom::col_character(),
        game_clock = vroom::col_time(format = ''),
        penalty_codes = vroom::col_character(),
        penalty_jersey_numbers = vroom::col_character(),
        pass_result = vroom::col_character(),
        epa = vroom::col_double(),
        is_defensive_pi = vroom::col_logical()
      )
    )
  path <- file.path(dir, 'targetedReciever.csv')
  target <-
    path %>%
    vroom::vroom(
      skip = 1L,
      progress = FALSE,
      col_names = c('game_id', 'play_id', 'target_nfl_id'),
      col_types = vroom::cols(
        .default = vroom::col_integer()
      )
    )
  plays <-
    plays %>%
    dplyr::inner_join(target, by = c('game_id', 'play_id'))
  plays

  suppressWarnings(
    plays <-
      plays %>%
      dplyr::mutate(
        dplyr::across(
          .data$personnel_o,
          list(
            n_rb = ~.extract_n_position(.x, 'rb'),
            n_wr = ~.extract_n_position(.x, 'wr'),
            n_te = ~.extract_n_position(.x, 'te'),
            n_qb = ~.extract_n_position(.x, 'qb'),
            n_ol = ~.extract_n_position(.x, 'ol'),
            n_p = ~.extract_n_position(.x, 'p'),
            n_k = ~.extract_n_position(.x, 'k'),
            n_ls = ~.extract_n_position(.x, 'ls'),
            n_dl_o = ~.extract_n_position(.x, 'dl'),
            n_lb_o = ~.extract_n_position(.x, 'lb'),
            n_db_o = ~.extract_n_position(.x, 'db')
          ),
          .names = '{fn}'
        ),
        dplyr::across(
          .data$personnel_d,
          list(
            n_dl = ~.extract_n_position(.x, 'dl'),
            n_lb = ~.extract_n_position(.x, 'lb'),
            n_db = ~.extract_n_position(.x, 'db'),
            n_rb_d = ~.extract_n_position(.x, 'rb'),
            n_wr_d = ~.extract_n_position(.x, 'wr'),
            n_te_d = ~.extract_n_position(.x, 'te')
          ),
          .names = '{fn}'
        )
      )
  )

  if(!drop_bad) {
    return(plays)
  }
  plays %>% .drop_bad_plays()

}})

#' Import \code{nflfastR} play-by-play data
#'
#' @param season 2018 by default.
#'
#' @examples
#' \dontrun{
#' import_nflfastr_pbp()
#' }
import_nflfastr_pbp <- memoise::memoise({function(season = 2018) {
  sprintf('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_%s.rds', season) %>%
    url() %>%
    readr::read_rds() %>%
    dplyr::rename(new_game_id = .data$game_id) %>%
    dplyr::rename(game_id = .data$old_game_id) %>%
    dplyr::mutate(dplyr::across(c(.data$game_id, .data$play_id), as.integer))
}})
