
#' Add \code{x_side} column
#'
#' @description If player's \code{x} is less than \code{abs(ball_x - cutoff)}
#' (accounting for \code{play_direction}), then \code{x_side} is
#' \code{"los"} (line of scrimmage).
#' Otherwise, it is \code{"backfield"}.
#' @param data Tracking data
#' @param cutoff Yards behind ball (towards backfield) within which to consider \code{"los"} (line of scrimmage).
#' @examples
#' \dontrun{
#' library(bdb2021)
#' library(tidyverse)
#' tracking <- import_tracking(1)
#' snap_frames <- tracking %>% filter(event == 'ball_snap')
#' # Used this to determine `cutoff` for `x_side`
#' snap_frames %>%
#'   filter(position == 'RB') %>%
#'   inner_join(plays) %>%
#'   mutate(x_relative = x - ball_x) %>%
#'   ggplot() +
#'   aes(x = x_relative) +
#'   geom_histogram(binwidth = 1) +
#'   facet_grid(offense_formation~play_direction)
#' }
add_x_side_col <- function(data, cutoff = 2.5) {
  data %>%
    # mutate(
    #   x_side =
    #     case_when(
    #       play_direction == 'left' & (x + !!cutoff) > ball_x ~ 'backfield',
    #       play_direction == 'right' & (x + !!cutoff) < ball_x ~ 'backfield',
    #       TRUE ~ NA_character_
    #     )
    # )
    dplyr::mutate(
      x_side =
        dplyr::if_else(
          (.data$x + !!cutoff) < .data$ball_x,
          'backfield',
          'los'
        )
    )
}

#' Add \code{y_side} column
#'
#' @description If player's \code{y} is less than or more than \code{abs(ball_y - cutoff)}
#' (accounting for \code{play_direction}), then \code{y_side} is
#' \code{"left"} or \code{"right"}.
#' Otherwise, it is \code{"mid"}.
#' @param data Tracking data
#' @param cutoff Yards horizontally along same of plane of ball to consider \code{"mid"}.
#' @examples
#' \dontrun{
#' library(bdb2021)
#' library(tidyverse)
#' tracking <- import_tracking(1)
#' snap_frames <- tracking %>% filter(event == 'ball_snap')
#' # Used this to determine `cutoff` for `y_side`
#' snap_frames %>%
#'   filter(position == 'TE') %>%
#'   inner_join(plays) %>%
#'   mutate(
#'     y_relative = y - ball_y,
#'     across(y_relative, ~cut(.x, breaks = seq(-8, 8, by = 0.5)))
#'   ) %>%
#'   ggplot() +
#'   aes(x = y_relative) +
#'   geom_bar() +
#'   facet_grid(offense_formation~.)
#' }
add_y_side_col <- function(data, cutoff = 3) {
  data %>%
    dplyr::mutate(
      y_side =
        dplyr::case_when(
          .data$play_direction == 'left' & (.data$y + !!cutoff) < .data$ball_y ~ 'left',
          .data$play_direction == 'left' & (.data$y - !!cutoff) >= .data$ball_y ~ 'right',
          .data$play_direction == 'left' ~ 'mid',
          .data$play_direction == 'right' & (.data$y + !!cutoff) <= .data$ball_y ~ 'right',
          .data$play_direction == 'right' & (.data$y - !!cutoff) >= .data$ball_y ~ 'left',
          .data$play_direction == 'right' ~ 'mid',
          TRUE ~ NA_character_
        )
    )
}

#' Add a \code{x_side} and \code{y_side} columns
#'
#' @param data Tracking data
#' @param cutoff_x \code{cutoff} for \code{add_x_side()}
#' @param cutoff_y \code{cutoff} for \code{add_y_side()}
#' @seealso \code{\link{add_x_side}} \code{\link{add_y_side}}
add_side_cols <- function(data, cutoff_x = 2.5, cutoff_y = 3) {
  data %>%
    add_x_side_col(cutoff = cutoff_x) %>%
    add_y_side_col(cutoff = cutoff_y)
}

#' Drop receivers ineligible for pick routes
#'
#' @description Drop frames that do not meet these criteria:
#' \itemize{
#'   \item offense
#'   \item has non-\code{NA} route
#'   \item \code{y_side != "mid"}
#'   \item \code{y_side != "backfield"}
#' }
#' \code{x_side} and \code{y_side} columns are expected
#' to already exist in \code{data}
#' (so \code{add_x_side} and \code{add_y_side} functions should be called before).
#' @param data Tracking data with \code{x_side} and \code{y_side} columns
drop_ineligible_pick_route_frames <- function(data) {
  data %>%
    dplyr::filter(.data$side == 'O' & !is.na(.data$route) & .data$y_side != 'mid' & .data$x_side != 'backfield')
}

#' Add \code{idx_y} column
#'
#' @description Add \code{idx_y} column describing receiver's position relative
#' to the sideline on one side of the field. \code{y_side} column is expected
#' to already exist in \code{data}
#' (so \code{add_y_side_col} should have already been called).
#' @param data Tracking data with \code{y_side} column
add_idx_y_col <- function(data) {
  data %>%
    dplyr::group_by(.data$game_id, .data$play_id, .data$frame_id, .data$y_side) %>%
    dplyr::mutate(
      idx_y =
        dplyr::case_when(
          .data$y_side == 'left' ~ dplyr::row_number(y),
          .data$y_side == 'right' ~ dplyr::row_number(-y),
          TRUE ~ NA_integer_
        )
    ) %>%
    dplyr::ungroup()
}

