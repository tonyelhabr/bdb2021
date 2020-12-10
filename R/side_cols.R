
#' Add `x_side` column
#'
#' If player's `x` is less than `abs(ball_x - cutoff)`
#' (accounting for `play_direction`), then `x_side` is
#' `"los"` (line of scrimmage). Otherwise, it is `"backfield"`.
#'
#' @param data Tracking data
#' @param cutoff Yards behind ball (towards backfield) within which to consider `"los"` (line of scrimmage).
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
    dplyr::mutate(
      x_side =
        dplyr::if_else(
          (.data$x + !!cutoff) < .data$ball_x,
          'backfield',
          'los'
        )
    )
}

#' Add `y_side` column
#'
#' If player's `y` is less than or more than `abs(ball_y - cutoff)`
#' (accounting for `play_direction`), then `y_side` is
#' `"left"`or `"right"`. Otherwise, it is `"mid"`.
#'
#' @param data Tracking data
#' @param cutoff Yards horizontally along same of plane of ball to consider `"mid"`.
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

#' Add a `x_side` and `y_side` columns
#'
#' @param data Tracking data
#' @param cutoff_x `cutoff` for `add_x_side()`
#' @param cutoff_y `cutoff` for `add_y_side()`
#' @export
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
#'   \item has non-`NA` route
#'   \item `y_side != "mid"`
#'   \item `y_side != "backfield"`
#' }
#' `x_side` and `y_side` columns are expected
#' to already exist in `data`
#' (so `add_x_side()` and `add_y_side()` functions should be called before).
#' @param data Tracking data with `x_side` and `y_side` columns
#' @export
drop_ineligible_pick_route_frames <- function(data) {
  data %>%
    dplyr::filter(.data$side == 'O' & !is.na(.data$route) & .data$y_side != 'mid' & .data$x_side != 'backfield')
}

