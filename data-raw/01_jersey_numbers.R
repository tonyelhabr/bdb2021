
do_import_jersey_numbers <- function(week = 1L, ...) {

  .display_info('Importing tracking for {week} at {Sys.time()}.')
  tracking <- week %>% import_tracking(standardize = FALSE)
  jersey_numbers <-
    tracking %>%
    dplyr::distinct(
      .data$game_id, .data$nfl_id, .data$position, .data$display_name, .data$jersey_number
    )
}

jersey_numbers <- do_by_week(weeks = 1:17L, f = do_import_jersey_numbers)
# Some strange cases where a player has more than one jersey number in a single game.
jersey_numbers <- jersey_numbers %>% distinct(game_id, nfl_id, .keep_all = TRUE)
usethis::use_data(jersey_numbers, overwrite = TRUE)

do_import_n_player <- function(week = 1L, ...) {

  .display_info('Importing tracking for {week} at {Sys.time()}.')
  tracking <- week %>% import_tracking(standardize = FALSE)
  n_player <-
    tracking %>%
    dplyr::distinct(
      .data$game_id, .data$play_id, .data$nfl_id, .data$position, .data$display_name
    )
}

n_player <- do_by_week(weeks = 1:17L, f = do_import_n_player)
# Some strange cases where a player has more than one jersey number in a single game.
n_player_agg <-
  n_player %>%
  inner_join(positions %>% select(position, side)) %>%
  filter(side == 'D') %>%
  count(game_id, play_id) %>%
  count(n, name = 'nn') %>%
  mutate(`%` = scales::percent(nn / sum(nn), accuracy = 0.1))
n_player_agg
