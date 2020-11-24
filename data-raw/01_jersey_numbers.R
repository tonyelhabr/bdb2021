
do_import_players_from_tracking <- function(week = 1L, ...) {

  .display_info('Importing tracking for {week} at {Sys.time()}.')
  tracking <- week %>% import_tracking(standardize = FALSE)
  players_from_tracking <-
    tracking %>%
    dplyr::distinct(
      .data$game_id, .data$nfl_id, .data$position, .data$display_name, .data$jersey_number
    )
}

players_from_tracking <- do_by_week(weeks = 1:17L, f = do_import_players_from_tracking)
# Some strange cases where a player has more than one jersey number in a single game.
players_from_tracking <- players_from_tracking %>% distinct(game_id, nfl_id, .keep_all = TRUE)
usethis::use_data(players_from_tracking, overwrite = TRUE)
