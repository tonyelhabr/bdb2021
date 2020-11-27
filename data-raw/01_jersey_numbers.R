
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
