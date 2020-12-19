
do_import_players_from_tracking <- function(week = 1L, ...) {

  .display_info('Importing tracking for {week} at {Sys.time()}.')
  tracking <- week %>% import_tracking(standardize = FALSE)
  n_player <-
    tracking %>%
    dplyr::distinct(
      .data$game_id, .data$play_id, .data$nfl_id, .data$position, .data$display_name, .data$jersey_number
    )
}

players_from_tracking <- do_by_week(weeks = 1:17L, f = do_import_players_from_tracking)
players_from_tracking
# plays <- import_plays()
# plays %>% filter(is.na(personnel_o)) %>% select(-n_qb) %>% inner_join(personnel_and_rushers) %>% relocate(n_qb, n_o_nonqb, n_route, n_d, n_rusher)
#
# plays %>%
#   filter(is.na(personnel_o)) %>%
#   inner_join(players_from_tracking)

usethis::use_data(players_from_tracking, overwrite = TRUE)
