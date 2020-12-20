
do_import_routes <- function(week = 1L, ...) {

  .display_info('Importing tracking for {week} at {Sys.time()}.')
  tracking <- week %>% import_tracking(standardize = FALSE)
  routes <-
    tracking %>%
    dplyr::filter(!is.na(.data$route)) %>%
    dplyr::distinct(
      .data$game_id, .data$play_id, .data$nfl_id, .data$route
    )
}

routes <- do_by_week(weeks = 1:17L, f = do_import_routes)

# usethis::use_data(routes, internal = TRUE, overwrite = TRUE)
usethis::use_data(routes, overwrite = TRUE)
