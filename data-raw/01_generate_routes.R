
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

usethis::use_data(routes, overwrite = TRUE)

# tracking
# tracking_events_frames <-
#   tracking %>%
#   filter(event != 'None') %>%
#   distinct(game_id, play_id, event, frame_id) %>%
#   pivot_wider(names_from = event, values_from = frame_id, values_fn = dplyr::first)
# tracking_events_frames %>%
#   filter(!is.na(pass_arrived)) %>%
#   select(game_id, play_id, pass_arrived, matches('^pass_outcome_'), -pass_outcome_touchdown) %>%
#   janitor::remove_empty(which = c('cols')) %>%
#   # inner_join(plays %>% select(game_id, play_id, play_description))
#   pivot_longer(-c(game_id, play_id, pass_arrived)) %>%
#   drop_na() %>%
#   mutate(diff = value - pass_arrived) %>%
#   group_by(name) %>%
#   summarize(across(diff, list(mean = mean, min = min, max = max), .names = '{fn}'), n = n()) %>%
#   ungroup()
# 
# tracking_events_frames %>%
#   filter((!is.na(pass_tipped))) %>% 
#   janitor::remove_empty(which = c('cols'))
# 
# pass_thrown_but_no_outcome <-
#   tracking_events_frames %>%
#   filter((!is.na(pass_forward) | !is.na(pass_shovel)) & (is.na(pass_outcome_caught) & is.na(pass_outcome_incomplete) & is.na(pass_outcome_interception) & is.na(pass_outcome_touchdown))) %>% 
#   janitor::remove_empty(which = c('cols'))
# pass_thrown_but_no_outcome
# 
# pass_thrown_but_no_outcome_or_arrival <-
#   tracking_events_frames %>%
#   filter((!is.na(pass_forward) | !is.na(pass_shovel)) & (is.na(pass_arrived) & is.na(pass_outcome_caught) & is.na(pass_outcome_incomplete) & is.na(pass_outcome_interception) & is.na(pass_outcome_touchdown))) %>% 
#   janitor::remove_empty(which = c('cols'))
# pass_thrown_but_no_outcome_or_arrival
