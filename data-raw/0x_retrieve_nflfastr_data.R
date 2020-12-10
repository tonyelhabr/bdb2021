
# Basically `fast_scraper` from `{nflfastR}` (https://github.com/mrcaseb/nflfastR/blob/master/R/top-level_scraper.R)
# TODO: Put this into 2 separate scripts
# Why do I have this again? I think it was to isolate the ep part?
library(tidyverse)
path_pbp_nflfastr <- file.path(.get_dir_data(), 'pbp_init_nflfastr.parquet')

# ----
sched <- nflfastR::fast_scraper_schedules(2018)
game_ids <- sched %>% dplyr::pull(.data$game_id)
# game_ids <- game_ids[249:length(game_ids)]
suppressWarnings({
  pbp_init_nflfastr <-
    purrr::map_dfr(game_ids, ~{
      .display_info('Scraping `game_id = {.x}`.')
      nflfastR:::get_pbp_nfl(.x)
    })
})

pbp_init_nflfastr <- pbp_init_nflfastr %>% distinct()
arrow::write_parquet(pbp_init_nflfastr, path_pbp_nflfastr)

# ----
pbp_init_nflfastr <- path_pbp_nflfastr %>% arrow::read_parquet()
pbp_nflfastr <-
  pbp_init_nflfastr %>%
  nflfastR:::add_game_data() %>%
  nflfastR:::add_nflscrapr_mutations() %>%
  nflfastR:::add_ep() %>%
  nflfastR:::add_air_yac_ep() %>%
  nflfastR:::add_wp() %>%
  nflfastR:::add_air_yac_wp() %>%
  nflfastR:::add_cp() %>%
  nflfastR:::add_drive_results() %>%
  nflfastR:::add_series_data() %>%
  nflfastR:::select_variables()
pbp_nflfastr

pbp_nflfastr$ep
pbp_nflfastr$epa
