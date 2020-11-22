
library(tidyverse)
sched <- nflfastR::fast_scraper_schedules(2018)
game_ids <- sched %>% dplyr::pull(.data$game_id)
game_ids <- game_ids[249:length(game_ids)]
suppressWarnings({
  pbp_init_nflfastr <-
    purrr::map_dfr(game_ids, ~{
      .display_info('Scraping `game_id = {.x}`.')
      nflfastR:::get_pbp_nfl(.x)
    })
})

pbp_init_nflfastr <- pbp_init_nflfastr %>% distinct()
arrow::write_parquet(pbp_init_nflfastr, file.path('inst', 'pbp_init_nflfastr.parquet'))
# usethis::use_data(pbp_init_nflfastr, overwrite = TRUE)
