
.get_events_throw <- memoise::memoise({function() {
  c(
    'pass_forward',
    'pass_shovel'
  )
}})

.get_events_end_routes <- memoise::memoise({function() {
  c(
    sprintf(
      'pass_outcome_%s',
      c('caught', 'incomplete', 'interception', 'touchdown')
    ),
    sprintf(
      'qb_%s',
      c('sack', 'strip_sack', 'spike')
    )
  )
}})

.get_events_end_rush <- memoise::memoise({function() {
  c(
    sprintf(
      'pass_%s',
      c('forward', 'shovel')
    ),
    sprintf(
      'qb_%s',
      c('sack', 'strip_sack', 'spike')
    )
  )
}})

.get_events_recoder <- memoise::memoise({function() {
  events_throw <- .get_events_throw()
  events_end_routes <- .get_events_end_routes()
  events_end_rush <- .get_events_end_rush()
  tibble(
    event = c('ball_snap', events_throw, events_end_rush, 'pass_arrived', events_end_routes),
    event_relabeled = c(
      'snap',
      rep('throw', length(events_throw)),
      rep('endrush', length(events_end_rush)),
      'arrival',
      rep('endroutes', length(events_end_routes))
    )
  )
  # tibble::tribble(
  #                      ~event, ~event_relabeled, ~event_order,
  #                 "ball_snap",           "snap",           1L,
  #              "pass_forward",          "throw",           2L,
  #               "pass_shovel",          "throw",           2L,
  #              "pass_forward",        "endrush",           3L,
  #               "pass_shovel",        "endrush",           3L,
  #                   "qb_sack",        "endrush",           3L,
  #             "qb_strip_sack",        "endrush",           3L,
  #                  "qb_spike",        "endrush",           3L,
  #              "pass_arrived",        "arrival",           4L,
  #       "pass_outcome_caught",      "endroutes",           5L,
  #   "pass_outcome_incomplete",      "endroutes",           5L,
  # "pass_outcome_interception",      "endroutes",           5L,
  #    "pass_outcome_touchdown",      "endroutes",           5L,
  #                   "qb_sack",      "endroutes",           5L,
  #             "qb_strip_sack",      "endroutes",           5L,
  #                  "qb_spike",      "endroutes",           5L
  # )
}})


.get_notable_events <- memoise::memoise({function() {
  c(
    'ball_snap',
    'pass_forward',
    'pass_shovel',
    'pass_arrived',
    sprintf(
      'pass_outcome_%s',
      c('caught', 'incomplete', 'interception', 'touchdown')
    ),
    sprintf(
      'qb_%s',
      c('sack', 'strip_sack', 'spike')
    )
  )
}})

filter_notable_events <- function(data, events = .get_notable_events()) {
  data %>%
    filter(.data$event %in% !!events)
}


relabel_events <- function(data) {
  events_recoder <- .get_events_recoder()
  data %>%
    dplyr::left_join(events_recoder, by = 'event')
}
