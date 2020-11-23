
#' Identify route intersection
#'
#' @description Identify if two routes intersect.
#' @param data Tracking data for a single play
#' @param pad_backwards Boolean for whether to extend route backwards
#' @param pad_x if \code{pad_backwards = TRUE}, this is the number of yards to extend the route
#' @examples
#' \dontrun{
#' identify_intersection()
#' }
identify_intersection <- function(data, pad_backwards = TRUE, pad_x = 1) {
  nfl_ids <- data %>% dplyr::distinct(nfl_id) %>% dplyr::pull(nfl_id)
  if(length(nfl_ids) <= 1L) {
    return(tibble::tibble())
  }

  if(pad_backwards) {
    first_x <-
      data %>%
      dplyr::filter(.data$frame_id == min(.data$frame_id))
    padded_x <-
      first_x %>%
      dplyr::mutate(
        frame_id = .data$frame_id - 5L,
        x = .data$x - !!pad_x
      )
    data <-
      dplyr::bind_rows(padded_x, data)
  }

  data_trans <-
    data %>%
    tidyr::nest(data = -c(.data$nfl_id)) %>%
    dplyr::mutate(
      line =
        purrr::map(
          data,
          ~dplyr::select(.x, x, y) %>%
            as.matrix() %>%
            sf::st_linestring()
        )
    ) %>%
    dplyr::select(nfl_id, line)
  # browser()
  line1 <- data_trans %>% slice(2) %>% pull(line) %>% pluck(1)
  line2 <- data_trans %>% slice(3) %>% pull(line) %>% pluck(1)
  sf::st_intersects(line1, line2) %>% as.integer()
  sf::st_intersection(line1, line2) %>% as.numeric()
  sf::st_join(line1, line2)
  # 2541316
  # 2543646
  res <-
    tidyr::crossing(
      nfl_id = nfl_ids,
      nfl_id_intersect = nfl_ids
    ) %>%
    dplyr::filter(.data$nfl_id != .data$nfl_id_intersect) %>%
    dplyr::inner_join(data_trans, by = 'nfl_id') %>%
    dplyr::inner_join(
      data_trans %>%
        dplyr::rename_all(~sprintf('%s_intersect', .x)),
      by = 'nfl_id_intersect'
    ) %>%
    dplyr::mutate(
      intersection =
        purrr::map2_int(
          line, line_intersect,
          ~sf::st_intersects(..1, ..2) %>%
            as.integer()
        ),
      has_intersection = purrr::map_lgl(.data$intersection, ~.x > 0L)
    ) %>%
    dplyr::filter(.data$has_intersection)
  res

  if(nrow(res) == 0L) {
    return(tibble::tibble())
  }

  res <-
    res %>%
    dplyr::select(.data$nfl_id, .data$nfl_id_intersect)
  res
}
