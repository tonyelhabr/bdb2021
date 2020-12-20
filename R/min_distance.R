
#' Calculate Euclidean distance
#'
#' @param x1,x2,y1,y2 Numeric values representing coordinates
#' @noRd
.dist <- function(x1, x2, y1, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

#' Coerce to matrix
#'
#' Used by distance calculation functions such as `identify_nearby_players()`.
#'
#' @param data A data.frame with 3 columns: `nfl_id`, `x`, `y`.
#' @noRd
.coerce_to_mat <- function(data) {
  res <-
    data %>%
    dplyr::select(.data$x, .data$y) %>%
    as.matrix()
  rownames(res) <- data$nfl_id
  res
}

#' Compute min distances between players.
#'
#' Compute "optimal" nearest assignments of defensive players to individual offensive and defensive players (not including QB) in a single frame using the Hungarian method.
#'
#' @param o,d A data.frame with 3 columns: `nfl_id`, `x`, `y`.
#' @param one_pass A boolean indicating whether to pass through defenders just once (for a one-to-one assignment), probably leaving some defenders without a matched offensive player. If `FALSE`, two passes are done (to assigned double coverers.
#' @return A tibble with 6 columns: `nfl_id_o`, `nfl_id_d`, `x_o`, `x_d`, `y_o`, `y_d`.
#' @export
compute_min_distances <- function(o, d, one_pass = TRUE) {
  o_mat <- o %>% .coerce_to_mat()
  d_mat <- d %>% .coerce_to_mat()
  dists <- fields::rdist(o_mat, d_mat)
  rows <- rownames(o_mat)
  cols <- rownames(d_mat)
  rownames(dists) <- rows
  colnames(dists) <- cols
  cols_idx_min <- clue::solve_LSAP(dists, maximum = FALSE)
  cols_min <- cols[cols_idx_min]
  cols_leftover <- cols[setdiff(1:length(cols), cols_idx_min)]
  if(one_pass) {

    pairs <-
      tibble::tibble(
        nfl_id_o = rows,
        nfl_id_d = cols_min,
        idx_closest = c(rep(1L, length(rows)))
      ) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with('nfl_id'), as.integer))

  } else if(length(cols_leftover) <= length(rows)) {
    rows_idx_min_leftover <- clue::solve_LSAP(t(dists[, cols_leftover]), maximum = FALSE)
    rows_min_leftover <- rows[rows_idx_min_leftover]
    pairs <-
      tibble::tibble(
        nfl_id_o = c(rows, rows_min_leftover),
        nfl_id_d = c(cols_min, cols_leftover),
        idx_closest = c(rep(1L, length(rows)), rep(2L, length(cols_leftover)))
      ) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with('nfl_id'), as.integer))
  } else {
    # This is reached if the number of defenders is more than 2x the number of potential pass catchers
    cols_idx_min_leftover <- clue::solve_LSAP(dists[, cols_leftover], maximum = FALSE)
    cols_min_leftover <- cols_leftover[cols_idx_min_leftover]
    pairs <-
      tibble::tibble(
        nfl_id_o = c(rows, rows),
        nfl_id_d = c(cols_min, cols_min_leftover),
        idx_closest = c(rep(1L, length(rows)), rep(2L, length(cols_min_leftover)))
      ) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with('nfl_id'), as.integer))
  }

  dists_tidy <-
    dists %>%
    tibble::as_tibble(rownames = 'nfl_id_o') %>%
    tidyr::pivot_longer(-c(.data$nfl_id_o), names_to = 'nfl_id_d', values_to = 'dist') %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with('nfl_id'), as.integer)) %>%
    dplyr::inner_join(o %>% dplyr::rename_all(~sprintf('%s_o', .x)), by = 'nfl_id_o') %>%
    dplyr::inner_join(d %>% dplyr::rename_all(~sprintf('%s_d', .x)), by = 'nfl_id_d')

  res <-
    dists_tidy %>%
    dplyr::inner_join(pairs, by = c('nfl_id_o', 'nfl_id_d'))
  res
}
