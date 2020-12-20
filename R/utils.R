
#' Pick route cutoff
#'
#' Threshold (in seconds) beyond which receiver intersections are no longer consider picks
#' @export
get_intersection_cutoff <- memoise::memoise({function() {
  2.0
}})

#' Binary 1 and 0 to TRUE and FALSE
#'
#' Convert numerics to logicals, mostly for modeling purposes.
#' 
#' @param x factor
#' @export
binary_fct_to_lgl <- function(x) {
  x %>% as.integer() %>% {. - 1L} %>% as.logical()
}

#' Convert factor to integer
#'
#' @param x factor
#' @export
binary_fct_to_int <- function(x) {
  x %>% as.integer() %>% {. - 1L}
}
