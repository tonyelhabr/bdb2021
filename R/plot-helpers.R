
#' @seealso \url(https://stackoverflow.com/a/17313561/120898)
#' @export
pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, "pt"), "mm"))
}

#' Round to multiple of any number.
#'
#' Basically like \code{\link[plyr]{round_any}}
#'
#' @description Round to multiple of any number.
#' @param x value.
#' @param accuracy digits
#' @param f function to use for rounding
#' @seealso \url{http://search.r-project.org/library/plyr/html/round_any.html}
.round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}
