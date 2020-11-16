
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.bdb2021 <- list(
    bdb2021.dir = '../oh_snap/data',
    bdb2021.verbose = TRUE
  )
  toset <- !(names(op.bdb2021) %in% names(op))
  if(any(toset)) options(op.bdb2021[toset])

  invisible()
}
