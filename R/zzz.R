
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.bdb2021 <- list(
    bdb2021.dir_in = '../oh_snap/data',
    bdb2021.dir_data = '../bdb2021-data/data',
    bdb2021.dir_figs = '../bdb2021-data/figs',
    bdb2021.verbose = TRUE
  )
  toset <- !(names(op.bdb2021) %in% names(op))
  if(any(toset)) options(op.bdb2021[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  if (.Platform$OS.type == 'windows')  { # nocov start
    if (interactive()) packageStartupMessage('Registering Windows fonts with R')
    extrafont::loadfonts('win', quiet = TRUE)
  }
}