% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import.R
\name{import_colors}
\alias{import_colors}
\title{Import team colors data}
\usage{
import_colors(dir = .get_dir())
}
\arguments{
\item{dir}{Directory where file is stored, which is dependent on whether
the code is run locally or on Kaggle.
Best to change with \code{options(bdb2021.dir = <dir>)} so that other \code{import_}
functions have the same change.}
}
\description{
Import team colors data
}
\examples{
\dontrun{
import_colors()
}
}
\seealso{
\url{https://github.com/leesharpe/nfldata/blob/master/data/teamcolors.csv}
}