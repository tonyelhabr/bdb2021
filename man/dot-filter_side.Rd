% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/animate_play.R
\name{.filter_side}
\alias{.filter_side}
\title{Filter by team}
\usage{
.filter_side(data, side = c("o", "d"))
}
\arguments{
\item{data}{data.frame with \code{side} column that is either \code{"O"} or \code{"D"}}

\item{side}{Either \code{"o"} or \code{"d"}. (It's converted to upper in the function.)}
}
\description{
Filter to one side of the ball (offense or defense).
}
\details{
This should only be used if you know what you're doing.
}
