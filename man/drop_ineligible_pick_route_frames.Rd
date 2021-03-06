% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/side_cols.R
\name{drop_ineligible_pick_route_frames}
\alias{drop_ineligible_pick_route_frames}
\title{Drop receivers ineligible for pick routes}
\usage{
drop_ineligible_pick_route_frames(data)
}
\arguments{
\item{data}{Tracking data with \code{x_side} and \code{y_side} columns}
}
\description{
Drop frames that do not meet these criteria:
\itemize{
\item offense
\item has non-\code{NA} route
\item \code{y_side != "mid"}
\item \code{y_side != "backfield"}
}
\code{x_side} and \code{y_side} columns are expected
to already exist in \code{data}
(so \code{add_x_side()} and \code{add_y_side()} functions should be called before).
}
