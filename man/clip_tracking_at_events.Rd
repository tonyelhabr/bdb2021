% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clip_tracking.R
\name{clip_tracking_at_events}
\alias{clip_tracking_at_events}
\title{Clip tracking data at events}
\usage{
clip_tracking_at_events(
  tracking,
  at = "throw",
  init_cnd = dplyr::quos(.data$event == "ball_snap")
)
}
\arguments{
\item{tracking}{Tracking data frame}

\item{at}{character vector indicating which \code{event} at which to clip each play}

\item{init_cnd}{a quosure for a filtering condition used to start the clip. This is ball snap by default. Only change this if you know what you're doing.}
}
\description{
Clip tracking data at events
}
