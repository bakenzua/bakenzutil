% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fill_column_within_time_window.R
\name{fill_column_within_time_window}
\alias{fill_column_within_time_window}
\title{fill_column_within_time_window}
\usage{
fill_column_within_time_window(
  df,
  group_by_col,
  time_col,
  fill_col,
  window_length_hours = 2,
  fill_direction = "down"
)
}
\arguments{
\item{df}{Dataframe to fill column within}

\item{group_by_col}{Column to group by}

\item{time_col}{Column of Time type to sort by within groups}

\item{fill_col}{Column to fill}

\item{window_length_hours}{Maximum length of fill}

\item{fill_direction}{Fill direction as per tidyr::fill definitions}
}
\value{
full_query
}
\description{
fill_column_within_time_window uses tidyr::fill to fill a column within a time window.
}
