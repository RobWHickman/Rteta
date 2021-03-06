% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standardize_coordinates.R
\name{standardize_opta_y}
\alias{standardize_opta_y}
\title{Standardize width (y) location columns from Opta event data to 105m x 68m convention}
\usage{
standardize_opta_y(
  data,
  cols = c("location_y", "PassEndY", "BlockedY", "GoalMouthY"),
  unit = "meters"
)
}
\arguments{
\item{data}{A dataframe of Opta event data}

\item{cols}{Columns to convert. Defaults to all y location columns}

\item{unit}{Convert to meters or yards. Defaults to meters}
}
\description{
Standardize width (y) location columns from Opta event data to 105m x 68m convention
}
\author{
Lars Maurath
}
