% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standardize_coordinates.R
\name{standardize_coordinates}
\alias{standardize_coordinates}
\title{Standardize all location columns for a given event data set to 105m x 68m convention}
\usage{
standardize_coordinates(data, provider, unit = "meters")
}
\arguments{
\item{data}{A dataframe of event data (needs to be cleaned for StatsBomb data)}

\item{provider}{Data provider for the data set. 'opta' and 'statsbomb' currently supported}

\item{unit}{Convert to meters or yards. Defaults to meters}
}
\description{
Standardize all location columns for a given event data set to 105m x 68m convention
}
\examples{

cl_final <- data.frame(match_id = 22912)
sb_data <- StatsBombR::get.matchFree(cl_final)
sb_data <- allclean(sb_data)
sb_data <- standardize_coordinates(sb_data, provider = "statsbomb")

}
\author{
Lars Maurath
}
