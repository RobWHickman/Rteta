% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standardize_coordinates.R
\name{standardize_statsbomb_z}
\alias{standardize_statsbomb_z}
\title{Standardize height (z) location columns from StatsBomb event data to 105m x 68m convention}
\usage{
standardize_statsbomb_z(data, cols = c("shot.end_location.z"), unit = "meters")
}
\arguments{
\item{data}{A dataframe of cleaned StatsBomb event data}

\item{cols}{Columns to convert. Defaults to all z location columns}

\item{unit}{Convert to meters or yards. Defaults to meters}
}
\description{
Standardize height (z) location columns from StatsBomb event data to 105m x 68m convention
}
\examples{

cl_final <- data.frame(match_id = 22912)
sb_data <- StatsBombR::get.matchFree(cl_final)
sb_data <- allclean(sb_data)
sb_data <- standardize_statsbomb_z(sb_data)

}
\author{
Lars Maurath
}
