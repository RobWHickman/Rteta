library(dplyr)

#' Standardize length (x) location columns from Opta event data to 105m x 68m convention
#' @param data A dataframe of Opta event data
#' @param cols Columns to convert. Defaults to all x location columns
#' @param unit Convert to meters or yards. Defaults to meters
#'
#'
#' @author Lars Maurath
#' @export standardize_opta_x
standardize_opta_x <- function(data, cols = c("location_x", "PassEndX", "BlockedX"), unit = "meters"){
  # pitch specifications come from https://en.wikipedia.org/wiki/Penalty_area

  if(unit == "meters"){
    data <- data %>%
      mutate_at(cols, ~ case_when(
        abs(. - 50) <= 33 ~ (. - 50) * (105 - 16.5 - 52.5) / (83 - 50) + 52.5, # space between 18 yard boxes
        abs(. - 50) > 33 & abs(. - 50) <= 38.5 ~ (. - 50) * (105 - 10.97 - 52.5) / (88.5 - 50) + 52.5, # spaces between 18 yard box and penalty spot
        abs(. - 50) > 38.5 & abs(. - 50) <= 44.2 ~ (. - 50) * (105 - 5.5 - 52.5) / (94.2 - 50) + 52.5, # spaces between penalty spot and 6 yard box
        abs(. - 50) > 44.2 & abs(. - 50) <= 50 ~ (. - 50) * (105 - 52.5) / (100 - 50) + 52.5 # spaces between 6 yard box and goal line
      ))
  } else if(unit == "yards"){
    data <- data %>%
      mutate_at(cols, ~ case_when(
        abs(. - 50) <= 33 ~ (. - 50) * (114.829 - 18 - 57.4147) / (83 - 50) + 57.4147, # space between 18 yard boxes
        abs(. - 50) > 33 & abs(. - 50) <= 38.5 ~ (. - 50) * (114.829 - 12 - 57.4147) / (88.5 - 50) + 57.4147, # spaces between 18 yard box and penalty spot
        abs(. - 50) > 38.5 & abs(. - 50) <= 44.2 ~ (. - 50) * (114.829 - 6 - 57.4147) / (94.2 - 50) + 57.4147, # spaces between penalty spot and 6 yard box
        abs(. - 50) > 44.2 & abs(. - 50) <= 50 ~ (. - 50) * (114.829 - 57.4147) / (100 - 50) + 57.4147 # spaces between 6 yard box and goal line
      ))
  }
  data
}

#' Standardize width (y) location columns from Opta event data to 105m x 68m convention
#' @param data A dataframe of Opta event data
#' @param cols Columns to convert. Defaults to all y location columns
#' @param unit Convert to meters or yards. Defaults to meters
#'
#'
#' @author Lars Maurath
#' @export standardize_opta_y
standardize_opta_y <- function(data, cols = c("location_y", "PassEndY", "BlockedY", "GoalMouthY"), unit = "meters"){
  # pitch specifications come from https://en.wikipedia.org/wiki/Penalty_area

  if(unit == "meters"){
    data <- data %>%
      mutate_at(cols, ~ case_when(
        abs(. - 50) <= 4.8 ~ (. - 50) * (7.32) / (54.8 - 45.2) + 34, # space between 18 yard boxes
        abs(. - 50) > 4.8 & abs(. - 50) <= 13.2 ~ (. - 50) * (2*5.5 + 7.32) / (63.2 - 36.8) + 34, # spaces between 18 yard box and penalty spot
        abs(. - 50) > 13.2 & abs(. - 50) <= 28.9 ~ (. - 50) * (2*16.5 + 7.32) / (78.9 - 21.1) + 34, # spaces between penalty spot and 6 yard box
        abs(. - 50) > 28.9 & abs(. - 50) <= 50 ~ (. - 50) * (68) / (100) + 34 # spaces between 6 yard box and goal line
      ))
  } else if(unit == "yards"){
    data <- data %>%
      mutate_at(cols, ~ case_when(
        abs(. - 50) <= 4.8 ~ (. - 50) * (8) / (54.8 - 45.2) + 37.1829, # space between 18 yard boxes
        abs(. - 50) > 4.8 & abs(. - 50) <= 13.2 ~ (. - 50) * (2*6 + 8) / (63.2 - 36.8) + 37.1829, # spaces between 18 yard box and penalty spot
        abs(. - 50) > 13.2 & abs(. - 50) <= 28.9 ~ (. - 50) * (2*18 + 8) / (78.9 - 21.1) + 37.1829, # spaces between penalty spot and 6 yard box
        abs(. - 50) > 28.9 & abs(. - 50) <= 50 ~ (. - 50) * (74.3657) / (100) + 37.1829 # spaces between 6 yard box and goal line
      ))
  }
}

#' Standardize height (z) location columns from Opta event data to 105m x 68m convention
#' @param data A dataframe of Opta event data
#' @param cols Columns to convert. Defaults to all z location columns
#' @param unit Convert to meters or yards. Defaults to meters
#'
#'
#' @author Lars Maurath
#' @export standardize_opta_z
standardize_opta_z <- function(data, cols = c("GoalMouthZ"), unit = "meters"){
  # pitch specifications come from https://en.wikipedia.org/wiki/Penalty_area

  if(unit == "meters"){
    data <- data %>%
      # we assume linear stretching for the z coordinate. Note that this transformation implied a post width of around 26cm which is more
      # than double the norm. This is ignored for now
      mutate_at(cols, ~ . / 38 * 2.44)
  } else if(unit == "yards"){
    data <- data %>%
      mutate_at(cols, ~ . / 38 * 2.67)
  }
}

#' Standardize length (x) location columns from StatsBomb event data to 105m x 68m convention
#' @param data A dataframe of cleaned StatsBomb event data
#' @param cols Columns to convert. Defaults to all x location columns
#' @param unit Convert to meters or yards. Defaults to meters
#' @examples
#'
#' cl_final <- data.frame(match_id = 22912)
#' sb_data <- StatsBombR::get.matchFree(cl_final)
#' sb_data <- allclean(sb_data)
#' sb_data <- standardize_statsbomb_x(sb_data)
#'
#' @author Lars Maurath
#' @export standardize_statsbomb_x
standardize_statsbomb_x <- function(data, cols = c("location.x", "carry.end_location.x", "pass.end_location.x", "shot.end_location.x", "location.x.GK"), unit = "meters"){
  # pitch specifications come from https://en.wikipedia.org/wiki/Penalty_area
  # and https://github.com/statsbomb/open-data/blob/master/doc/StatsBomb%20Open%20Data%20Specification%20v1.1.pdf

  data <- data %>%
    mutate_at(cols, ~ case_when(
      . >= 102 ~ . - (120 - 114.829), # offensive 18 yard box
      . > 18 & . < 102 ~ (. - 18) * (114.829 - 36) / (102 - 18) + 18, # space between 18 yard boxes
      . <= 18 ~ . # defensive 18 yard box
    ))

  if(unit == "yards"){
    data <- data
  } else if(unit == "meters"){
    data <- data %>%
      mutate_at(cols, ~ . / 1.09361)
  } else{
    stop("Unknown unit")
  }

  data
}

#' Standardize width (y) location columns from StatsBomb event data to 105m x 68m convention
#' @param data A dataframe of cleaned StatsBomb event data
#' @param cols Columns to convert. Defaults to all y location columns
#' @param unit Convert to meters or yards. Defaults to meters
#' @examples
#'
#' cl_final <- data.frame(match_id = 22912)
#' sb_data <- StatsBombR::get.matchFree(cl_final)
#' sb_data <- allclean(sb_data)
#' sb_data <- standardize_statsbomb_y(sb_data)
#'
#' @author Lars Maurath
#' @export standardize_statsbomb_y
standardize_statsbomb_y <- function(data, cols = c("location.y", "carry.end_location.y", "pass.end_location.y", "shot.end_location.y", "location.y.GK"), unit = "meters"){
  # pitch specifications come from https://en.wikipedia.org/wiki/Penalty_area
  # and https://github.com/statsbomb/open-data/blob/master/doc/StatsBomb%20Open%20Data%20Specification%20v1.1.pdf

  data <- data %>%
    mutate_at(cols, ~ abs(. - 80)) %>% # flip y-coordinates
    mutate_at(cols, ~ case_when(
      . < 18 ~ .,
      . >= 18 & . <= 62 ~ (. - 40) + 74.3657/2, # space within 18 yard boxes
      . < 18 ~ . * (74.3657 - 44) / 2 / 18,
      . > 62 ~ (. - 80) * (74.3657 - 44) / 2 / 18 + 74.3657
    ))

  if(unit == "yards"){
    data <- data
  } else if(unit == "meters"){
    data <- data %>%
      mutate_at(cols, ~ . / 1.09361)
  } else{
    stop("Unknown unit")
  }

  data
}

#' Standardize height (z) location columns from StatsBomb event data to 105m x 68m convention
#' @param data A dataframe of cleaned StatsBomb event data
#' @param cols Columns to convert. Defaults to all z location columns
#' @param unit Convert to meters or yards. Defaults to meters
#' @examples
#'
#' cl_final <- data.frame(match_id = 22912)
#' sb_data <- StatsBombR::get.matchFree(cl_final)
#' sb_data <- allclean(sb_data)
#' sb_data <- standardize_statsbomb_z(sb_data)
#'
#' @author Lars Maurath
#' @export standardize_statsbomb_z
standardize_statsbomb_z <- function(data, cols = c("shot.end_location.z"), unit = "meters"){
  # pitch specifications come from https://en.wikipedia.org/wiki/Penalty_area
  # and https://github.com/statsbomb/open-data/blob/master/doc/StatsBomb%20Open%20Data%20Specification%20v1.1.pdf

  if(unit == "yards"){
    data <- data
  } else if(unit == "meters"){
    data <- data %>%
      mutate_at(cols, ~ . / 1.09361)
  } else{
    stop("Unknown unit")
  }

  data
}

#' Standardize all location columns for a given event data set to 105m x 68m convention
#' @param data A dataframe of event data (needs to be cleaned for StatsBomb data)
#' @param provider Data provider for the data set. 'opta' and 'statsbomb' currently supported
#' @param unit Convert to meters or yards. Defaults to meters
#' @examples
#'
#' cl_final <- data.frame(match_id = 22912)
#' sb_data <- StatsBombR::get.matchFree(cl_final)
#' sb_data <- allclean(sb_data)
#' sb_data <- standardize_coordinates(sb_data, provider = "statsbomb")
#'
#' @author Lars Maurath
#' @export standardize_statsbomb_z
standardize_coordinates <- function(data, provider, unit = "meters"){

  if(provider == "opta"){
    data <- data %>%
      standardize_opta_x(unit = unit) %>%
      standardize_opta_y(unit = unit) %>%
      standardize_opta_z(unit = unit)
  } else if (provider == "statsbomb"){
    data <- data %>%
      standardize_statsbomb_x(unit = unit) %>%
      standardize_statsbomb_y(unit = unit) %>%
      standardize_statsbomb_z(unit = unit)
  } else{
    stop("Unknown provider. Please choose 'opta' or 'statsbomb'.")
  }

  data
}
