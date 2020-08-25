#' Get a dataframe of n lagged columns of a feature (to be deprecated)
#' @param vec A vector of data
#' @param lags An integer >= 0
#' @param name A character string
#'
#' @examples
#' feature <- runif(10)
#' lagged_cols <- vaep_features_lag(feature, 3, "lagged_feature_")
#' head(lagged_cols)
#'
#' @author Robert Hickman
#' @export vaep_features_lag

vaep_features_lag <- function(vec, lags, name) {
  x <- lapply(0:lags, lag, x = vec) %>% do.call(cbind, .) %>% as.data.frame()
  #replace names
  names(x) <- paste0(name, 0:lags)
  return(x)
}

#' Get the features of each match event (and preceeding events) from a spadl format
#' @param spadl A dataframe of event data in spadl format
#'
#'
#' @author Robert Hickman
#' @export vaep_get_features

vaep_get_features <- function(spadl) {
  type_ids <- Rteta::vaep_features_lag(spadl$type_id, 2, "type_id_a")

  types_onehot <- do.call(
    cbind,
    lapply(Rteta::spadl_type_ids$type_id, function(i, cols) {
      df <- as.data.frame(cols == i)
      names(df) <- gsub("_id_", paste0("_", i, "_"), names(df))
      return(df)
    }, cols = type_ids)
  )

  bodypart_ids <- Rteta::vaep_features_lag(spadl$bodypart_id, 2, "bodypart_id_a")

  bodypart_onehot <- do.call(
    cbind,
    lapply(Rteta::spadl_bodypart_ids$bodypart_id, function(i, cols) {
      df <- as.data.frame(cols == i)
      names(df) <- gsub("_id_", paste0("_", i, "_"), names(df))
      return(df)
    }, cols = type_ids)
  )

  result_ids <- Rteta::vaep_features_lag(spadl$result_id, 2, "result_id_a")

  result_onehot <- do.call(
    cbind,
    lapply(Rteta::spadl_result_ids$result_id, function(i, cols) {
      df <- as.data.frame(cols == i)
      names(df) <- gsub("_id_", paste0("_", i, "_"), names(df))
      return(df)
    }, cols = type_ids)
  )

  period_ids <- Rteta::vaep_features_lag(spadl$period_id, 2, "period_id_a")

  halves <- split(spadl,spadl$period_id)
  period_time_seconds <- lapply(halves, function(h) vaep_features_lag(h$time_seconds, 2, "time_seconds_a"))
  period_time_seconds <- do.call(rbind, period_time_seconds)

  half_ends <- lapply(halves, function(h) h[which.max(h$time_seconds),])
  half_ends <- do.call(rbind, half_ends)[c("time_seconds", "period_id")]
  half_ends$period_id <- half_ends$period_id + 1
  half_ends <- rbind(data.frame(time_seconds = 0, period_id = 1), half_ends)

  total_time_seconds <- period_time_seconds
  total_time_seconds$period_id <- spadl$period_id
  total_time_seconds <- merge(total_time_seconds, half_ends, by = "period_id")

  total_time_seconds$overall_time_secondsa0 = total_time_seconds$time_seconds_a0 + total_time_seconds$time_seconds
  total_time_seconds$overall_time_secondsa1 = total_time_seconds$time_seconds_a1 + total_time_seconds$time_seconds
  total_time_seconds$overall_time_secondsa2 = total_time_seconds$time_seconds_a2 + total_time_seconds$time_seconds

  total_time_seconds <- total_time_seconds[grep("a[0-9]$", names(total_time_seconds))]

  start_xs <- Rteta::vaep_features_lag(spadl$start_x, 2, "start_x_a")
  start_ys <- Rteta::vaep_features_lag(spadl$start_y, 2, "start_y_a")
  end_xs <- Rteta::vaep_features_lag(spadl$end_x, 2, "end_x_a")
  end_ys <- Rteta::vaep_features_lag(spadl$end_y, 2, "end_y_a")

  delta_x <- end_xs - start_xs
  names(delta_x) <- gsub("^end_", "delta_", names(delta_x))
  delta_y <- end_ys - start_ys
  names(delta_y) <- gsub("^end_", "delta_", names(delta_y))

  movement <- sqrt(delta_x^2 + delta_y ^ 2)
  names(movement) <- gsub("^delta_x_", "movement_", names(movement))

  goal_context <- Rteta::get_context_goals(spadl)
}

#' Get the goal context of a spadl data frame
#' @param spadl A dataframe of event data in spadl format
#'
#'
#' @author Robert Hickman
#' @export get_context_goals

get_context_goals <- function(spadl) {
  teama <- spadl$team_id == unique(spadl$team_id)[1]
  teama_goals <- (teama & grepl("^shot", spadl$type_name) & spadl$result_name == "success") | (teamb & grepl("^shot", spadl$type_name) & spadl$result_name == "owngoal")
  teamb <- spadl$team_id == unique(spadl$team_id)[2]
  teamb_goals <- (teamb & grepl("^shot", spadl$type_name) & spadl$result_name == "success") | (teama & grepl("^shot", spadl$type_name) & spadl$result_name == "owngoal")

  goalsfor <- lag(cumsum(teama_goals) * teama + cumsum(teamb_goals) * teamb, default = 0)
  goalsagainst <- lag(cumsum(teama_goals) * teamb + cumsum(teamb_goals) * teama, default = 0)

  df <- data.frame(goalsfor, goalsagainst, goalsdiff = goalsfor - goalsagainst)

  return(df)
}

