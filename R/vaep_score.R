#' Calculate the VAEP score for all actions in a spadl df
#' @param spadl A df of spadl events
#' @param scores The name of a col of values calculated for the chance of scoring in next n actions
#' @param concedes The name of a col of values calculated for the chance of conceeding in next n actions
#' @param score_type Whether to score for the attacking contribution or defensive contribution
#'
#'
#' @author Robert Hickman
#' @export vaep_score_actions

vaep_score_actions <- function(spadl, scores, concedes, score_type) {
  if(score_type == "attack") {
    pos_contrib <- unlist(spadl[scores])
    neg_contrib <- unlist(spadl[concedes])
    multiplyer <- 1
  } else if(score_type %in% c("defense", "defence")) {
    pos_contrib <- unlist(spadl[concedes])
    neg_contrib <- unlist(spadl[scores])
    multiplyer <- -1
  } else {
    errorCondition("score_type should be either 'attack' or 'defence'")
  }

  same_game <- spadl$game_id == lag(spadl$game_id)
  same_period <- spadl$period_id == lag(spadl$period_id)
  same_team <- spadl$team_id == lag(spadl$team_id)

  prev_val <- lag(pos_contrib, default = 0) * (same_game & same_period & same_team) + lag(neg_contrib, default = 0) * (same_game & same_period & !same_team)

  max_duration = 10
  toolong <- spadl$time_seconds - lag(spadl$time_seconds, default = 0) > max_duration
  prev_val[toolong] <- 0

  prev_goal <- lag(
    (grepl("^shot", spadl$type_name) & spadl$result_name == "success") |
      spadl$result_name == "owngoal",
    default = FALSE
  )
  prev_val[prev_goal] <- 0

  value_add <- multiplyer * (pos_contrib - prev_val)
  return(value_add)
}

#' Calculate the VAEP score for all actions in a spadl df
#' @param spadl A df of spadl events
#' @param scores The name of a col of values calculated for the chance of scoring in next n actions
#' @param concedes The name of a col of values calculated for the chance of conceeding in next n actions
#'
#'
#' @author Robert Hickman
#' @export vaep_get_scores

vaep_get_scores <- function(spadl, scores, concedes) {
  spadl$attack_score <- offensive_value <- vaep_score_actions(spadl, scores, concedes, "attack")
  spadl$defence_score <- defensive_value <- vaep_score_actions(spadl, scores, concedes, "defence")

  spadl$vaep_value <- spadl$attack_score + spadl$defence_score

  return(spadl)
}

