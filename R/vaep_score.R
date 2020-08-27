#' Calculate the VAEP score for all actions in a spadl df
#' @param spadl A df of spadl events
#' @param scores The name of a col of values calculated for the chance of scoring in next n actions
#' @param concedes The name of a col of values calculated for the chance of conceeding in next n actions
#'
#'
#' @author Robert Hickman
#' @export vaep_calculate_score

vaep_calculate_score <- function(spadl, scores, concedes) {
  goal_chance <- spadl[scores]
  concede_chance <- spadl[concedes]

  same_game <- spadl$game_id == lag(spadl$game_id)
  same_period <- spadl$period_id == lag(spadl$period_id)
  same_team <- spadl$team_id == lag(spadl$team_id)


}
