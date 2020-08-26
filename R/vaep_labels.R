#' Finds and labels goals in the next n actions from a df of spadl events
#' @param spadl A dataframe of event data in spadl format
#' @param n_prev_actions The number of events to look backwards and class as 'leading to a goal'
#'
#'
#' @author Robert Hickman
#' @export vaep_get_labels

vaep_get_labels <- function(spadl, n_prev_actions = 10) {
  team_ids <- unique(spadl$team_id)

  goals <- which(grepl("^shot", spadl$type_name) & spadl$result_name == "success")
  owngoals <- which(spadl$result_name == "owngoal")

  goal_action_ids <- spadl$action_id[c(goals, owngoals)]
  goal_teams <- c(
    spadl$team_id[goals],
    sapply(spadl$team_id[owngoals], function(f) team_ids[!team_ids %in% f])
  )

  goal_actions <- mapply(
    Rteta::find_previous_actions,
    goal_action_ids,
    goal_teams,
    MoreArgs = list(n_prev_actions, spadl)
  )
  goal <- sort(unlist(goal_actions))

  conceeding_actions <- mapply(
    Rteta::find_previous_actions,
    goal_action_ids,
    sapply(goal_teams, function(f) team_ids[!team_ids %in% f]),
    MoreArgs = list(n_prev_actions, spadl)
  )
  concede <- sort(unlist(conceeding_actions))

  label_df <- data.frame(
    scores = seq(nrow(spadl)) %in% goal,
    concedes = seq(nrow(spadl)) %in% concede,
    goal_from_shot = seq(nrow(spadl)) %in% goals
  )
  return(label_df)
}

#' Does the indexing to find the goals/concessions in a df of spadl events
#' @param action_id The action_id of an action
#' @param team The team_id of an action
#' @param n_prev_actions The number of events to look backwards and class as 'leading to a goal'
#' @param spadl A dataframe of event data in spadl format
#'
#'
#' @author Robert Hickman
#' @export find_previous_actions

find_previous_actions <- function(action_id, team, n_prev_actions, spadl) {
  x <- which(spadl$team_id == team & spadl$action_id > action_id - n_prev_actions & spadl$action_id <= action_id)
}
