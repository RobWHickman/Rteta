#' Convert a dataframe of StatsBomb data into the spadl format
#' @param match_events A dataframe of imported StatsBomb event data
#' @examples
#'
#' cl_final <- data.frame(match_id = 22912)
#' sb_data <- StatsBombR::get.matchFree(cl_final)
#' sb_spadl <- sb_convert_spadl(match_events)
#'
#' @author Robert Hickman
#' @export sb_convert_spadl

sb_convert_spadl <- function(match_events) {
  #some useful variables
  home_team <- match_events$team.id[1]

  #init the actions df
  spadl_df <- data.frame(
    game_id = match_events$match_id,
    period_id = match_events$period,
    time_seconds = as.numeric(lubridate::hms(match_events$timestamp)),
    timestamp = match_events$timestamp,
    team_id = match_events$team.id,
    team_name = match_events$team.name,
    player_id = match_events$player.id,
    player_name = match_events$player.name
  )

  #get the locations of events on pitch
  #convert to spadl coordinates (uses length 105 and height 68)
  match_locations <- Rteta::sb_clean_locations(match_events)
  match_locations <- cbind(
    apply(match_locations[grepl("^location.*x$", names(match_locations))], 2, Rteta::spadl_dict, type = "location_x", provider = "statsbomb"),
    apply(match_locations[grepl("^location.*y$", names(match_locations))], 2, Rteta::spadl_dict, type = "location_y", provider = "statsbomb"),
    match_locations[grepl("^location.*z$", names(match_locations))]
  )
  match_locations <- apply(match_locations, 2, round, digits = 2)
  colnames(match_locations) <- gsub("(^location\\.)([a-z]$)", "start_\\2", colnames(match_locations))
  colnames(match_locations) <- gsub("(^location\\.end\\.)([a-z]$)", "end_\\2", colnames(match_locations))

  #get the actions on the pitch
  actions <- match_events$type.name
  actions_extra <- match_events[grepl("\\.type\\.name", names(match_events))]
  actions_extra <- dplyr::coalesce(!!!actions_extra)
  actions <- Rteta::spadl_dict("action", "statsbomb", actions)

  #mapply this later
  #manual munging
  actions[actions == "pass" & actions_extra == "Free Kick" & (match_events$pass.height.name == "High Pass" | match_events$pass.cross)] <- "freekick_crossed"
  actions[actions == "pass" & actions_extra == "Free Kick"] <- "freekick_short"
  actions[actions == "pass" & actions_extra == "Corner" & (match_events$pass.height.name == "High Pass" | match_events$pass.cross)] <- "corner_crossed"
  actions[actions == "pass" & actions_extra == "Corner"] <- "corner_short"
  actions[actions == "pass" & actions_extra == "Goal Kick"] <- "goalkick"
  actions[actions == "pass" & actions_extra == "Throw-in"] <- "throw_in"
  actions[actions == "pass" & match_events$pass.cross] <- "cross"
  actions[actions == "duel" & actions_extra == "Tackle"] <- "tackle"
  actions[actions == "duel"] <- "non_action"
  actions[actions == "shot" & actions_extra == "Free Kick"] <- "shot_freekick"
  actions[actions == "shot" & actions_extra == "Penalty"] <- "shot_penalty"
  actions[actions == "goal keeper" & actions_extra == "Shot Saved"] <- "keeper_save"
  actions[actions == "goal keeper" & actions_extra %in% c("Collected", "Keeper Sweeper")] <- "keeper_claim"
  actions[actions == "goal keeper" & actions_extra == "Punch"] <- "keeper_punch"
  actions[actions == "goal keeper"] <- "non_action"

  #get the bodyparts uses for actions
  bodyparts <- match_events[names(match_events)[grepl("body_part\\.name", names(match_events))]]
  bodyparts <- dplyr::coalesce(!!!bodyparts)
  bodyparts <- Rteta::spadl_dict("body", "statsbomb", bodyparts)

  #get the result of each actions
  results <- match_events[names(match_events)[
    c(
      grep("(?<!substitution)outcome.name", names(match_events), perl = TRUE),
      grep("card.name", names(match_events)),
      grep("clearance.body_part.name", names(match_events))
    )]]
  results <- dplyr::coalesce(!!!results)
  results[which(match_events$type.name == "Own Goal Against")] <- "Own Goal Against"
  results[which(match_events$type.name == "Miscontrol")] <- "Miscontrol"
  results <- Rteta::spadl_dict("results", "statsbomb", results)

  #bind everything together
  spadl_df$action_id <- 1:nrow(spadl_df)
  spadl_df$type_name = actions
  spadl_df$bodypart_name = bodyparts
  spadl_df$result_name = results
  spadl_df <- cbind(spadl_df, match_locations)

  #filter non actions
  spadl_df <- spadl_df[-which(spadl_df$type_name == "non_action"),]
  #add end positions for stationary events
  spadl_df$end_x[is.na(spadl_df$end_x)] <- spadl_df$start_x[is.na(spadl_df$end_x)]
  spadl_df$end_y[is.na(spadl_df$end_y)] <- spadl_df$start_y[is.na(spadl_df$end_y)]
  #fix direction of play
  spadl_df$start_x[spadl_df$team_id != home_team] <- 105 - spadl_df$start_x[spadl_df$team_id != home_team]
  spadl_df$end_x[spadl_df$team_id != home_team] <- 105 - spadl_df$end_x[spadl_df$team_id != home_team]
  spadl_df$start_y[spadl_df$team_id != home_team] <- 68 - spadl_df$start_y[spadl_df$team_id != home_team]
  spadl_df$end_y[spadl_df$team_id != home_team] <- 68 - spadl_df$end_y[spadl_df$team_id != home_team]
  #fill in clearance end locations
  spadl_df$end_x[spadl_df$type_name == "clearance"] <- spadl_df$start_x[which(spadl_df$type_name == "clearance") + 1]
  spadl_df$end_y[spadl_df$type_name == "clearance"] <- spadl_df$start_y[which(spadl_df$type_name == "clearance") + 1]

  #include extra dribbles
  spadl_df <- dplyr::arrange(spadl_df, period_id, time_seconds)
  extra_dribbles <- Rteta::split_dribbles(spadl_df)
  spadl_df <- rbind(spadl_df, extra_dribbles)
  spadl_df <- arrange(spadl_df, period_id, time_seconds)
  spadl_df$action_id <- seq(nrow(spadl_df))
  #add ids
  spadl_df <- dplyr::left_join(spadl_df, Rteta::spadl_type_ids, by = "type_name")
  spadl_df <- dplyr::left_join(spadl_df, Rteta::spadl_result_ids, by = "result_name")
  spadl_df <- dplyr::left_join(spadl_df, Rteta::spadl_bodypart_ids, by = "bodypart_name")


  return(spadl_df)
}

#' Convert data from provider specs into a common spadl format
#' @param type A type of data to be provided
#' @param provider The provider of the event data
#' @param data A vector of event data to be converted
#'
#' @examples
#'
#' @author Robert Hickman
#' @export spadl_dict

#ugly code- rewrite as switch. but works
spadl_dict <- function(type, provider, data) {
  if(provider == "statsbomb") {
    if(type == "action") {
      data <- tolower(data)
      data <- gsub("-", "_", data)
      data[data == "own goal against"] <- "shot"
      data[data == "foul committed"] <- "foul"
      data[data == "dribble"] <- "take_on"
      data[data == "carry"] <- "dribble"
      data[data == "miscontrol"] <- "bad_touch"
      data[!data %in% c("pass", "shot", "take_on", "clearance", "dribble", "duel", "foul", "goal keeper", "interception", "bad_touch")] <- "non_action"

      return(data)
    }
    if(type == "body") {
      data[grepl("Foot", data)] <- "foot"
      data[grepl("Drop Kick", data)] <- "foot"
      data[grepl("Head", data)] <- "head"
      data[!data %in% c("foot", "head") & !is.na(data)] <- "other"
      data[is.na(data)] <- "foot"

      return(data)
    }
    if(type == "location_x") {
      data <- ((data - 1) / 119) * 105

      return(data)
    }
    if(type == "location_y") {
      data <- 68 - ((data - 1) / 79) * 68

      return(data)
    }
    if(type == "results") {
      data[data %in% c("Out", "Incomplete", "Blocked", "Off T", "Post", "Saved", "Wayward", "Incomplete", "Lost In Play", "Lost Out", "In Play Danger", "No Touch", "Miscontrol")] <- "fail"
      data[data %in% c("Own Goal Against")] <- "owngoal"
      data[data %in% c("Second Yellow", "Red Card")] <- "red_card"
      data[data %in% c("Yellow Card")] <- "yellow_card"
      data[data %in% c("Pass Offside")] <- "offside"
      data[!data %in% c("fail", "owngoal", "red_card", "yellow_card", "offside")] <- "success"

      return(data)
    }
    if(!type %in% c("body", "location_x", "location_y", "results")) {
      errorCondition("please specify correct data conversion type")
    }
  }
}

#' Add missing dribbles back into spadl data frame
#' @param spadl A data frame of the prospective spadl format
#'
#' @examples
#'
#' @author Robert Hickman
#' @export split_dribbles

split_dribbles <- function(spadl) {
  leading_actions <- lead(spadl)
  same_team <- spadl$team_id == leading_actions$team_id
  same_period <- spadl$period_id == leading_actions$period_id
  dx <- spadl$end_x - leading_actions$start_x
  dy <- spadl$end_y - leading_actions$start_y

  min_dribble_length = 3.0
  max_dribble_length = 60.0
  #bleh I don't like how this works
  max_dribble_duration = 10

  dhyp <- sqrt(dx^2 + dy^2)
  notclose_leads <- dhyp >= min_dribble_length
  notfar_leads <- dhyp <= max_dribble_length
  same_phase <- floor(leading_actions$time_seconds) - floor(spadl$time_seconds) < max_dribble_duration

  dribble_idx <- which(same_team& same_period & same_phase & notfar_leads & notclose_leads)

  dribble_actions <- spadl[dribble_idx,]
  dribble_actions$player_id <- spadl$player_id[dribble_idx+1]
  dribble_actions$player_name <- spadl$player_name[dribble_idx+1]
  dribble_actions$start_x <- spadl$end_x[dribble_idx]
  dribble_actions$end_x <- spadl$start_x[dribble_idx + 1]
  dribble_actions$start_y <- spadl$end_y[dribble_idx]
  dribble_actions$end_y <- spadl$start_y[dribble_idx + 1]
  dribble_actions$type_name <- "dribble"
  dribble_actions$bodypart_name <- "foot"
  dribble_actions$result_name <- "success"
  dribble_actions$time_seconds <- dribble_actions$time_seconds + 0.1

  return(dribble_actions)
}
