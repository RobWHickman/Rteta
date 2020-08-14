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
  match_locations <- Rteta::sb_clean_locations(match_events) %>%
    select_at(vars(starts_with("location."))) %>%
    rename_at(vars(starts_with("location.end")), .funs = funs(gsub("location\\.end\\.", "end_", .))) %>%
    rename_at(vars(starts_with("location")), .funs = funs(gsub("location\\.", "start_", .)))
  match_locations <- cbind(
    match_locations[grep("z$", names(match_locations))],
    apply(match_locations[grep("x$", names(match_locations))], 2, spadl_dict, type = "location_x", provider = "statsbomb"),
    apply(match_locations[grep("y$", names(match_locations))], 2, spadl_dict, type = "location_y", provider = "statsbomb")
  )
  match_locations$end_location_x[which(match_events$type.name == "Clearance")] <- match_locations$start_location_x[which(match_events$type.name == "Clearance") + 1]
  match_locations$end_location_y[which(match_events$type.name == "Clearance")] <- match_locations$start_location_y[which(match_events$type.name == "Clearance") + 1]

  actions <- match_events$type.name
  actions_extra <- match_events %>%
    dplyr::select(grep("\\.type\\.name", names(match_events)))
  actions_extra <- dplyr::coalesce(!!!actions_extra)

  actions <- Rteta::spadl_dict("action", "statsbomb", actions)

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
  actions[actions == "goal keeper"] <- "non_actions"


  #get the player minutes per match
  player_mins <- Rteta::sb_getmins_played(match_events) %>%
    select(match_id, team_name = team.name, player_id = player.id, player_name = player.name, seconds = state_seconds) %>%
    group_by(match_id, team_name, player_id, player_name) %>%
    summarise(minutes = sum(seconds) / 60)

  bodyparts <- match_events[names(match_events)[grepl("body_part\\.name", names(match_events))]]
  bodyparts <- dplyr::coalesce(!!!bodyparts)

  results <- match_events %>%
    select(names(match_events)[
      c(
        grep("(?<!substitution)outcome.name", names(match_events), perl = TRUE),
        grep("card.name", names(match_events)),
        grep("clearance.body_part.name", names(match_events))
      )])
  results <- dplyr::coalesce(!!!results)
  results[which(match_events$type.name == "Own Goal Against")] <- "Own Goal Against"
  results[which(match_events$type.name == "Miscontrol")] <- "Miscontrol"

  actions <- match_events %>%
    mutate(seconds = as.numeric(lubridate::hms(match_events$timestamp))) %>%
    select(game_id = match_id, period_id = period, time_seconds = seconds, timestamp,
           team_id = team.id, team_name = team.name, player_id = player.id, player_name = player.name) %>%
    mutate(action = actions,
           bodypart_name = spadl_dict("body", "statsbomb", bodyparts),
           result_name = spadl_dict("results", "statsbomb", results)) %>%
    cbind(match_locations)
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
      data[grepl("Foot", data)] <- "Foot"
      data[grepl("Drop Kick", data)] <- "Foot"
      data[grepl("Head", data)] <- "Head"
      data[!data %in% c("Foot", "Head") & !is.na(data)] <- "Other"

      return(data)
    }
    if(type == "location_x") {
      data <- (data / 120) * 105

      return(data)
    }
    if(type == "location_y") {
      data <- (data / 80) * 68

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
