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

sb_convert_spadl <- function(match_events, save_dir = ".") {
  home_team <- match_events$team.id[1]

  match_locations <- Rteta::sb_clean_locations(match_events) %>%
    select_at(vars(starts_with("location."))) %>%
    rename_at(vars(starts_with("location.end")), .funs = funs(gsub("location\\.end\\.", "end_", .))) %>%
    rename_at(vars(starts_with("location")), .funs = funs(gsub("location\\.", "start_", .))) %>%
    dplyr::mutate_at(vars(ends_with("x")), .funs = funs(case_when(
      match_events$team.id != home_team ~ 105 - spadl_dict(type = "location_x", provider = "statsbomb", data = .),
      TRUE ~ spadl_dict(type = "location_x", provider = "statsbomb", data = .)
    ))) %>%
  dplyr::mutate_at(vars(ends_with("y")), .funs = funs(case_when(
    match_events$team.id != home_team ~ 68 - spadl_dict(type = "location_y", provider = "statsbomb", data = .),
    TRUE ~ spadl_dict(type = "location_y", provider = "statsbomb", data = .)
  )))
  match_locations$end_x[which(is.na(match_locations$end_x))] <- match_locations$start_x[which(is.na(match_locations$end_x))]
  match_locations$end_y[which(is.na(match_locations$end_y))] <- match_locations$start_y[which(is.na(match_locations$end_y))]
  match_locations$end_x[which(match_events$type.name == "Clearance")] <- match_locations$start_x[which(match_events$type.name == "Clearance") + 1]
  match_locations$end_y[which(match_events$type.name == "Clearance")] <- match_locations$start_y[which(match_events$type.name == "Clearance") + 1]

  #rotate away team locations


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
  actions[actions == "goal keeper"] <- "non_action"


  #get the player minutes per match
  player_mins <- Rteta::sb_getmins_played(match_events) %>%
    dplyr::select(match_id, team_name = team.name, player_id = player.id, player_name = player.name, seconds = state_seconds) %>%
    dplyr::group_by(match_id, team_name, player_id, player_name) %>%
    dplyr::summarise(minutes = sum(seconds) / 60)

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
    cbind(match_locations) %>%
    dplyr::filter(!is.na(player_id) & action != "non_action")

  extra_dribbles <- Rteta::split_dribbles(actions)
  spadl <- rbind(actions, extra_dribbles) %>%
    dplyr::arrange(period_id, time_seconds)

  # if(!dir.exists(save_dir)) dir.create(save_dir)
  # if(!dir.exists(file.path(save_dir, "minutes"))) dir.create(file.path(save_dir, "minutes"))
  # if(!dir.exists(file.path(save_dir, "spadl"))) dir.create(file.path(save_dir, "spadl"))
  #
  # game_id <- unique(match_events$match_id)
  # arrow::write_parquet(spadl, paste0(file.path(save_dir, "minutes", game_id), ".parquet"))
  # arrow::write_parquet(spadl, paste0(file.path(save_dir, "spadl", game_id), ".parquet"))
  #
  # message <- paste("written game", game_id)
  return(spadl)
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
      data[is.na(data)] <- "Foot"

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
  dx <- spadl$end_x - leading_actions$start_x
  dy <- spadl$end_y - leading_actions$start_y

  min_dribble_length = 3.0
  max_dribble_length = 60.0
  max_dribble_duration = 10.0

  dhyp <- sqrt(dx^2 + dy^2)
  notclose_leads <- dhyp >= min_dribble_length
  notfar_leads <- dhyp <= max_dribble_length
  same_phase <- leading_actions$time_seconds - spadl$time_seconds < max_dribble_duration

  dribble_idx <- which(same_team & same_phase & notfar_leads & notclose_leads)

  dribble_actions <- spadl[dribble_idx,]
  dribble_actions$player_id <- spadl$player_id[dribble_idx+1]
  dribble_actions$player_name <- spadl$player_name[dribble_idx+1]
  dribble_actions$start_x <- spadl$start_x[dribble_idx - 1]
  dribble_actions$end_x <- spadl$start_y[dribble_idx - 1]
  dribble_actions$end_x <- spadl$start_x[dribble_idx + 1]
  dribble_actions$end_y <- spadl$start_y[dribble_idx + 1]
  dribble_actions$action <- "dribble"
  dribble_actions$bodypart_name <- "Foot"
  dribble_actions$result_name <- "success"
  dribble_actions$time_seconds <- dribble_actions$time_seconds + 0.1

  return(dribble_actions)
}


