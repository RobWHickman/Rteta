#' Clean location columns from import of StatsBomb event data
#' @param match_events A dataframe of imported StatsBomb event data
#' @examples
#'
#' cl_final <- data.frame(match_id = 22912)
#' sb_data <- StatsBombR::get.matchFree(cl_final)
#' sb_data_cleanlocs <- sb_clean_locations(sb_data)
#'
#' @author Robert Hickman
#' @export sb_clean_locations
sb_clean_locations <- function(match_events) {
  #takes start locations of actions and reduces to x and y columns
  start <- match_events$location
  start[sapply(start, is.null)] <- NA
  start_locs <- as.data.frame(t(sapply(start, "[", i = seq_len(max(lengths(start))))))
  if(max(lengths(start)) == 2) {
    names(start_locs) <- c("location.x", "location.y")
  } else {
    names(start_locs) <- c("location.x", "location.y", "location.z")
  }

  # takes end locations for the 3 'progressive' actions and coalesces into 3
  # end location columns (x, y, z)
  ends <- match_events[names(match_events)[grepl("[^goalkeeper].end_location", names(match_events))]]
  ends <- lapply(ends, function(f) {
    f[sapply(f, is.null)] <- NA
    return(f)
  })
  # possibly could be quicker without dependencies
  end_locs <- dplyr::coalesce(!!!ends)
  end_locs <- plyr::ldply(end_locs, rbind)
  names(end_locs) <- c("location.end.x", "location.end.y", "location.end.z")

  # gk end location is same procedure as start location
  gk_end <- match_events$goalkeeper.end_location
  gk_end[sapply(gk_end, is.null)] <- NA
  gk_end_locs <- as.data.frame(do.call(rbind, gk_end))
  names(gk_end_locs) <- c("gk.location.end.x", "gk.location.end.y")

  # remove original columns and replace
  match_events <- match_events[-grep("location", names(match_events))]
  match_events <- cbind(match_events, start_locs, end_locs, gk_end_locs)
}

#' Find minutes played and (guesstimate) position for players from Statsbomb event data
#' @param match_events A dataframe of imported StatsBomb event data
#' @examples
#'
#' cl_final <- data.frame(match_id = 22912)
#' sb_data <- StatsBombR::get.matchFree(cl_final)
#' sb_data_cleanlocs <- sb_getmins_played(sb_data)
#'
#' @author Robert Hickman
#' @export sb_getmins_played
sb_getmins_played = function(match_events) {
  # add in the seconds the event happened at
  match_events$secondselapsed <- as.numeric(lubridate::hms(match_events$timestamp))

  # get the half end times
  halfends <- match_events %>%
    dplyr::filter(type.name == "Half End") %>%
    dplyr::group_by(match_id, period) %>%
    dplyr::summarise(secondselapsed = max(secondselapsed))

  lineups <- match_events %>%
    # filter and select relevant data
    dplyr::filter(type.name %in% c("Starting XI", "Tactical Shift")) %>%
    dplyr::select(match_id, team.name, period, secondselapsed, tactics.lineup) %>%
    # flatten json
    tidyr::unnest_longer("tactics.lineup") %>%
    jsonlite::flatten() %>%
    # clean names
    dplyr::rename_with(., ~gsub("tactics.lineup.", "", .x)) %>%
    dplyr::select(-jersey_number)

  # get subs
  subs <- match_events %>%
    dplyr::filter(type.name == "Substitution") %>%
    dplyr::select(match_id, team.name, period, secondselapsed, player.id, player.name, substitution.replacement.id, substitution.replacement.name, team.name) %>%
    tidyr::pivot_longer(cols = c(player.name, substitution.replacement.name), values_to = "player.name") %>%
    dplyr::mutate(player.id = case_when(
      name == "player.name" ~ player.id,
      name == "substitution.replacement.name" ~ substitution.replacement.id
    )) %>%
    dplyr::mutate(position.name = case_when(
      name == "player.name" ~ "Subbed Off",
      name == "substitution.replacement.name" ~ "Subbed On"
    )) %>%
    # a better way to do this or fill in later?
    dplyr::mutate(position.id = NA) %>%
    dplyr::select(names(lineups))

  # in case not in the dataset
  if(!"bad_behaviour.card.name" %in% names(match_events)) match_events$bad_behaviour.card.name <- NA
  if(!"foul_committed.card.name" %in% names(match_events)) match_events$foul_committed.card.name <- NA
  reds <- match_events %>%
    dplyr::filter(bad_behaviour.card.name %in% c("Red Card", "Second Yellow") | foul_committed.card.name %in% c("Red Card", "Second Yellow")) %>%
    dplyr::mutate(position.id = NA, position.name = "Sent Off") %>%
    dplyr::select(names(lineups))

  player_changes <- bind_rows(lineups, subs, reds) %>%
    dplyr::arrange(period, secondselapsed) %>%
    dplyr::left_join(tidyr::pivot_wider(halfends, id_cols = "match_id", names_from = "period", values_from = "secondselapsed"), by = "match_id") %>%
    dplyr::mutate(secondselapsed = case_when(
      period == 1 ~ secondselapsed,
      period == 2 ~ `1` + secondselapsed
    )) %>%
    dplyr::group_by(match_id, player.id) %>%
    dplyr::mutate(state_seconds = lead(secondselapsed) - secondselapsed) %>%
    dplyr::mutate(state_seconds = case_when(
      is.na(state_seconds) ~ (`1` + `2`) - secondselapsed,
      TRUE ~ state_seconds
    ))
  if(nrow(reds) > 0 ) {
    player_changes <- player_changes %>%
      dplyr::left_join(mutate(reds, red_flag = 1)) %>%
      dplyr::group_by(player.id) %>%
      dplyr::mutate(red_flag = zoo::na.locf(red_flag, na.rm = FALSE)) %>%
      ungroup()
  } else {
    player_changes <- player_changes %>%
      mutate(red_flag = NA)
  }
  player_changes <- player_changes %>%
    dplyr::filter(!position.name %in% c("Subbed Off", "Sent Off") & is.na(red_flag)) %>%
    dplyr::select(-`1`, -`2`, -red_flag) %>%
    dplyr::arrange(team.name, position.id, player.name, period)

  return(player_changes)
}
