#' Clean location columns from import of StatsBomb event data
#' @param df A dataframe of imported StatsBomb event data
#' @examples
#'
#' cl_final <- data.frame(match_id = 22912)
#' sb_data <- StatsBombR::get.matchFree(cl_final)
#' sb_data_cleanlocs <- sb_clean_locations(sb_data)
#'
#' @author Robert Hickman
#' @export
#' @importFrom dplyr coalesce
#' @importFrom plyr ldply

sb_clean_locations <- function(df) {
  #takes start locations of actions and reduces to x and y columns
  start <- df$location
  start[sapply(start, is.null)] <- NA
  start_locs <- as.data.frame(do.call(rbind, start))
  names(start_locs) <- c("location.x", "location.y")

  # takes end locations for the 3 'progressive' actions and coalesces into 3
  # end location columns (x, y, z)
  ends <- df[names(df)[grepl("[^goalkeeper].end_location", names(df))]]
  ends <- lapply(ends, function(f) {
    f[sapply(f, is.null)] <- NA
    return(f)
  })
  # possibly could be quicker without dependencies
  end_locs <- dplyr::coalesce(!!!ends)
  end_locs <- plyr::ldply(end_locs, rbind)
  names(end_locs) <- c("location.end.x", "location.end.y", "location.end.z")

  # gk end location is same procedure as start location
  gk_end <- df$goalkeeper.end_location
  gk_end[sapply(gk_end, is.null)] <- NA
  gk_end_locs <- as.data.frame(do.call(rbind, gk_end))
  names(gk_end_locs) <- c("gk.location.end.x", "gk.location.end.y")

  # remove original columns and replace
  df <- df[-grep("location", names(df))]
  df <- cbind(df, start_locs, end_locs, gk_end_locs)
}
