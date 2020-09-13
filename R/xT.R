#create_boxes
#
# function to create the boxes to bin xT events
#
#' @param n_wide_boxes An integer for how many boxes across the pitch (wing to wing)
#' @param n_long_boxes An integer for how many boxes along the pitch (goal to goal)
#' @param pitch_w An integer for the width of the pitch (80 for Statsbomb)
#' @param pitch_w An integer for the length of the pitch (120 for Statsbomb)
#
create_boxes <- function(n_wide_boxes, n_long_boxes, pitch_w = 80, pitch_l = 120) {
  box_width <- pitch_w / n_wide_boxes
  box_length <- pitch_l/ n_long_boxes

  boxes <- data.frame(x0 = rep(seq(0, pitch_l-box_length, box_length), each = n_wide_boxes),
                      x1 = rep(seq(box_length, pitch_l, box_length), each = n_wide_boxes),
                      y0 = rep(seq(0, pitch_w-box_width, box_width), n_long_boxes),
                      y1 = rep(seq(box_width, pitch_w, box_width), n_long_boxes)) %>%
    dplyr::mutate(id = 1:n(),
                  center_x = (x0 + x1) / 2,
                  center_y = (y0 + y1) / 2)

  return(boxes)
}

#get_grid_ref
#
# helper func to find the x or y grid ref for the x or y coordinate of an action
#
#' @param coord An integer coordinate of an event
#' @param seq A sequence of the outer limits of boxes for xT in one direction
#
get_grid_ref <- function(coord, seq) {
  #if some error return NA
  #can afford to lose some data that's malspecified
  if(coord > max(seq) | is.na(coord)) {
    return(NA)
  } else {
    #else return the x or y box the event is located in
    #-1 to line up with Karun's method
    return(min(which(seq>=coord))-1)
  }
}

#get_box
#
# function to translate between Karun's script and to my boxes df
#
#' @param x_grid An integer representing the x grid of the event
#' @param y_grid An integer representing the y grid of the event
#' @param box_matrix A matrix of box ids based on grid ref
#
get_box <- function(x_grid, y_grid, box_matrix) {
  return(box_matrix[y_grid + 1, x_grid + 1])
}

#organise_box_movement
#
# create a df of how often each box moves vs. shoots and the xG
#
#' @param boxed_data a data frame of boxed_data to be separated into transition and shooting data
#
organise_box_movement <- function(boxed_data) {
  #separate out shot data and calculate change of scoring in box
  box_shots <- boxed_data %>%
    dplyr::filter(event_type == "shot") %>%
    dplyr::group_by(start_box) %>%
    dplyr::summarise(shots = n(), xG = mean(xg))

  #separate out transition data and calculate change of movement
  box_movements <- boxed_data %>%
    dplyr::filter(event_type %in% c("carry", "pass")) %>%
    dplyr::group_by(start_box) %>%
    dplyr::summarise(movements = n())

  #join the data
  #set NAs to 0
  box_transitions <- dplyr::left_join(box_movements, box_shots)
  box_transitions[is.na(box_transitions)] <- 0

  #transform the percentages
  box_transitions <- box_transitions %>%
    dplyr::mutate(move_perc = movements / (movements + shots),
                  shoot_perc = shots / (movements + shots))

  return(box_transitions)
}

#get_next_box
#
# get the next boss for either successful or unsuccesful passes
#
#' @param box
#' @param success
#' @param boxed_data
#' @param boxes
#
get_next_box <- function(box, success, boxed_data, boxes) {
  #get the transitions that start within this box
  start_box_transitions <- boxed_data %>%
    dplyr::filter(start_box == box) %>%
    dplyr::filter(event_type %in% c("carry", "pass"))

  #if there are passes from this box
  if(nrow(start_box_transitions) != 0) {
    #fr succesful transitions
    if(success == 1) {
      end_boxes <- start_box_transitions %>%
        dplyr::group_by(end_box) %>%
        dplyr::summarise(passes = n(),
                         succesful = sum(outcome)) %>%
        dplyr::mutate(start_box = box,
                      perc = (passes/sum(passes)) * (succesful / passes)) %>%
        dplyr::select(start_box, end_box, perc)
      #and unsuccesful transitions
    } else {
      end_boxes <- start_box_transitions %>%
        dplyr::group_by(end_box) %>%
        dplyr::summarise(passes = n(),
                         succesful = sum(outcome)) %>%
        dplyr::mutate(start_box = box,
                      perc = (passes/sum(passes)) * (1 - (succesful / passes))) %>%
        dplyr::select(start_box, end_box, perc)
    }
    #otherwise make sure there is something to bind
    if(box == 1) {
      missing_boxes <- which(!boxes$id %in% end_boxes$end_box)
      if(length(missing_boxes) > 0) {
        extra_rows <- data.frame(start_box = 1,
                                 end_box = missing_boxes,
                                 perc = 0)

        end_boxes <- dplyr::bind_rows(end_boxes, extra_rows)
      }
    }
    return(end_boxes)
  } else {
    end_boxes <- data.frame(start_box = box,
                            end_box = boxes$id,
                            perc = 0)
  }
}

#convert_matrix
#
# convert the passing network into a matrix
convert_matrix <- function(box, passing_network, boxes) {
  if(!box %in% passing_network$start_box) {
    matrix_row <- matrix(data = rep(NA, max(boxes$id)), nrow = 1)
  } else {
    matrix_row <- filter(passing_network, start_box == box) %>%
      select(-start_box) %>%
      .[order(as.numeric(colnames(.)))] %>%
      as.matrix()
  }
  matrix_row[is.na(matrix_row)] <- 0
  return(matrix_row)
}

#iterate_xt
# function to iterate through the matrices of transition movement and shots to
# calculate the xt of each box
#
#' @param iterations how many times to iterate the xt script. 5 is generall enough
#' @param n_boxes the number of boxes that data should split into
#' @param box_transitions calculated by organise_box_movement. How often players pass or shoot in a given box
#' @param passing_matrix the proportion of transitions from any box to another box
#' @param loss_passing_matrix the proportion of transition from any box to the inverse box when the ball is lost
#' @param type can be "normal" which is equivalent to Karun's initial work, or "plusminus" which is equivelanet to my StatsBomb conference work (Oct 2019)
#' @param boxes a data frame of the boxes
#' @param opposition_xt a data frame of xt results from Karun's initial work to calculate the xt of where the ball is lost
#
iterate_xt <- function(iterations, n_boxes, box_transitions, passing_matrix, loss_passing_matrix, type = "plusminus", boxes, opposition_xt = NA) {
  iteration_data <- data.frame(start_box = NULL,
                               xT = NULL,
                               iteration = NULL)
  ball_location <- diag(1, n_boxes, n_boxes)
  total_ball_loss <- list()

  for(i in 1:iterations) {
    iteration_xt <- ball_location %>%
      apply(., 1, function(x) sum(x * box_transitions$shoot_perc * box_transitions$xG)) %>%
      as.data.frame()
    names(iteration_xt) <- "xT"

    iteration_data <- iteration_xt %>%
      rownames_to_column(var = "start_box") %>%
      dplyr::mutate(iteration = i) %>%
      dplyr::bind_rows(iteration_data, .)

    iteration_ball_loss <- ball_location %*% loss_passing_matrix * box_transitions$move_perc
    total_ball_loss[[i]] <- iteration_ball_loss
    ball_location <- ball_location %*% passing_matrix * box_transitions$move_perc
  }

  xt <- iteration_data %>%
    dplyr::group_by(start_box) %>%
    dplyr::summarise(xT = sum(xT)) %>%
    dplyr::mutate(start_box = as.numeric(start_box)) %>%
    dplyr::arrange(-xT)

  if(type == "plusminus") {
    total_xT <- calc_plusminus(xt, total_ball_loss, boxes, opposition_xt)
    return(total_xT)
  } else {
    return(xt)
  }
}

#calc_plusminus
#
# work out how much risk is presented when players lose the ball to an inverse box for the other team
# basically the thrust of my work for the StatsBomb conference (Oct 2019)
#
#' @param xt
#' @param total_ball_loss
#' @param boxes
#' @param opposition_xt
#
calc_plusminus <- function(xt, total_ball_loss, boxes, opposition_xt) {
  if(is.na(opposition_xt)) {
    xt_vec <- xt %>%
      arrange(-start_box) %>%
      .$xT
  } else {
    xt_vec <- opposition_xt %>%
      arrange(-start_box) %>%
      .$xT
  }
  loss_xts <- total_ball_loss %>%
    lapply(., function(x) {
      xt_loss <- rowSums(x * xt_vec)

      df <- data.frame(start_box = boxes$id,
                       xt_loss = xt_loss)
    }) %>%
    do.call(rbind, .) %>%
    dplyr::group_by(start_box) %>%
    dplyr::summarise(xT_loss = sum(xt_loss))

  total_xt <- left_join(xt, loss_xts) %>%
    dplyr::mutate(total_Xt = xT - xT_loss)
  return(total_xt)
}

get_event_location <- function(event_location) {
  if(all(is.na(event_location))) {
    return(data.frame(x = NA, y = NA))
  } else {
    loc_data <- t(matrix(unlist(event_location)[1:2], nrow = 2))
    loc_df <- as.data.frame(loc_data) %>%
      rename(x = V1, y = V2)
    return(loc_df)
  }
}

