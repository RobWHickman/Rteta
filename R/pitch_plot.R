library(ggplot2)

#' Plot standard pitch with dimension of 105m x 68m (or yard equivalent). 
#' Event or tracking data the has been standardized to these dimensions
#' can be readily plotted on top. 
#' @param unit Dimensions in meters or yards. Defaults to meters
#' @param theme Choose between light and dark theme. Defaults to light
#' @param type Choose between full_pitch, offensive_half or defensive_half. Defaults to full_pitch
#' @param orientation Choose between horizontal or vertical orientation. Defaults to horizontal 
#' @param direction_arrow Optional indicator for direction of play. Defaults to false
#'
#'
#' @author Lars Maurath
#' @export plot_pitch
plot_pitch <- function(unit = "meters", theme = "light", type = "full_pitch", orientation = "horizontal", direction_arrow = FALSE){
  
  if(theme == "light"){
    pitch_color <- "white"
    line_color <- "grey10"
  } else if(theme == "dark"){
    pitch_color <- "grey20"
    line_color <- "grey95"
  }
  
  length <- 105
  width <- 68
  
  mid_point_x = length / 2
  mid_point_y = width / 2
  
  goal_width <- 7.32
  
  center_circle_radius <- 9.15
  penalty_distance <- 10.97
  penalty_box_distance <- 16.5
  six_yard_box_distance <- 5.5
  
  yards_per_meter <- 1.09361
  
  if(unit == "yards"){
    length <- length / yards_per_meter
    width <- width / yards_per_meter
    
    mid_point_x = length / 2
    mid_point_y = width / 2
    
    goal_width <- goal_width / yards_per_meter
    
    center_circle_radius <- center_circle_radius / yards_per_meter
    penalty_distance <- penalty_distance / yards_per_meter
    penalty_box_distance <- penalty_box_distance / yards_per_meter
    six_yard_box_distance <- six_yard_box_distance / yards_per_meter
  }
  
  circle_function <- function(center, radius, npoints = 100){
    tt <- seq(0, 2*pi, length.out = npoints)
    xx <- center[1] + radius * cos(tt)
    yy <- center[2] + radius * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  center_circle_data <- circle_function(center = c(mid_point_x, mid_point_y), radius = center_circle_radius, npoints = 500)
  
  penalty_arc_left_data <- circle_function(center = c(penalty_distance, mid_point_y), radius = center_circle_radius, npoints = 500) %>%
    filter(x >= penalty_box_distance)
  
  penalty_arc_right_data <- circle_function(center = c(length - penalty_distance, mid_point_y), radius = center_circle_radius, npoints = 500) %>%
    filter(x <= length - penalty_box_distance)
  
  if(type == "full_pitch"){
    
    aspect_ratio <- width/length
    
    p <- ggplot() +
      geom_rect(aes(xmin = 0, xmax = length, ymin = 0, ymax = width), size = 0.5, colour = line_color, fill = pitch_color) + # outside border
      geom_rect(aes(xmin = 0, 
                    xmax = penalty_box_distance, 
                    ymin = mid_point_y - goal_width/2 - penalty_box_distance, 
                    ymax = mid_point_y + goal_width/2 + penalty_box_distance), 
                size = 0.5, 
                alpha = 0.0, 
                colour = line_color) + # left penalty box
      geom_rect(aes(xmin = length - penalty_box_distance, 
                    xmax = length, 
                    ymin = mid_point_y - goal_width/2 - penalty_box_distance, 
                    ymax = mid_point_y + goal_width/2 + penalty_box_distance), 
                size = 0.5, 
                alpha = 0.0, 
                colour = line_color) + # right penalty box
      geom_rect(aes(xmin = 0, 
                    xmax = six_yard_box_distance, 
                    ymin = mid_point_y - goal_width/2 - six_yard_box_distance, 
                    ymax = mid_point_y + goal_width/2 + six_yard_box_distance), 
                size = 0.5, 
                alpha = 0.0, 
                colour = line_color) + # left 6 yard box
      geom_rect(aes(xmin = length - six_yard_box_distance, 
                    xmax = length, 
                    ymin = mid_point_y - goal_width/2 - six_yard_box_distance, 
                    ymax = mid_point_y + goal_width/2 + six_yard_box_distance), 
                size = 0.5, 
                alpha = 0.0, 
                colour = line_color) + # right 6 yard  box
      geom_segment(aes(x = mid_point_x, xend = mid_point_x, y = 0, yend = width), size = 0.5, colour = line_color) + # center pitch
      geom_segment(aes(x = length, xend = length, y = mid_point_y - goal_width/2, yend = mid_point_y + goal_width/2), size = 2, colour = line_color) + # goal line
      geom_segment(aes(x = 0, xend = 0, y = mid_point_y - goal_width/2, yend = mid_point_y + goal_width/2), size = 2, colour = line_color) + # goal line
      geom_point(aes(x = mid_point_x, y = mid_point_y), size = 0.5, colour = line_color) + # center spot
      geom_point(aes(x = penalty_distance, y = mid_point_y), size = 0.5, colour = line_color) + # left penalty spot
      geom_point(aes(x = length - penalty_distance, y = mid_point_y), size = 0.5, colour = line_color) + # right penalty spot
      geom_path(data = center_circle_data, aes(x,y), size = 0.5, colour = line_color) +
      geom_path(data = penalty_arc_left_data, aes(x,y), size = 0.5, colour = line_color) +
      geom_path(data = penalty_arc_right_data, aes(x,y), size = 0.5, colour = line_color) +
      theme_void() +
      theme(plot.background = element_rect(fill = pitch_color),
            aspect.ratio = width/length)
    
  } else if(type == "offensive_half"){
    
    center_circle_data <- center_circle_data %>%
      filter(x >= length / 2)
    
    aspect_ratio <- width/(length/2)
    
    p <- ggplot() +
      geom_rect(aes(xmin = length / 2, xmax = length, ymin = 0, ymax = width), size = 0.5, colour = line_color, fill = pitch_color) + # outside border
      geom_rect(aes(xmin = length - penalty_box_distance, 
                    xmax = length, 
                    ymin = mid_point_y - goal_width/2 - penalty_box_distance, 
                    ymax = mid_point_y + goal_width/2 + penalty_box_distance), 
                size = 0.5, 
                alpha = 0.0, 
                colour = line_color) + # offensive penalty box
      geom_rect(aes(xmin = length - six_yard_box_distance, 
                    xmax = length, 
                    ymin = mid_point_y - goal_width/2 - six_yard_box_distance, 
                    ymax = mid_point_y + goal_width/2 + six_yard_box_distance), 
                size = 0.5, 
                alpha = 0.0, 
                colour = line_color) + # offensive 6 yard  box
      geom_segment(aes(x = length, xend = length, y = mid_point_y - goal_width/2, yend = mid_point_y + goal_width/2), size = 2, colour = line_color) + # goal line
      geom_point(aes(x = length - penalty_distance, y = mid_point_y), size = 0.5, colour = line_color) + # offensive penalty spot
      geom_path(data = center_circle_data, aes(x,y), size = 0.5, colour = line_color) +
      geom_path(data = penalty_arc_right_data, aes(x,y), size = 0.5, colour = line_color) +
      theme_void() +
      theme(plot.background = element_rect(fill = pitch_color),
            aspect.ratio = aspect_ratio)
  } else if(type == "defensive_half"){
    
    center_circle_data <- center_circle_data %>%
      filter(x <= length / 2)
    
    aspect_ratio <- width/(length/2)
    
    p <- ggplot() +
      geom_rect(aes(xmin = 0, xmax = length / 2, ymin = 0, ymax = width), size = 0.5, colour = line_color, fill = pitch_color) + # outside border
      geom_rect(aes(xmin = 0, 
                    xmax = penalty_box_distance, 
                    ymin = mid_point_y - goal_width/2 - penalty_box_distance, 
                    ymax = mid_point_y + goal_width/2 + penalty_box_distance), 
                size = 0.5, 
                alpha = 0.0, 
                colour = line_color) + # defensive penalty box
      geom_rect(aes(xmin = 0, 
                    xmax = six_yard_box_distance, 
                    ymin = mid_point_y - goal_width/2 - six_yard_box_distance, 
                    ymax = mid_point_y + goal_width/2 + six_yard_box_distance), 
                size = 0.5, 
                alpha = 0.0, 
                colour = line_color) + # defensive 6 yard box
      geom_segment(aes(x = 0, xend = 0, y = mid_point_y - goal_width/2, yend = mid_point_y + goal_width/2), size = 2, colour = line_color) + # goal line
      geom_point(aes(x = penalty_distance, y = mid_point_y), size = 0.5, colour = line_color) + # defensive penalty spot
      geom_path(data = center_circle_data, aes(x,y), size = 0.5, colour = line_color) +
      geom_path(data = penalty_arc_left_data, aes(x,y), size = 0.5, colour = line_color) +
      theme_void() +
      theme(plot.background = element_rect(fill = pitch_color),
            aspect.ratio = aspect_ratio)
  } 
  
  if(orientation == "vertical"){
    
    p <- p +
      scale_y_reverse() +
      theme(aspect.ratio = 1/aspect_ratio) +
      coord_flip()
  }
  
  if(direction_arrow){
    p <- p +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
      geom_segment(aes(x = 40, xend = 65, y = -5, yend = -5), size = 0.5, colour = line_color, arrow = arrow(length = unit(0.03, "npc")))
  }  
  
  p
}
