library(data.table)
library(plyr)


list_drivers <- function(data_folder_path) {
  list.dirs(path=data_folder_path, full.names=FALSE, recursive=FALSE)
}


read_driver_trip <- function(data_folder_path, driver, trip) {
  fread(file.path(data_folder_path, driver, paste(trip, '.csv', sep='')))
}


euclidean_norm <- function(x1, y1, x2 = 0, y2 = 0) {
  ((x2 - x1) ^ 2 + (y2 - y1) ^ 2) ^ 0.5
}


scalar_product <- function(x1, y1, x2 = 0, y2 = 0) {
  x1 * x2 + y1 * y2
}


angular_difference <- function(from_angle, to_angle) {
  difference <- to_angle - from_angle
  atan2(sin(difference), cos(difference))
}


circular_arc_length <- function(euclidean_distance_between_ends, angle_between_tangents) {
  r = (euclidean_distance_between_ends / 2) / cos(angle_between_tangents / 2)
  (pi - abs(angle_between_tangents)) * r
}

calc_velocity_vector <- function(trip_data_table) {
  trip_data_table[, `:=`(dx = c(NA, diff(x)),
                         dy = c(NA, diff(y)))]
  trip_data_table$dx[1] = trip_data_table$dx[2]
  trip_data_table$dy[1] = trip_data_table$dy[2]
  trip_data_table
}
  

calc_acceleration_vector <- function(trip_data_table) {
  trip_data_table[, `:=`(ddx = c(NA, diff(dx)),
                         ddy = c(NA, diff(dy)))]
  trip_data_table$ddx[1] = trip_data_table$ddx[2]
  trip_data_table$ddy[1] = trip_data_table$ddy[2]
  trip_data_table
}


calc_velocity <- function(trip_data_table) {
  trip_data_table[, velocity := (dx ^ 2 + dy ^ 2) ^ 0.5]
  trip_data_table
}
 

calc_acceleration <- function(trip_data_table) {
  trip_data_table[, acceleration := scalar_product(ddx, ddy, dx, dy) / euclidean_norm(dx, dy)]
  trip_data_table
}


calc_angle <- function(trip_data_table) {
  trip_data_table[, angle := atan2(dy, dx)]
  trip_data_table
}


calc_angular_velocity <- function(trip_data_table) {
  angular_differences = c(NA, diff(trip_data_table$angle))
  angular_differences[1] = angular_differences[2]
  trip_data_table[, angular_velocity := atan2(sin(angular_differences), cos(angular_differences))]
  trip_data_table
}


calc_angular_acceleration <- function(trip_data_table) {
  trip_data_table[, angular_acceleration := c(NA, diff(angular_velocity))]
  trip_data_table$angular_acceleration[1] = trip_data_table$angular_acceleration[2]
  trip_data_table  
}


calc_velocity_validity <- function(trip_data_table, max_velocity_m_per_s=50) {
  trip_data_table[, velocity_check := velocity < max_velocity_m_per_s]
  trip_data_table
}


calc_angular_velocity_validity <- function(trip_data_table,
                                           max_absolute_angular_velocity = 5 / 6 * pi) {
  trip_data_table[, angular_velocity_check := 
                    abs(angular_velocity) < max_absolute_angular_velocity]
  trip_data_table
}


calc_overall_data_validity <- function(trip_data_table) {
  trip_data_table[, overall_check := velocity_check & angular_velocity_check]
  trip_data_table
}

calc_trip_data <- function(trip_data_table, max_velocity_m_per_s = 50,
                           max_absolute_angular_velocity = 5 / 6 * pi) {
  calc_overall_data_validity(
    calc_angular_velocity_validity(
    calc_velocity_validity(
    calc_angular_acceleration(
    calc_angular_velocity(
    calc_angle(
    calc_acceleration(
    calc_velocity(
    calc_acceleration_vector(
    calc_velocity_vector(trip_data_table))))))),
    max_velocity_m_per_s),
    max_absolute_angular_velocity))
}
  

check_trip_data_quality <- function(trip_data_table) {
  all(trip_data_table$overall_check)
}


bad_trip_data_indices <- function(trip_data_table) {
  which(!trip_data_table$overall_check)
}


clean_velocity_data <- function(trip_data_table, num_rows_before_after = 3) {
  while (!all(trip_data_table$velocity_check)) {
    bad_velocity_data_row_num = which(!trip_data_table$velocity_check)[1]
    before <- trip_data_table[(bad_velocity_data_row_num - num_rows_before_after) : 
                                (bad_velocity_data_row_num - 1)]
    after <- trip_data_table[(bad_velocity_data_row_num + 1): 
                               (bad_velocity_data_row_num + num_rows_before_after)]
    
    d <- before[num_rows_before_after, .(x, y, velocity, angle)]
    x_after <- trip_data_table[bad_velocity_data_row_num, x]
    y_after <- trip_data_table[bad_velocity_data_row_num, y]
    velocity_after <- mean(after$velocity)
    angle_after <- mean(after$angle)
    n = 1
    interpolated_next_xy_velocity_angle <-
      interpolate_next_xy_velocity_angle(d$x, d$y, d$velocity, d$angle,
                                         x_after, y_after, velocity_after, angle_after)
    while (!is.null(interpolated_next_xy_velocity_angle)) {
      d <- rbind(d, interpolated_next_xy_velocity_angle)
      n <- n + 1
      next_d <- d[n]
      interpolated_next_xy_velocity_angle <-
        interpolate_next_xy_velocity_angle(next_d$x, next_d$y, next_d$velocity, next_d$angle,
                                           x_after, y_after, velocity_after, angle_after) 
    }
    
    d <- data.table(rbind.fill(before, d[-1],
                               trip_data_table[bad_velocity_data_row_num], after))
    
    d <- calc_trip_data(d)
    num_rows <- nrow(d)
    d <- d[-c(1 : num_rows_before_after, (num_rows - num_rows_before_after + 1) : num_rows)]
    
    trip_data_table <- rbind(trip_data_table[1 : (bad_velocity_data_row_num - 1)],
                             d, trip_data_table[(bad_velocity_data_row_num + 1) : 
                                                  nrow(trip_data_table)])
  }
  trip_data_table
}





interpolate_next_xy_velocity_angle <- function(x1, y1, v1, a1, x2, y2, v2, a2) {
  angular_diff <- angular_difference(a1, a2)
  straight_line_angle <- atan2(y2 - y1, x2 - x1)
  straight_line_angular_diff <- angular_difference(a1, straight_line_angle)
  if ((straight_line_angular_diff > min(0, angular_diff)) &
        (straight_line_angular_diff < max(0, angular_diff))) {
    arc_length <- circular_arc_length(euclidean_norm(x1, y1, x2, y2), angular_diff)
    mean_velocity <- (v1 + v2) / 2
    estimated_num_seconds <- floor(arc_length / mean_velocity)
    if (estimated_num_seconds > 0) {
      velocity_change_per_second <- (v2 - v1) / estimated_num_seconds
      angular_change_per_second <- angular_diff / estimated_num_seconds
      velocity <- v1 + velocity_change_per_second
      angle <- a1 + angular_change_per_second
      x <- x1 + velocity * cos(angle)
      y <- y1 + velocity * sin(angle)
      list(x=x, y=y, velocity=velocity, angle=angle)
    } else {
      NULL
    }
  } else {
    euclidean_distance <- euclidean_norm(x1, x2, y1, y2)
    mean_velocity <- (v1 + v2) / 2
    estimated_num_seconds <- floor(euclidean_distance / mean_velocity)
    if (estimated_num_seconds > 0) {
      velocity_change_per_second <- (v2 - v1) / estimated_num_seconds
      velocity <- v1 + velocity_change_per_second
      angle <- straight_line_angle - angular_difference(straight_line_angle, a2) / 12
      x <- x1 + velocity * cos(angle)
      y <- y1 + velocity * sin(angle)
      list(x=x, y=y, velocity=velocity, angle=angle)
    } else {
      NULL
    }
  }
}