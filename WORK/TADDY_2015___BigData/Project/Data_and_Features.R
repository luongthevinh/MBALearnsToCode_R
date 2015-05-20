list_drivers <- function(data_folder_path)
{
  list.dirs(path=data_folder_path, full.names=FALSE, recursive=FALSE)
}


read_driver_trip <- function(data_folder_path, driver, trip)
{
  library(data.table)
  fread(file.path(data_folder_path, driver, paste(trip, '.csv', sep='')))
}


calc_velocity_vector <- function(trip_data_table)
{
  trip_data_table[, `:=`(dx = c(NA, diff(x)),
                         dy = c(NA, diff(y)))]
  trip_data_table$dx[1] = trip_data_table$dx[2]
  trip_data_table$dy[1] = trip_data_table$dy[2]
  trip_data_table
}
  

calc_acceleration_vector <- function(trip_data_table)
{
  trip_data_table[, `:=`(ddx = c(NA, diff(dx)),
                         ddy = c(NA, diff(dy)))]
  trip_data_table$ddx[1] = trip_data_table$ddx[2]
  trip_data_table$ddy[1] = trip_data_table$ddy[2]
  trip_data_table
}


calc_velocity <- function(trip_data_table)
{
  trip_data_table[, velocity := (dx ^ 2 + dy ^ 2) ^ 0.5]
  trip_data_table
}
 

calc_acceleration <- function(trip_data_table)
{
  trip_data_table[, acceleration := (ddx ^ 2 + ddy ^ 2) ^ 0.5]
  trip_data_table
}


calc_angle <- function(trip_data_table)
{
  trip_data_table[, angle := atan2(dy, dx)]
  trip_data_table
}


calc_angular_velocity <- function(trip_data_table)
{
  angular_differences = c(NA, diff(trip_data_table$angle))
  angular_differences[1] = angular_differences[2]
  trip_data_table[, angular_velocity := atan2(sin(angular_differences), cos(angular_differences))]
  trip_data_table
}


calc_angular_acceleration <- function(trip_data_table)
{
  trip_data_table[, angular_acceleration := c(NA, diff(angular_velocity))]
  trip_data_table$angular_acceleration[1] = trip_data_table$angular_acceleration[2]
  trip_data_table  
}


calc_velocity_validity <- function(trip_data_table, max_velocity_m_per_s=50)
{
  trip_data_table[, velocity_check := velocity < max_velocity_m_per_s]
  trip_data_table
}


calc_angular_velocity_validity <- function(trip_data_table,
                                           max_absolute_angular_velocity = 5 / 6 * pi)
{
  trip_data_table[, angular_velocity_check := abs(angular_velocity) < max_absolute_angular_velocity]
  trip_data_table
}


calc_overall_data_validity <- function(trip_data_table)
{
  trip_data_table[, overall_check := velocity_check & angular_velocity_check]
  trip_data_table
}

calc_trip_data <- function(trip_data_table, max_velocity_m_per_s = 50,
                           max_absolute_angular_velocity = 5 / 6 * pi)
{
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
  

check_trip_data_quality <- function(trip_data_table)
{
  all(trip_data_table$overall_check)
}


bad_trip_data_indices <- function(trip_data_table)
{
  which(!trip_data_table$overall_check)
}


clean_velocity_data <- function(trip_data_table, bad_velocity_data_row_num,
                                num_rows_before_after = 3)
{
  library(data.table)
  library(plyr)
  
  before <- trip_data_table[(bad_velocity_data_row_num - num_rows_before_after) : 
                              (bad_velocity_data_row_num - 1)]
  after <- trip_data_table[(bad_velocity_data_row_num + 1): 
                             (bad_velocity_data_row_num + num_rows_before_after)]
  
  xy_before <- unlist(before[num_rows_before_after, .(x, y)])
  xy_after <- unlist(after[1, .(x, y)])
  euclidean_distance <- sum((xy_before - xy_after) ^ 2) ^ 0.5
  
  angle_before <- mean(before$angle)
  angle_after <- mean(after$angle)
  angular_difference <- angular_differences(angle_after, angle_before)
  
  arc_length <- circular_arc_length(euclidean_distance, angular_difference)
  
  velocity_before <- mean(before$velocity)
  velocity_after <- mean(after$velocity)
  mean_velocity <- (velocity_before + velocity_after) / 2
  
  estimated_num_seconds <- round(arc_length / mean_velocity)
  
  velocity_change_per_second <- (velocity_after - velocity_before) / estimated_num_seconds
  angular_change_per_second <- angular_difference / estimated_num_seconds
  
  estimated_num_seconds_to_derive <- estimated_num_seconds - 1
  derived_x <- rep(NA, estimated_num_seconds_to_derive)
  derived_y <- rep(NA, estimated_num_seconds_to_derive)
  derived_velocities <- rep(NA, estimated_num_seconds_to_derive)
  derived_angles <- rep(NA, estimated_num_seconds_to_derive)
  derived_x[1] <- xy_before[1]
  derived_y[1] <- xy_before[2]
  derived_velocities[1] <- velocity_before
  derived_angles[1] <- angle_before  
  for (t in 2 : estimated_num_seconds_to_derive)
  {
    derived_velocities[t] <- derived_velocities[t - 1] + velocity_change_per_second
    derived_angles[t] <- derived_angles[t - 1] + angular_change_per_second
    derived_x[t] <- derived_x[t - 1] + derived_velocities[t] * cos(derived_angles[t])
    derived_y[t] <- derived_y[t - 1] + derived_velocities[t] * sin(derived_angles[t])
  }
  
  d <- data.table(x = derived_x[2 : estimated_num_seconds_to_derive],
                  y = derived_y[2 : estimated_num_seconds_to_derive])
  d <- data.table(rbind.fill(before, d, trip_data_table[bad_velocity_data_row_num], after))
  d <- calc_trip_data(d)
  print(d)
  d <- d[(num_rows_before_after + 1) : 
           (num_rows_before_after + estimated_num_seconds_to_derive - 1)]
  
  rbind(trip_data_table[1 : (bad_velocity_data_row_num - 1)], d,
        trip_data_table[bad_velocity_data_row_num : nrow(trip_data_table)])  
}


#clean_trip_data_table(trip_data_table):
#  return trip_data_table



angular_differences <- function(angles_1, angles_2)
{
  differences <- angles_1 - angles_2
  atan2(sin(differences), cos(differences))
}



circular_arc_length <- function(euclidean_distance_between_ends, angle_between_tangents)
{
  r = (euclidean_distance_between_ends / 2) / cos(angle_between_tangents / 2)
  (pi - abs(angle_between_tangents)) * r
}