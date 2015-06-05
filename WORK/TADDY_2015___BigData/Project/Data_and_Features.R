library(data.table)
library(plyr)


list_drivers <- function(data_folder_path) {
  list.dirs(path = data_folder_path, full.names = FALSE, recursive = FALSE)
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


angle_between_2_points <- function(x_from, y_from, x_to, y_to) {
  atan2(y_to - y_from, x_to - x_from)
}


angular_difference <- function(from_angle, to_angle) {
  angular_diff <- to_angle - from_angle
  atan2(sin(angular_diff), cos(angular_diff))
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
  trip_data_table[, velocity := euclidean_norm(dx, dy)]
  trip_data_table
}


calc_acceleration <- function(trip_data_table) {
  trip_data_table[, acceleration := scalar_product(ddx, ddy, dx, dy) / velocity]
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
  trip_data_table[, abs_angular_velocity := abs(angular_velocity)]
  trip_data_table
}


calc_angular_acceleration <- function(trip_data_table) {
  trip_data_table[, angular_acceleration := c(NA, diff(angular_velocity))]
  trip_data_table$angular_acceleration[1] = trip_data_table$angular_acceleration[2]
  trip_data_table[, abs_angular_acceleration := abs(angular_acceleration)]
  trip_data_table  
}


calc_velocity_validity <- function(trip_data_table, max_velocity_m_per_s=50) {
  trip_data_table[, velocity_check := velocity < max_velocity_m_per_s]
  trip_data_table
}


calc_angular_velocity_validity <- function(trip_data_table,
                                           max_absolute_angular_velocity = 5 / 6 * pi) {
  trip_data_table[, angular_velocity_check := 
                    abs_angular_velocity < max_absolute_angular_velocity]
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
        max_velocity_m_per_s = max_velocity_m_per_s),
      max_absolute_angular_velocity = max_absolute_angular_velocity))
}


check_trip_data_quality <- function(trip_data_table) {
  all(trip_data_table$overall_check)
}


bad_velocity_data_indices <- function(trip_data_table) {
  which(!trip_data_table$velocity_check)
}



intersection_xy_and_signed_side_lengths <- function(x1, y1, a1, x2, y2, a2) {
  c1 <- cos(a1)
  s1 <- sin(a1)
  c2 <- cos(a2)
  s2 <- sin(a2)
  dx <- x2 - x1
  dy <- y2 - y1
  d <- c1 * s2 - s1 * c2
  signed_side1 <- (s2 * dx - c2 * dy) / d
  signed_side2 <- (s1 * dx - c1 * dy) / d
  c(x1 + c1 * signed_side1, y1 + s1 * signed_side1, signed_side1, signed_side2)
}


UNIT_TEST___intersection_xy_and_side_lengths <- function(num_times = 1000) {
  num_successes <- 0
  for (t in 1 : num_times) {
    cat('Test #', t, '\r', sep = "")
    coordinates <- runif(4, -10.0, 10.0)
    x1 <- coordinates[1]
    x2 <- coordinates[2]
    y1 <- coordinates[3]
    y2 <- coordinates[4]
    angles <- runif(2, -pi, pi)
    a1 <- angles[1]
    a2 <- angles[2]
    xy_and_signed_sides <- intersection_xy_and_signed_side_lengths(x1, y1, a1, x2, y2, a2)
    x <- xy_and_signed_sides[1]
    y <- xy_and_signed_sides[2]
    signed_side1 <- xy_and_signed_sides[3]
    signed_side2 <- xy_and_signed_sides[4]
    d1 <- euclidean_norm(x1, y1, x, y)
    d2 <- euclidean_norm(x2, y2, x, y)
    num_successes = num_successes + all.equal(d1, abs(signed_side1)) * all.equal(d2, abs(signed_side2))
  }
  cat(num_successes, 'Successes out of', num_times, 'Tests')
}


intersection_xy <- function(x1_from, y1_from, x1_to, y1_to, x2_from, y2_from, x2_to, y2_to) {
  a1 <- atan2(y1_to - y1_from, x1_to - x1_from)
  a2 <- atan2(y2_to - y2_from, x2_to - x2_from)
  xy_and_signed_sides <- intersection_xy_and_signed_side_lengths(x1_from, y1_from, a1,
                                                                 x2_from, y2_from, a2)
  xy_and_signed_sides[1 : 2]
}


UNIT_TEST___intersection_xy <- function(num_times = 1000) {
  num_successes <- 0
  for (t in 1 : num_times) {
    cat('Test #', t, '\r', sep = "")
    coordinates <- runif(2, -10.0, 10.0)
    x <- coordinates[1]
    y <- coordinates[2]
    angles <- runif(2, -pi, pi)
    a1 <- angles[1]
    c1 <- cos(a1)
    s1 <- sin(a1)
    a2 <- angles[2]
    c2 <- cos(a2)
    s2 <- sin(a2)
    distances <- runif(4, -10.0, 10.0)
    x1_from <- x + c1 * distances[1]
    y1_from <- y + s1 * distances[1]
    x1_to <- x + c1 * distances[2]
    y1_to <- y + s1 * distances[2]
    x2_from <- x + c2 * distances[3]
    y2_from <- y + s2 * distances[3]
    x2_to <- x + c2 * distances[4]
    y2_to <- y + s2 * distances[4]
    xy <- intersection_xy(x1_from, y1_from, x1_to, y1_to, x2_from, y2_from, x2_to, y2_to)
    num_successes = num_successes + all.equal(xy[1], x) * all.equal(xy[2], y)
  }
  cat(num_successes, 'Successes out of', num_times, 'Tests')
}









interpolate_xy <- function(x1, y1, v1, a1, x2, y2, v2, a2, max_acute_angle = 5 * pi / 6,
                           start_time = NULL, max_time = 6) {
  #print(start_time)
  if (is.null(start_time)) {
    recursion_start_time = Sys.time()
  } else {
    recursion_start_time = start_time
    if (Sys.time() > start_time + max_time) {
      cat('INTERPOLATION TAKING TOO LONG --> forcibly skipped!\n')
      return('skipped')
    }
  }
  
  
  d <- euclidean_norm(x1, y1, x2, y2)
  
  if (d <= max(v1, v2)) {
    return(NULL)
  } else {
    
    mean_velocity <- (v1 + v2) / 2
    
    xy_and_signed_sides <- intersection_xy_and_signed_side_lengths(x1, y1, a1, x2, y2, a2)
    x <- xy_and_signed_sides[1]
    y <- xy_and_signed_sides[2]
    signed_side1 <- xy_and_signed_sides[3]
    signed_side2 <- xy_and_signed_sides[4]
    
    angular_diff <- angular_difference(a1, a2)
    
    if ((signed_side1 > 0) & (signed_side2 < 0)) {   # most standard case
      
      d1 = signed_side1
      d2 = - signed_side2
      
      if (abs(angular_diff) < max_acute_angle) {
        
        half_perimeter <- (d + d1 + d2) / 2
        
        if (half_perimeter > 3 * mean_velocity) {
          estimated_num_seconds <- floor(half_perimeter / mean_velocity)
          seq1_x <- seq(x1, x, length.out = estimated_num_seconds)
          seq1_y <- seq(y1, y, length.out = estimated_num_seconds)
          seq2_x <- seq(x, x2, length.out = estimated_num_seconds)
          seq2_y <- seq(y, y2, length.out = estimated_num_seconds)
          intersection_x_series <- numeric()
          intersection_y_series <- numeric()
          curved_distance <- 0
          current_x <- x1
          current_y <- y1
          for (t in 1 : (estimated_num_seconds - 1)) {
            next_xy <- intersection_xy(seq1_x[t], seq1_y[t], seq2_x[t], seq2_y[t],
                                       seq1_x[t + 1], seq1_y[t + 1], seq2_x[t + 1], seq2_y[t + 1])
            next_x <- next_xy[1]
            next_y <- next_xy[2]
            curved_distance <- curved_distance + euclidean_norm(current_x, current_y, next_x, next_y)
            current_x <- next_x
            current_y <- next_y
            intersection_x_series <- append(intersection_x_series, current_x)
            intersection_y_series <- append(intersection_y_series, current_y)
          }
          curved_distance <- curved_distance + euclidean_norm(current_x, current_y, x2, y2)
          
          reestimated_num_seconds <- floor(curved_distance / mean_velocity)
          if (reestimated_num_seconds != estimated_num_seconds) {
            estimated_num_seconds <- reestimated_num_seconds
            seq1_x <- seq(x1, x, length.out = estimated_num_seconds)
            seq1_y <- seq(y1, y, length.out = estimated_num_seconds)
            seq2_x <- seq(x, x2, length.out = estimated_num_seconds)
            seq2_y <- seq(y, y2, length.out = estimated_num_seconds)
            intersection_x_series <- numeric()
            intersection_y_series <- numeric()
            for (t in 1 : (estimated_num_seconds - 1)) {
              next_xy <- intersection_xy(seq1_x[t], seq1_y[t], seq2_x[t], seq2_y[t],
                                         seq1_x[t + 1], seq1_y[t + 1], seq2_x[t + 1], seq2_y[t + 1])
              next_x <- next_xy[1]
              next_y <- next_xy[2]
              intersection_x_series <- append(intersection_x_series, next_x)
              intersection_y_series <- append(intersection_y_series, next_y)
            }
          }
          
          return(list(x = intersection_x_series, y = intersection_y_series))
          
        } else {   # if (d1 + d2 > 2 * mean_velocity)
          
          return(list(x=x, y=y))
          
        }
        
      } else {  # if not (abs(angular_diff) < max_acute_angle)
        
        m_x <- (x1 + x2) / 2
        m_y <- (y1 + y2) / 2
        angle_of_ray_from_midpoint <- angle_between_2_points(m_x, m_y, x, y)
        r <- d / 2
        x <- m_x + cos(angle_of_ray_from_midpoint) * r
        y <- m_y + sin(angle_of_ray_from_midpoint) * r
        a <- angle_of_ray_from_midpoint + pi / 2
        intersection_xy_with_side2 <-
          intersection_xy_and_signed_side_lengths(x, y, a, x2, y2, a2)
        intersection_x <- intersection_xy_with_side2[1]
        intersection_y <- intersection_xy_with_side2[2]
        a <- angle_between_2_points(x, y, intersection_x, intersection_y)
        
        interpolated_xy_1 <- interpolate_xy(x1, y1, v1, a1, x, y, mean_velocity, a,
                                            5 * pi / 6,
                                            recursion_start_time)
        if ((!is.null(interpolated_xy_1)) & is.atomic(interpolated_xy_1)) {
          return('skipped')
        }
        interpolated_xy_2 <- interpolate_xy(x, y, mean_velocity, a, x2, y2, v2, a2,
                                            5 * pi / 6,
                                            recursion_start_time)
        if ((!is.null(interpolated_xy_2)) & is.atomic(interpolated_xy_2)) {
          return('skipped')
        }
        return(list(x = c(interpolated_xy_1$x, x, interpolated_xy_2$x),
                    y = c(interpolated_xy_1$y, y, interpolated_xy_2$y)))
        
      }
      
    } else if ((signed_side1 < 0) & (signed_side2 > 0)) {
      
      m_x <- (x1 + x2) / 2
      m_y <- (y1 + y2) / 2
      angle_of_ray_from_midpoint <- angle_between_2_points(x, y, m_x, m_y)
      r <- d / 2
      x <- m_x + cos(angle_of_ray_from_midpoint) * r
      y <- m_y + sin(angle_of_ray_from_midpoint) * r
      a <- angle_of_ray_from_midpoint + pi / 2
      intersection_xy_with_side2 <-
        intersection_xy_and_signed_side_lengths(x, y, a, x2, y2, a2)
      intersection_x <- intersection_xy_with_side2[1]
      intersection_y <- intersection_xy_with_side2[2]
      a <- angle_between_2_points(x, y, intersection_x, intersection_y)
      
      interpolated_xy_1 <- interpolate_xy(x1, y1, v1, a1, x, y, mean_velocity, a,
                                          5 * pi / 6,
                                          recursion_start_time)
      if ((!is.null(interpolated_xy_1)) & is.atomic(interpolated_xy_1)) {
        return('skipped')
      }
      interpolated_xy_2 <- interpolate_xy(x, y, mean_velocity, a, x2, y2, v2, a2,
                                          max_acute_angle = max_acute_angle,
                                          start_time = recursion_start_time)
      if ((!is.null(interpolated_xy_2)) & is.atomic(interpolated_xy_2)) {
        return('skipped')
      }
      return(list(x = c(interpolated_xy_1$x, x, interpolated_xy_2$x),
                  y = c(interpolated_xy_1$y, y, interpolated_xy_2$y)))
      
    } else if ((signed_side1 > 0) & (signed_side2 > 0)) {
      
      m_x <- (x1 + x2) / 2
      m_y <- (y1 + y2) / 2
      from_x <- (3 * x1 + x) / 4
      from_y <- (3 * y1 + y) / 4
      a <- angle_between_2_points(from_x, from_y, m_x, m_y)
      interpolated_xy_1 <- interpolate_xy(x1, y1, v1, a1, m_x, m_y, mean_velocity, a,
                                          max_acute_angle = max_acute_angle,
                                          start_time = recursion_start_time)
      if ((!is.null(interpolated_xy_1)) & is.atomic(interpolated_xy_1)) {
        return('skipped')
      }
      interpolated_xy_2 <- interpolate_xy(m_x, m_y, mean_velocity, a, x2, y2, v2, a2,
                                          max_acute_angle = max_acute_angle,
                                          start_time = recursion_start_time)
      if ((!is.null(interpolated_xy_2)) & is.atomic(interpolated_xy_2)) {
        return('skipped')
      }
      return(list(x = c(interpolated_xy_1$x, m_x, interpolated_xy_2$x),
                  y = c(interpolated_xy_1$y, m_y, interpolated_xy_2$y)))
      
    } else {   # if ((signed_side1 < 0) & (signed_side2 < 0)) 
      
      m_x <- (x1 + x2) / 2
      m_y <- (y1 + y2) / 2
      to_x <- (3 * x2 + x) / 4
      to_y <- (3 * y2 + y) / 4
      a <- angle_between_2_points(m_x, m_y, to_x, to_y)
      interpolated_xy_1 <- interpolate_xy(x1, y1, v1, a1, m_x, m_y, mean_velocity, a,
                                          max_acute_angle = max_acute_angle,
                                          start_time = recursion_start_time)
      if ((!is.null(interpolated_xy_1)) & is.atomic(interpolated_xy_1)) {
        return('skipped')
      }
      interpolated_xy_2 <- interpolate_xy(m_x, m_y, mean_velocity, a, x2, y2, v2, a2,
                                          max_acute_angle = max_acute_angle,
                                          start_time = recursion_start_time)
      if ((!is.null(interpolated_xy_2)) & is.atomic(interpolated_xy_2)) {
        return('skipped')
      }
      return(list(x = c(interpolated_xy_1$x, m_x, interpolated_xy_2$x),
                  y = c(interpolated_xy_1$y, m_y, interpolated_xy_2$y)))
    }
  }
}


clean_velocity_data <- function(trip_data_table, num_rows_before_after = 3) {
  while (!all(trip_data_table$velocity_check)) {
    bad_velocity_data_row_num = which(!trip_data_table$velocity_check)[1]
    
    if (bad_velocity_data_row_num <= num_rows_before_after) {
      trip_data_table <- trip_data_table[-c(1 : bad_velocity_data_row_num)]
    } else if (bad_velocity_data_row_num > nrow(trip_data_table) - 2 * num_rows_before_after) {
      trip_data_table <- trip_data_table[1 : (bad_velocity_data_row_num - 1)]
    } else {
      before_start_row <- bad_velocity_data_row_num - num_rows_before_after
      before_end_row <- bad_velocity_data_row_num - 1
      before <- trip_data_table[before_start_row : before_end_row]
      
      after_start_row <- bad_velocity_data_row_num + num_rows_before_after
      after_end_row <- bad_velocity_data_row_num + 2 * num_rows_before_after - 1
      after <- trip_data_table[after_start_row : after_end_row]
      
      x_before <- before$x[num_rows_before_after]
      y_before <- before$y[num_rows_before_after]
      velocity_before <- mean(before$velocity)
      angle_before <- mean(before$angle)
      
      x_after <- after$x[1]
      y_after <- after$y[1]
      velocity_after <- mean(after$velocity)
      angle_after <- mean(after$angle)
      
      interpolated_xy <- interpolate_xy(x_before, y_before, velocity_before, angle_before,
                                        x_after, y_after, velocity_after, angle_after)
      if ((!is.null(interpolated_xy)) & is.atomic(interpolated_xy)) {
        return('skipped')
      }
      
      d <- data.table(rbind.fill(before, as.data.table(interpolated_xy), after))
      d <- calc_trip_data(d)
      num_rows <- nrow(d)
      d <- d[-c(1 : num_rows_before_after, (num_rows - num_rows_before_after + 1) : num_rows)]
      
      trip_data_table <- rbind(trip_data_table[1 : before_end_row], d,
                               trip_data_table[after_start_row : nrow(trip_data_table)])
    }
  }
  trip_data_table
}


clean_velocity_data_for_all_driver_trips <- function(superfolder_path) {
  source('Visualization.R')
  data_folder_path <- file.path(superfolder_path, 'drivers')
  drivers <- list_drivers(data_folder_path)
  num_drivers <- length(drivers)
  unclean <- list()
  unclean_count <- 0
  skipped <- list()
  visualization_folder_path <- file.path(superfolder_path, 'data_cleaning_visuals')
  for (i in 1 : num_drivers) {
    progress = 100 * i / num_drivers
    driver <- drivers[i]
    driver_folder_path <- file.path(data_folder_path, driver) 
    for (trip in 1 : 200) {
      cat('Checking Driver #', driver, ' (', i, ' / ', num_drivers,
          ' = ', formatC(progress, format="f", digits = 1), '%) Trip #', trip,
          ' | unclean trips so far = ', unclean_count, '   \r', sep = "")
      trip_data <- calc_trip_data(read_driver_trip(data_folder_path, driver, trip))
      if (any(!trip_data$velocity_check)) {
        unclean_count <- unclean_count + 1
        if (driver %in% names(unclean)) {
          unclean[[driver]] <- append(unclean[[driver]], trip)
        } else {
          unclean[[driver]] <- trip
        }
        ggsave(file.path(visualization_folder_path,
                         paste(driver, '_', trip, '_bad.png', sep = "")),
               plot_trip(trip_data, color="blue"))
        
        #trip_data <- clean_velocity_data(trip_data)
        #if ((!is.null(trip_data)) & is.atomic(trip_data)) {
        #  if (driver %in% names(skipped)) {
        #    skipped[[driver]] <- append(skipped[[driver]], trip)
        #  } else {
        #    skipped[[driver]] <- trip
        #  }
        #} else {
        #  ggsave(file.path(visualization_folder_path,
        #                   paste(driver, '_', trip, '_clean.png', sep = "")),
        #         plot_trip(trip_data, color="blue"))
        #  saveRDS(trip_data, file.path(data_folder_path, driver, paste(trip, '.RDS', sep = "")))
        #}
      } else {
        saveRDS(trip_data, file.path(data_folder_path, driver, paste(trip, '.RDS', sep = "")))
      }
    }
  }
  saveRDS(unclean, file.path(superfolder_path, 'unclean_velocity_data_cases.RDS'))
  #saveRDS(skipped, file.path(superfolder_path, 'unclean_velocity_data_cases_skipped.RDS'))
  list(unclean=unclean)  #, skipped=skipped
}