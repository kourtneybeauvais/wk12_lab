library(dplyr)
# install.packages("lubridate")
library(lubridate)

bike <- read.csv("~/Downloads/sample_bike.csv")

#' Estimate hourly NHPP (Non-Homogeneous Poisson Process) arrival rates
#'
#' This function takes raw bike-share trip data and computes estimated
#' hourly arrival rates (`mu_hat`) for each start–end station pair.
#' It removes rebalancing events, converts times, extracts the hour
#' of the day, counts trips within each hour, and determines whether
#' each station had any activity (alpha).
#'
#' @param df A data frame containing trip records with the following columns:
#'   - `start_station` (character or numeric)
#'   - `end_station` (character or numeric)
#'   - `start_time` (POSIX-convertible)
#'   - `end_time` (POSIX-convertible)
#'
#' @return A data frame with the following columns:
#'   - `hour` (integer 0–23)
#'   - `start_station` (integer)
#'   - `end_station` (integer)
#'   - `mu_hat` (numeric): estimated average hourly arrival rate
#'
#' @details
#' *Rebalancing trips* (where start or end station = "R") are removed.  
#' `alpha` is an indicator equal to 1 if at least one trip started at the
#' station during that hour; otherwise 0.  
#' `mu_hat` is computed as `n_trips / alpha`, and is 0 when alpha = 0.

nhpp <- function(df) {
  # only keep real trips
  df_trips <- df %>% filter(start_station != "R" & end_station != "R")
  
  df_trips$start_station <- as.integer(df_trips$start_station)
  df_trips$end_station <- as.integer(df_trips$end_station)
  df_trips$start_time <- as_datetime(df_trips$start_time)
  df_trips$end_time <- as_datetime(df_trips$end_time)
  
  results <- data.frame()
  
  for (hr in 0:23) {
    # filter for trips starting within the hour
    trips_hr <- df_trips %>% 
      filter(hour(start_time) == hr)
    
    # x hat: number of trips from s to t in hour h
    x_hat <- trips_hr %>%
      group_by(start_station, end_station) %>%
      summarize(n_trips = n(), .groups = 'drop')
    
    # for alpha hat: fraction of time station s "had bikes" in hour h
    # (did at least one trip start here during that hour)
    active_stations <- trips_hr %>%
      group_by(start_station) %>%
      summarize(alpha = 1, .groups = 'drop') # alpha = 1 if active this hour
    
    # merge trip counts and station activity 
    # if start_station not active, alpha = 0
    x_hat <- x_hat %>%
      left_join(active_stations, by = "start_station") %>%
      mutate(alpha = ifelse(is.na(alpha), 0, alpha),
             mu_hat = ifelse(alpha > 0, n_trips / alpha, 0),
             hour = hr)
    
    results <- rbind(results, x_hat)
  }
  results <- results %>%
    select(hour, start_station, end_station, mu_hat)
  return(results)
}

