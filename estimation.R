library(dplyr)
# install.packages("lubridate")
library(lubridate)

bike <- read.csv("~/Downloads/sample_bike.csv")

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

