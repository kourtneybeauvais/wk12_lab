# simulation
library(tidyverse)

#' Simulate one full day of bike-trip demand using NHPP thinning
#'
#' This function uses the estimated rates (mu_hat) for each start–end station 
#' pair to simulate trip arrival times throughout a 24-hour day. It first creates 
#' a complete table of hourly rates for all station pairs, then uses thinning 
#' with each pair's mu_max to generate realistic, time-varying arrival times.
#'
#' @param rates_df A data frame produced by nhpp(). Must include columns:
#'   hour, start_station, end_station, mu_hat.
#'
#' @return A data frame with simulated trips, containing:
#'   \itemize{
#'     \item{\code{hour}}{Hour of day (0–23)}
#'     \item{\code{start_station}}{Starting station ID}
#'     \item{\code{end_station}}{Ending station ID}
#'     \item{\code{start_time}}{Exact simulated arrival time (0–24, in hours)}
#'     \item{\code{end_time}}{Same as start_time (no duration modeled yet)}
#'   }
#'   

simulate_demand <- function(rates_df) {
  # identify each station pairing and its mu_max
  pairs <- rates_df %>% 
    group_by(start_station, end_station) %>%
    summarize(mu_max = max(mu_hat), .groups = "drop")
  
  complete_arrivals <- data.frame(hour = c(0),
                                  start_station = c(0),
                                  end_station = c(0),
                                  mu_hat = c(0),
                                  mu_max = c(0))
  
  # for each start/end station pairing,
  # identify mu_max and mu_hat, including every hour 0-23 (complete data frame)
  for (i in 1:nrow(pairs)) {
    pair_start <- pairs$start_station[i]
    pair_end <- pairs$end_station[i]
    pair_mu_max <- pairs$mu_max[i]
    for (j in 0:23) {
      row <- subset(rates_df, start_station == pair_start & end_station == pair_end & hour == j)
      if (nrow(row) == 0) {
        mu <- 0
      } else {
        mu <- row$mu_hat[1]
        }
      complete_arrivals <- rbind(complete_arrivals, 
                                 data.frame(hour = j,
                                            start_station = pair_start,
                                            end_station = pair_end,
                                            mu_max = pair_mu_max,
                                            mu_hat = mu))

    }
  }
  complete_arrivals <- complete_arrivals[-1,]
  
  sim_arrivals <- data.frame(hour = c(0),
                             start_station = c(0),
                             end_station = c(0),
                             start_time = c(0),
                             end_time = c(0))
  
  # simulate a day for each pair + thinning
  for (i in 1:nrow(pairs)) {
    time <- c(0)
    pair_start <- pairs$start_station[i]
    pair_end <- pairs$end_station[i]
    pair_mu_max <- pairs$mu_max[i]
    pair_subset <- subset(complete_arrivals, start_station == pair_start & end_station == pair_end)
    while (time[length(time)] < 24) {
      # possible next arrival
      next_time <- time[length(time)] + rexp(1, rate = pair_mu_max)
      
      if (next_time >= 24) break
      
      time <- c(time, next_time)  # always move time forward
      
      # thinning probability
      current_hour <- next_time - (next_time %% 1)
      p <- subset(pair_subset, 
                  hour == (time[length(time)] - (time[length(time)] %% 1)))$mu_hat/pair_mu_max
      p <- ifelse(length(p) == 0, 0, p)
      
      # append only if it passes thinning
      if (runif(1) < p) {
        sim_arrivals <- rbind(sim_arrivals, data.frame(
          hour = current_hour,
          start_station = pair_start,
          end_station = pair_end,
          start_time = next_time,
          end_time = next_time
        ))
        }
      }
  }
  sim_arrivals <- sim_arrivals[-1,]
  sim_arrivals <- sim_arrivals[order(sim_arrivals$start_time),]
  return(sim_arrivals)
}

sim_df <- simulate_demand(rates_df)

# initial placement strategy:
# 10 bikes per start station
init_placement <- data.frame(start_station = unique(sim_df$start_station), 
                        n_bikes = rep(10, length(unique(sim_df$start_station))))

#' Simulate bike movements through the system for one day
#'
#' This function takes an initial bike placement and a simulated list of trips,
#' then processes each trip sequentially. A trip succeeds if the start station
#' has at least one bike available. When successful, a bike is removed from the
#' start station and added to the end station. The function tracks whether each
#' simulated trip was successful and returns the updated placements.
#'
#' @param placement_df A data frame with at least:
#'   \describe{
#'     \item{start_station}{(character or numeric) Station ID}
#'     \item{n_bikes}{(integer) Number of bikes initially at each station}
#'   }
#'
#' @param sim_df A data frame of simulated demand from `simulate_demand()`,
#'   containing at least:
#'   \describe{
#'     \item{start_station}{(character or numeric) Station where the trip starts}
#'     \item{end_station}{(character or numeric) Destination station}
#'   }
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{results}{`sim_df` updated with a `successful_ride` column (0/1)}
#'     \item{final_placement}{Updated placement_df showing final bike counts}
#'   }
#'   

simulate_trips <- function(placement_df, sim_df) {
  
  sim_df$successful_ride <- 0
  
  for (i in 1:nrow(sim_df)) {
    
    start  <- sim_df$start_station[i]
    end    <- sim_df$end_station[i]
    
    # index of start station in placement_df
    j <- which(placement_df$start_station == start)
    
    if (length(j) == 1 && placement_df$n_bikes[j] > 0) {
      
      # remove one bike from start
      placement_df$n_bikes[j] <- placement_df$n_bikes[j] - 1
      
      # add bike to destination
      k <- which(placement_df$start_station == end)
      if (length(k) == 1) {
        placement_df$n_bikes[k] <- placement_df$n_bikes[k] + 1
      }
      sim_df$successful_ride[i] <- 1
    } else {
      sim_df$successful_ride[i] <- 0
    }
  }
  
  return(list(
    results = sim_df,
    final_placement = placement_df))
}




