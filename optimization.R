# optimization

source("estimation.R")
source("simulation.R")

#' Optimization
#' 
#' This function iteratively optimizes the initial distribution of bikes across
#' stations in order to maximize the fraction of successful simulated trips.
#' At each iteration a single bike is moved from one station to another, and the 
#' new placement is accepted only if it improves the simulated success rate.
#'
#' @param sim_df A data frame of simulated trips produced by simulate_demand().
#' @param n_iter Integer. Number of optimization iterations to run.
#' @param fleet_size Integer. Total number of bikes available for allocation.

optimize_placement <- function(sim_df, n_iter = 200, fleet_size) {
  
  initialize_placement <- function(sim_df, fleet_size) {
    stations <- sort(unique(sim_df$start_station))
    n <- length(stations)
    
    base <- floor(fleet_size / n)
    remainder <- fleet_size %% n
    
    data.frame(
      start_station = stations,
      n_bikes = base + c(rep(1, remainder), rep(0, n - remainder))
    )
  }
  
  # starting point
  current <- initialize_placement(sim_df, fleet_size)
  current_score <- mean(simulate_trips(current, sim_df)$results$successful_ride)
  
  history <- data.frame(iter = 0,
                        score = current_score,
                        placement = I(list(current)))
  
  for (iter in 1:n_iter) {
    
    # propose a new placement by moving ONE bike
    proposal <- current
    
    # randomly choose donor and receiver stations
    donor <- sample(which(proposal$n_bikes > 0), 1)
    receiver <- sample(1:nrow(proposal), 1)
    while (receiver == donor) {
      receiver <- sample(1:nrow(proposal), 1)
    }
    
    # apply the move
    proposal$n_bikes[donor] <- proposal$n_bikes[donor] - 1
    proposal$n_bikes[receiver] <- proposal$n_bikes[receiver] + 1
    
    # evaluate new placement
    new_score <- mean(simulate_trips(proposal, sim_df)$results$successful_ride)
    
    # accept if better
    if (new_score > current_score) {
      current <- proposal
      current_score <- new_score
    }
    
    # record iteration
    history <- rbind(history, 
                     data.frame(iter = iter,
                                score = current_score,
                                placement = I(list(current))))
  }
  best <- history %>% slice_max(score, n = 1)
  return(best)
}

# RUN ALL

bike <- read.csv("~/Documents/GitHub/wk12_lab/sample_bike.csv")

call_all_functions <- function(df, seed, fleet_size, n_iter) {
  a <- nhpp(df)
  b <- simulate_demand(a, seed)
  c <- optimize_placement(b, n_iter, fleet_size)
  
  return(c)
}

call_all_functions(bike, 1234, 180, 100)


