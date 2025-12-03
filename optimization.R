# optimization

source("estimation.R")
source("simulation.R")

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

optimize_placement(sim_df, 50, 180)
