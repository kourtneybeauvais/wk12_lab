# tests

source("estimation.R")
source("simulation.R")
source("optimization.R")


test_df <- data.frame(
  start_station = c("A", "A", "A", "B", "B", "B", "B"),
  end_station = c("B", "B", "B", "A", "A", "A", "A"),
  hour = c(1, 2, 3, 0, 1, 2, 3),
  mu_hat = c(1, 2, 1, 3, 1, 0, 1)
)

test_sim_demand <- simulate_demand(test_df, 9857)

test_sim_df <- data.frame(
  hour = c(0, 1, 1, 1, 2, 3, 3),
  start_station = c("A", "A", "A", "B", "B", "B", "B"),
  end_station = c("B", "B", "B", "A", "A", "A", "A"),
  start_time = c(0.3, 1.145, 1.56, 1.98, 2.87, 3.67, 3.908),
  end_time = c(0.3, 1.145, 1.56, 1.98, 2.87, 3.67, 3.908)
)

test_sim_trips <- simulate_trips(test_sim_df, 150)

test_placement <- data.frame(start_station = unique(sim_df$start_station), 
                             n_bikes = rep(10, length(unique(sim_df$start_station))))
  
  