source("estimation.R")
source("simulation.R")
source("optimization.R")

call_all_functions <- function(path, seed, fleet_size, n_iter) {
  bikes  <- read.csv(path)
  rates  <- nhpp(bikes)
  simd   <- simulate_demand(rates, seed)
  best   <- optimize_placement(simd, n_iter, fleet_size)
  return(best)
}

fleet_75 <- call_all_functions("~/Documents/GitHub/wk12_lab/sample_bike.csv",
                   1234, 75, 100)

fleet_75$score[1]
fleet_75$placement[1]

fleet_125 <- call_all_functions("~/Documents/GitHub/wk12_lab/sample_bike.csv",
                              1234, 125, 100)

fleet_125$score[1]
fleet_125$placement[1]

fleet_175 <- call_all_functions("~/Documents/GitHub/wk12_lab/sample_bike.csv",
                               1234, 175, 100)

fleet_175$score[1]
fleet_175$placement[1]

