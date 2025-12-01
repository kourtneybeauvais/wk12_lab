# placement
source("estimation.R")
source("simulation.R")

my_opt <- function(filename){
  rates_df <- nhpp(filename)
  sim_df <- simulate_day(rates_df)
  
}

filename <- "~/Downloads/sample_bike.csv"
my_opt(filename)

