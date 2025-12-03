Bike Fleet Optimization Project – Week 12

This project simulates bike-sharing demand using historical trip data and proposes starting bike allocations for different fleet sizes (50, 75, 100 bikes). 
The work follows the design approved in Step 1 and is written in modular R scripts.

Project Structure:

estimation.R – Loads data and estimates hourly arrival rates (μ̂) using NHPP methods.
simulation.R – Simulates bike usage for a day using estimated rates.
placement.R – Builds initial allocation candidates and selects the best-performing one.
utils.R – Small helper functions.
test.R – Unit tests for main functions.


How It Works:

Load & clean data
Estimate hourly arrival rates per station pair
Simulate daily usage across multiple replications
Evaluate allocation strategies
Output recommended starting bikes per station for each fleet size


Dependencies:

R
dplyr
lubridate


Contributors:

Sara Sultani
Kourtney Beauvais