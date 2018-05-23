library(mgcv)
library(MASS)
library(dplyr)
library(tidyr)
library(ggplot2)
#library(data.table)

source("nest_counting_functions.R")



# load nest data ----------------------------------------------------------

merrimajeel <- read_fishnet_data("nest_counts/fishnet_counts/merrimajeel_rf_data.csv", 
                                 area_thresh = 2400, crowd_thresh = 200, crown_bins = 8)
blockb <- read_fishnet_data("nest_counts/fishnet_counts/blockb_rf_data.csv",
                            area_thresh = 1500, crowd_thresh = 200, crown_bins = 4)
maczoo <- read_fishnet_data("nest_counts/fishnet_counts/maczoo_rf_data.csv",
                            area_thresh = 2400, crowd_thresh = 1000, crown_bins = 8)
eulimbah <- read_fishnet_data("nest_counts/fishnet_counts/eulimbah_rf_data.csv",
                              area_thresh = 1500, crowd_thresh = 200, crown_bins = 4)




# crunch the numbers for each colony --------------------------------------

## see function in "nest_counting_functions.R" - argument description here:
# crunch_it <- function(run_iters = F, save = F, # whether to re-run analysis/save data or just load and plot
#                       colony_name, colony_data, # name of the colony (string) and loaded colony data
#                       nboot = 1000, # number of bootstrap iterations for nest count estimates
#                       smaller_sample = 5, num_draws = 3, # number of samples to take from each crowd ratio bin (from data load), repeated num_draws times
#                       kfold_iters = 50, kfold_k = 10, # kfold iters and k for simulation
#                       error_level = 0.05){ # manual counting error level to compare on plots

## merrimajeel creek - lower lachlan river
crunch_it(run_iters = T, save = T,
          "merrimajeel", merrimajeel, 
          nboot = 800, 
          smaller_sample = 4, num_draws = 10,
          kfold_iters = 20, kfold_k = 10,
          error_level = 0.05)

# block bank - lower lachlan river
crunch_it(run_iters = T, save = T,
          "blockb", blockb, 
          nboot = 800, 
          smaller_sample = 3, num_draws = 10,
          kfold_iters = 20, kfold_k = 3,
          error_level = 0.05)

# zoo paddock - macquaries marches
crunch_it(run_iters = T, save = T,
          "maczoo", maczoo, 
          nboot = 800, 
          smaller_sample = 4, num_draws = 10,
          kfold_iters = 20, kfold_k = 10,
          error_level = 0.05)

# eulimbah - lowbidgee
crunch_it(run_iters = T, save = T,
          "eulimbah", eulimbah, 
          nboot = 800, 
          smaller_sample = 3, num_draws = 3,
          kfold_iters = 20, kfold_k = 3,
          error_level = 0.05)



