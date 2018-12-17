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
                            area_thresh = 1500, crowd_thresh = 200, crown_bins = 8)
maczoo <- read_fishnet_data("nest_counts/fishnet_counts/maczoo_rf_data.csv",
                            area_thresh = 2400, crowd_thresh = 1000, crown_bins = 8)
eulimbah <- read_fishnet_data("nest_counts/fishnet_counts/eulimbah_rf_data.csv",
                              area_thresh = 1000, crowd_thresh = 200, crown_bins = 8)



# manual counting accuracy ------------------------------------------------

# manually counted in-situ and via drone imagery
merrimajeel_counts <- data.frame(ground = c(17,22,27,27,24,20,8,13,14,25,9,16),
                                 drone = c(16,21,26,21,29,21,11,16,14,26,14,18))
merrimajeel_interval <- get_interval(merrimajeel_counts$ground, merrimajeel_counts$drone)

blockb_counts <- data.frame(ground = c(41,48,23,30,18,23,18,3),
                                 drone = c(30,46,19,28,19,16,18,4))
blockb_interval <- get_interval(blockb_counts$ground, merrimajeel_counts$drone)

maczoo_counts <- data.frame(ground = c(8,26,11,20,2,12,30,19,8,8,10,12,26,14),
                            drone = c(8,18,9,10,0,9,36,14,6,4,9,8,19,4))
maczoo_interval <- get_interval(maczoo_counts$ground, merrimajeel_counts$drone)

eulimbah_counts <- data.frame(ground = c(5,2,5,17,5,17,3,17,15,28,15,129),
                            drone = c(6,4,5,22,4,15,4,17,6,17,16,116))
eulimbah_interval <- get_interval(eulimbah_counts$ground, merrimajeel_counts$drone)


                                

# crunch the numbers for each colony --------------------------------------

## see function in "nest_counting_functions.R" - argument description here:
# crunch_it <- function(run_iters = F, save = F, # whether to re-run analysis/save data or just load and plot
#                       colony_name, colony_data, # name of the colony (string) and loaded colony data
#                       nboot = 1000, # number of bootstrap iterations for nest count estimates
#                       smaller_sample = 5, num_draws = 3, # number of grids to sample, repeated num_draws times ##### XXX ##### number of samples to take from each crowd ratio bin (from data load), repeated num_draws times
#                       quants = c(0.1,0.9), # central quantile to sample from (trim extreme grids you wouldn't choose to count) 
#                       model_type = NULL, # choose 'area', 'glm' or 'gam'
#                       kfold_iters = 50, kfold_k = 10, # kfold iters and k for simulation
#                       error_level = 0.05){ # manual counting error level to compare on plots

run_resampling <- FALSE # whether to run or just plot
save_resampling <- FALSE # whether to save results to disk
model_type <- 'area' # choose area, glm or gam to model estimates
nboot <- 800
num_draws <- 40
plot_only <- T

## merrimajeel creek - lower lachlan river
merrimajeel_estimates <- 
  crunch_it(run_iters = run_resampling, save = save_resampling,
          "merrimajeel", merrimajeel, 
          nboot = nboot, 
          smaller_sample = 30, num_draws = num_draws,
          model_type = model_type,
          quants = c(0.05,0.95),
          kfold_iters = 10, kfold_k = 10,
          error_level = merrimajeel_interval/2,
          plot_only = plot_only,
          title = "Merrimajeel")

# block bank - lower lachlan river
blockb_estimates <- 
  crunch_it(run_iters = run_resampling, save = save_resampling,
          "blockb", blockb, 
          nboot = nboot, 
          smaller_sample = 10, num_draws = num_draws,
          model_type = model_type,
          quants = c(0.1,0.9),
          kfold_iters = 10, kfold_k = 10,
          error_level = blockb_interval/2,
          plot_only = plot_only,
          title = "Block Bank")

# zoo paddock - macquaries marshes
zoomac_estimates <- 
  crunch_it(run_iters = run_resampling, save = save_resampling,
          "maczoo", maczoo, 
          nboot = nboot, 
          smaller_sample = 30, num_draws = num_draws,
          model_type = model_type, 
          quants = c(0.1,0.9),
          kfold_iters = 10, kfold_k = 10,
          error_level = maczoo_interval/2,
          plot_only = plot_only,
          title = "Zoo Paddock")

# eulimbah - lowbidgee
eulimbah_estimates <- 
  crunch_it(run_iters = run_resampling, save = save_resampling,
          "eulimbah", eulimbah, 
          nboot = nboot, 
          smaller_sample = 15, num_draws = num_draws,
          model_type = model_type,
          quants = c(0.1,0.9),
          kfold_iters = 10, kfold_k = 10,
          error_level = eulimbah_interval/2,
          plot_only = plot_only,
          title = "Eulimbah")



# paper figures -----------------------------------------------------------

library(gridExtra)

## per-grid estimates figure (supp. fig.)
grid.arrange(estimate_nests_kfold_grids(30, merrimajeel, 10, "Merrimajeel"),
             estimate_nests_kfold_grids(15, blockb, 10, "Block Bank"),
             estimate_nests_kfold_grids(30, maczoo, 10, "Zoo Paddock"),
             estimate_nests_kfold_grids(10, eulimbah, 10, "Eulimbah"),
             ncol = 2)


## monte carlo resamplign estimates (supp. fig.)
grid.arrange(merrimajeel_estimates[[1]], merrimajeel_estimates[[2]],
             blockb_estimates[[1]], blockb_estimates[[2]],
             zoomac_estimates[[1]], zoomac_estimates[[2]],
             eulimbah_estimates[[1]], eulimbah_estimates[[2]],
             ncol = 2)


## k-fold figure
merrim_kfold <- readRDS("merrimajeel_kfold_estimates.rds")
blockb_kfold <- readRDS("blockb_kfold_estimates.rds")
maczoo_kfold <- readRDS("maczoo_kfold_estimates.rds")
eulimb_kfold <- readRDS("eulimbah_kfold_estimates.rds")

merrim_plt <- plot_nest_estimates_kfold(merrim_kfold, merrimajeel, merrimajeel_interval/2, "Merrimajeel")
blockb_plt <- plot_nest_estimates_kfold(blockb_kfold, blockb, blockb_interval/2, "Block Bank")
maczoo_plt <- plot_nest_estimates_kfold(maczoo_kfold, maczoo, maczoo_interval/2, "Zoo Paddock")
eulimb_plt <- plot_nest_estimates_kfold(eulimb_kfold, eulimbah, eulimbah_interval/2, "Eulimbah")

grid.arrange(merrim_plt, blockb_plt, maczoo_plt, eulimb_plt, ncol = 2)


