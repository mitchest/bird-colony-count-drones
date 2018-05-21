library(mgcv)
library(MASS)
library(dplyr)
library(tidyr)
library(ggplot2)
#library(data.table)

source("nest_counting_functions.R")



# load nest data ----------------------------------------------------------

merrimajeel <- read.csv("nest_counts/merrimajeel_nests_50m.csv", stringsAsFactors = F) %>%
  na.omit() %>%
  rename(nest_count = Species) %>%
  filter(net_area == 2500) %>%
  mutate(nest_area_ratio = nest_area / nest_count,
         crowd_ratio = net_area / nest_area) %>%
  filter(crowd_ratio < 200) %>%
  mutate(crowd_bin = ntile(crowd_ratio, 8))



# merrimajeel -------------------------------------------------------------

## straight up area ratio
# merrimajeel_estimates <- bind_cols(lapply(1:221, estimate_nest_counts, 1000, merrimajeel))
# saveRDS(merrimajeel_estimates, file = "merrimajeel_estimates.rds")
merrimajeel_estimates <- readRDS("merrimajeel_estimates.rds")
merrimajeel_estimates_long <- gather(merrimajeel_estimates)
merrimajeel_estimates_long$samples <- as.numeric(unlist(lapply(merrimajeel_estimates_long$key, function(x) strsplit(x, "_")[[1]][2])))
plot_nest_estimates(merrimajeel_estimates_long, merrimajeel, 0.05)

## model nest area and crowdedness
# merrimajeel_estimates_glm <- bind_cols(lapply(1:221, estimate_nest_counts_glm, 1000, merrimajeel))
# saveRDS(merrimajeel_estimates_glm, file = "merrimajeel_estimates_glm.rds")
merrimajeel_estimates_glm <- readRDS("merrimajeel_estimates_glm.rds")
merrimajeel_estimates_glm_long <- gather(merrimajeel_estimates_glm)
merrimajeel_estimates_glm_long$samples <- as.numeric(unlist(lapply(merrimajeel_estimates_glm_long$key, function(x) strsplit(x, "_")[[1]][2])))
plot_nest_estimates(merrimajeel_estimates_glm_long, merrimajeel, 0.05)

## test out resampling with smaller sampling size
kfold_samples <- rep(c(5, 6), 10)
samples_actual_n <- kfold_samples * length(unique(merrimajeel$crowd_bin))
# glm
merrimajeel_kfold_estimates_glm <- lapply(X = kfold_samples,
                                      FUN = estimate_nests_kfold, merrimajeel, 50, 10, 'glm')
names(merrimajeel_kfold_estimates_glm) <- paste0(rep(1:10, each = 2),"_",samples_actual_n)
plot_nest_estimates_kfold(merrimajeel_kfold_estimates_glm, merrimajeel, 0.05)
# gam
merrimajeel_kfold_estimates_gam <- lapply(X = kfold_samples,
                                          FUN = estimate_nests_kfold, merrimajeel, 50, 10, 'gam')
names(merrimajeel_kfold_estimates_gam) <- paste0(rep(1:10, each = 2),"_",samples_actual_n)
plot_nest_estimates_kfold(merrimajeel_kfold_estimates_gam, merrimajeel, 0.05)



