# functions ---------------------------------------------------------------

read_fishnet_data <- function(path, area_thresh = 2400, crowd_thresh = 200, crown_bins = 8) {
  read.csv(path, stringsAsFactors = F) %>%
    na.omit() %>%
    filter(net_area > area_thresh) %>%
    mutate(nest_area_ratio = nest_area / nest_count,
           crowd_ratio = net_area / nest_area) %>%
    filter(crowd_ratio < crowd_thresh) %>%
    mutate(crowd_bin = ntile(crowd_ratio, crown_bins),
           Id = 1:n())
}

estimate_nest <- function(samples, data) {
  Ids <- sample(x = data$Id, size = samples)
  train_data <- filter(data, Id %in% Ids)
  sum(data$nest_area) / (sum(train_data$nest_area) / sum(train_data$nest_count))
}

estimate_nest_glm <- function(samples, data) {
  Ids <- sample(x = data$Id, size = samples)
  train_data <- filter(data, Id %in% Ids)
  nest_glm <- glm(nest_count ~ nest_area + crowd_ratio, family = "poisson", data = train_data)
  sum(predict(nest_glm, newdata = data.frame(nest_area = data$nest_area, crowd_ratio = data$crowd_ratio), type = "response"))
}

estimate_nest_counts <- function(samples, iters, data) {
  df_out <- data.frame(
    unlist(replicate(n = iters,
                     expr = estimate_nest(samples, data),
                     simplify = F))
  )
  names(df_out) <- paste0("counts_",samples)
  df_out
}

estimate_nest_counts_glm <- function(samples, iters, data) {
  df_out <- data.frame(
    unlist(replicate(n = iters,
                     expr = estimate_nest_glm(samples, data),
                     simplify = F))
  )
  names(df_out) <- paste0("counts_",samples)
  df_out
}

plot_nest_estimates <- function(estimates_long, data, count_error, title) {
  tot_nests <- sum(data$nest_count)
  nests_lwr <- tot_nests - tot_nests * count_error
  nests_upr <- tot_nests + tot_nests * count_error
  plt <- ggplot(estimates_long, aes(y = value, x = samples)) + 
    geom_boxplot(aes(group = samples), outlier.shape = NA) +
    scale_y_continuous(limits = c(tot_nests-tot_nests*0.2, tot_nests+tot_nests*0.2)) +
    geom_hline(yintercept = c(tot_nests, nests_lwr, nests_upr), colour = 2) +
    xlab("number random transects used") + ylab("estimated nest count") + 
    ggtitle(title) + theme_bw()
  print(plt)
}

fit_kfold_glm <- function(x, kfold_data, data) { ############### wait, don't predict out to all original data - just the data we don't have coutns for!
  kdat <- filter(kfold_data, fold != x)
  nest_glm <- glm.nb(nest_count ~ nest_area + crowd_ratio, data = kdat)
  sum(predict(nest_glm, newdata = data.frame(nest_area = data$nest_area, crowd_ratio = data$crowd_ratio), type = "response"))
}

fit_kfold_gam <- function(x, kfold_data, data) {
  kdat <- filter(kfold_data, fold != x)
  nest_gam <- gam(nest_count ~ s(nest_area) + s(crowd_ratio), family = nb(), data = kdat, method = "REML")
  sum(predict(nest_gam, newdata = data.frame(nest_area = data$nest_area, crowd_ratio = data$crowd_ratio), type = "response"))
}

estimate_nests_kfold <- function(samples, data, iters, k, model = NULL) {
  if (is.null(model)) {stop("specify 'glm or 'gam'")}
  kfold_data <- data %>%
    group_by(crowd_bin) %>% #stratify sample
    sample_n(samples)
  #sample(x = data$Id, size = samples) %>%
  #kfold_data <- filter(data, Id %in% Ids)
  kfold_data <- kfold_data %>%
    group_by(crowd_bin) %>% # stratify sample
    mutate(fold = sample(rep(sample(k), length.out = n())))
  #kfold_data$fold <- sample(rep(1:k, length.out = nrow(kfold_data)))
  if (model == 'glm') {
    return(as.vector(replicate(n = iters,
                        expr = {unlist(lapply(X = 1:k, FUN = fit_kfold_glm, kfold_data, data))})))
  }
  if (model == 'gam') {
    return(as.vector(replicate(n = iters,
                        expr = {unlist(lapply(X = 1:k, FUN = fit_kfold_gam, kfold_data, data))})))
  }
}

plot_nest_estimates_kfold <- function(kfold_estimates, data, count_error, title) {
  tot_nests <- sum(data$nest_count)
  nests_lwr <- tot_nests - tot_nests * count_error
  nests_upr <- tot_nests + tot_nests * count_error
  #plot data
  kfold_estimates_long <- kfold_estimates %>%
    as.data.frame() %>%
    gather() %>%
    # mutate(samples = as.numeric(unlist(lapply(key, function(x) strsplit(x, "_")[[1]][2]))),
    #        key = factor(key), samples = factor(samples))
    mutate(key = factor(key))
  # plot
  options(scipen = 100000)
  plt <- ggplot(kfold_estimates_long, aes(y = value)) + 
    geom_boxplot(aes(x = key), outlier.shape = NA) +
    scale_y_continuous(limits = quantile(kfold_estimates_long$value, c(0.01, 0.9))) +
    geom_hline(yintercept = c(tot_nests, nests_lwr, nests_upr), colour = 2) +
    #facet_wrap(~samples) +
    xlab("Different random draws of training data") + ylab("Estimated nest count") + 
    ggtitle(title) + 
    theme_bw() + theme(axis.text.x=element_blank())
  print(plt)
}

crunch_it <- function(run_iters = F, save = F, # whether to re-run analysis/save data or just load and plot
                      colony_name, colony_data, # name of the colony (string) and loaded colony data
                      nboot = 1000, # number of bootstrap iterations for nest count estimates
                      smaller_sample = 5, num_draws = 3, # number of samples to take from each crowd ratio bin (from data load), repeated num_draws times
                      kfold_iters = 50, kfold_k = 10, # kfold iters and k for simulation
                      error_level = 0.05){ # manual counting error level to compare on plots
  if (run_iters) {
    ## straight up area ratio
    colony_estimates <- bind_cols(lapply(1:nrow(colony_data), estimate_nest_counts, nboot, colony_data))
    colony_estimates_long <- gather(colony_estimates)
    colony_estimates_long$samples <- as.numeric(unlist(lapply(colony_estimates_long$key, 
                                                              function(x) strsplit(x, "_")[[1]][2])))
    if (save) saveRDS(colony_estimates_long, file = paste0(colony_name,"_estimates.rds"))
    rm(colony_estimates_long)
    ## model nest area and crowdedness
    colony_estimates_glm <- bind_cols(lapply(1:nrow(colony_data), estimate_nest_counts_glm, nboot, colony_data))
    colony_estimates_glm_long <- gather(colony_estimates_glm)
    colony_estimates_glm_long$samples <- as.numeric(unlist(lapply(colony_estimates_glm_long$key, 
                                                                  function(x) strsplit(x, "_")[[1]][2])))
    if (save) saveRDS(colony_estimates_glm_long, file = paste0(colony_name,"_estimates_glm.rds"))
    rm(colony_estimates_glm_long)
    ## test out resampling with smaller sampling size
    kfold_samples <- rep(smaller_sample, num_draws)
    samples_actual_n <- kfold_samples * length(unique(colony_data$crowd_bin))
    # glm
    colony_kfold_estimates_glm <- lapply(X = kfold_samples,
                                         FUN = estimate_nests_kfold, colony_data, kfold_iters, kfold_k, 'glm')
    attr(colony_kfold_estimates_glm, "sampling") <- paste0("k-fold: ", samples_actual_n, " out of ", nrow(colony_data), " plots")
    #names(colony_kfold_estimates_glm) <- paste0(kfold_samples,"_",samples_actual_n)
    if (save) saveRDS(colony_kfold_estimates_glm, file = paste0(colony_name,"_kfold_estimates_glm.rds"))
    rm(colony_kfold_estimates_glm)
    # # gam
    # colony_kfold_estimates_gam <- lapply(X = kfold_samples,
    #                                      FUN = estimate_nests_kfold, colony_data, kfold_iters, kfold_k, 'gam')
    # #names(colony_kfold_estimates_gam) <- paste0(kfold_samples,"_",samples_actual_n)
    # saveRDS(colony_kfold_estimates_gam, file = paste0(colony_name,"_kfold_estimates_gam.rds"))
    # rm(colony_kfold_estimates_gam)
  }
  colony_estimates <- readRDS(paste0(colony_name,"_estimates.rds"))
  colony_estimates_glm <- readRDS(paste0(colony_name,"_estimates_glm.rds"))
  colony_kfold_estimates_glm <- readRDS(paste0(colony_name,"_kfold_estimates_glm.rds"))
  #colony_kfold_estimates_gam <- readRDS(paste0(colony_name,"_kfold_estimates_gam.rds"))
  
  plot_nest_estimates(colony_estimates, colony_data, error_level, "Area only estimate")
  plot_nest_estimates(colony_estimates_glm, colony_data, error_level, "Area/crowd model")
  plot_nest_estimates_kfold(colony_kfold_estimates_glm, colony_data, error_level, attr(colony_kfold_estimates_glm, "sampling"))
  #plot_nest_estimates_kfold(colony_kfold_estimates_gam, colony_data, error_level, "GAM k-fold")
}