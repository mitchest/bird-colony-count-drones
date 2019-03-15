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

get_interval <- function(true, count, z = 1.96) {
  error <- abs(sum(true) - sum(count)) / sum(true)
  print(paste0("Count error was: ", error))
  z * sqrt( (error * (1 - error)) / length(true))
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
  invisible(plt)
}

fit_kfold_area <- function(x, kfold_data, data) { ############### wait, do we want to predict out to all original data, or just the data we don't have coutns for?
  kdat <- filter(kfold_data, fold != x)
  sum(data$nest_area) / (sum(kdat$nest_area) / sum(kdat$nest_count))
}

fit_kfold_area_grids <- function(x, kfold_data, data) { 
  kdat <- filter(kfold_data, fold != x)
  data.frame(manual_counts = data$nest_count,
             area_estimates = data$nest_area / (kdat$nest_area / kdat$nest_count))
}

fit_kfold_glm <- function(x, kfold_data, data) { ############### wait, do we want to predict out to all original data, or just the data we don't have coutns for?
  kdat <- filter(kfold_data, fold != x)
  nest_glm <- glm(nest_count ~ nest_area + crowd_ratio, family = "poisson", data = kdat)
  sum(predict(nest_glm, newdata = data.frame(nest_area = data$nest_area, crowd_ratio = data$crowd_ratio), type = "response"))
}

fit_kfold_glm_grids <- function(x, kfold_data, data) { ############### wait, do we want to predict out to all original data, or just the data we don't have coutns for?
  kdat <- filter(kfold_data, fold != x)
  nest_glm <- glm(nest_count ~ nest_area + crowd_ratio, family = "poisson", data = kdat)
  data.frame(manual_counts = data$nest_count,
             area_estimates = predict(nest_glm, newdata = data.frame(nest_area = data$nest_area, crowd_ratio = data$crowd_ratio), type = "response"))
}

fit_kfold_gam <- function(x, kfold_data, data) {
  kdat <- filter(kfold_data, fold != x)
  nest_gam <- gam(nest_count ~ s(nest_area) + s(crowd_ratio), family = nb(), data = kdat, method = "REML")
  sum(predict(nest_gam, newdata = data.frame(nest_area = data$nest_area, crowd_ratio = data$crowd_ratio), type = "response"))
}

estimate_nests_kfold <- function(samples, data, iters, k, model = NULL, quants = c(0.1,0.9)) {
  if (!model %in% c('glm','gam','area')) {stop("specify 'glm, 'gam' or 'area'")}
  kfold_data <- data %>%
    # stratify by crowd bin
    filter(crowd_ratio > quantile(crowd_ratio, quants[1]), crowd_ratio < quantile(crowd_ratio, quants[2])) %>%
    sample_frac(1) %>%
    group_by(crowd_bin) %>%
    mutate(crowd_id = 1:n()) %>%
    arrange(crowd_id) %>%
    .[1:samples,] %>%
    ungroup()
    
    # # SRS
    # sample_n(samples)
    
    # # Sample quantile of crowd ratios
    # filter(crowd_ratio > quantile(crowd_ratio, 0.10), crowd_ratio < quantile(crowd_ratio, 0.80)) %>%
    # sample_n(samples)
  
  if (model == 'glm') {
    return(
      as.vector(
        replicate(n = iters,
                  expr = {
                    kfold_data <- kfold_data %>%
                      #group_by(crowd_bin) %>% # stratify sample
                      mutate(fold = sample(rep(sample(k), length.out = n())))
                    unlist(lapply(X = 1:k, FUN = fit_kfold_glm, kfold_data, data))
                  })))
  }
  if (model == 'gam') {
    return(
      as.vector(
        replicate(n = iters,
                  expr = {
                    kfold_data <- kfold_data %>%
                      #group_by(crowd_bin) %>% # stratify sample
                      mutate(fold = sample(rep(sample(k), length.out = n())))
                    unlist(lapply(X = 1:k, FUN = fit_kfold_gam, kfold_data, data))
                  })))
  }
  if (model == 'area') {
    return(
      as.vector(
        replicate(n = iters,
                  expr = {
                    kfold_data <- kfold_data %>%
                      #group_by(crowd_bin) %>% # stratify sample
                      mutate(fold = sample(rep(sample(k), length.out = n())))
                    unlist(lapply(X = 1:k, FUN = fit_kfold_area, kfold_data, data))
                  })))
  }
}

estimate_nests_kfold_grids <- function(samples, data, iters, title, k = 10, quants = c(0.1,0.9)) {
  kfold_data <- data %>%
    # stratify by crowd bin
    filter(crowd_ratio > quantile(crowd_ratio, quants[1]), crowd_ratio < quantile(crowd_ratio, quants[2])) %>%
    sample_frac(1) %>%
    group_by(crowd_bin) %>%
    mutate(crowd_id = 1:n()) %>%
    arrange(crowd_id) %>%
    .[1:samples,] %>%
    ungroup()
  grid_est <- bind_rows(replicate(n = iters,
                                  expr = {
                                    kfold_data <- kfold_data %>%
                                    #group_by(crowd_bin) %>% # stratify sample
                                    mutate(fold = sample(rep(sample(k), length.out = n())))
                                    bind_rows(lapply(X = 1:k, FUN = fit_kfold_area_grids, kfold_data, data))},
                                  simplify = F))
  plt <- ggplot(grid_est, aes(manual_counts, area_estimates)) +
    geom_point() +
    ggtitle(title) + ylab("Manual grid count") + xlab("Semi-automated grid count") +
    geom_abline(slope = 1, intercept = 0) +
    theme_classic()
  plt
}


plot_nest_estimates_kfold <- function(kfold_estimates, data, count_error, title) {
  tot_nests <- sum(data$nest_count)
  nests_lwr <- tot_nests - tot_nests * count_error
  nests_upr <- tot_nests + tot_nests * count_error
  #plot data
  kfold_estimates_long <- kfold_estimates %>%
    as.data.frame(col.names = 1:length(kfold_estimates)) %>%
    gather() %>%
    # mutate(samples = as.numeric(unlist(lapply(key, function(x) strsplit(x, "_")[[1]][2]))),
    #        key = factor(key), samples = factor(samples))
    mutate(key = factor(key)) %>%
    group_by(key) %>%
    summarise(mean = mean(value),
              upr = quantile(value, 0.95),
              lwr = quantile(value, 0.05)) %>%
    ungroup() %>%
    mutate(upr2 = upr - mean, lwr2 = mean - lwr)
  #print(kfold_estimates_long)
  # plot
  options(scipen = 100000)
  plt <- ggplot(kfold_estimates_long, aes(x = key)) + 
    #geom_boxplot(aes(x = key), outlier.shape = NA) +
    geom_errorbar(aes(ymin = mean - lwr2, ymax = mean + upr2), width = 0, colour = "grey") +
    geom_point(aes(y = mean), size = 2) +
    scale_y_continuous(limits = c(min(kfold_estimates_long$lwr),max(kfold_estimates_long$upr))) +
    geom_hline(yintercept = c(tot_nests, nests_lwr, nests_upr), colour = 2) +
    #facet_wrap(~samples) +
    xlab("") +#xlab("Different random draws of training data") + 
    ylab("Estimated nest count") + 
    ggtitle(title) + 
    theme_classic() + theme(axis.text.x=element_blank())
  print(plt)
}

crunch_it <- function(run_iters = F, save = F, # whether to re-run analysis/save data or just load and plot
                      colony_name, colony_data, # name of the colony (string) and loaded colony data
                      nboot = 1000, # number of bootstrap iterations for nest count estimates
                      smaller_sample = 5, num_draws = 3, # number of grids to sample, repeated num_draws times ##### XXX ##### number of samples to take from each crowd ratio bin (from data load), repeated num_draws times
                      quants = c(0.1,0.9), # central quantile to sample from (trim extreme grids you wouldn't choose to count) 
                      model_type = NULL, # choose 'area', 'glm' or 'gam'
                      kfold_iters = 50, kfold_k = 10, # kfold iters and k for simulation
                      error_level = 0.05,
                      plot_only = F,
                      title = "Resampled estimates"){ # manual counting error level to compare on plots
  if (run_iters) {
    ## straight up area ratio
    colony_estimates0 <- bind_cols(lapply(1:nrow(colony_data), estimate_nest_counts, nboot, colony_data))
    colony_estimates <- gather(colony_estimates0)
    colony_estimates$samples <- as.numeric(unlist(lapply(colony_estimates$key, 
                                                              function(x) strsplit(x, "_")[[1]][2])))
    if (save) saveRDS(colony_estimates, file = paste0(colony_name,"_estimates.rds"))
    rm(colony_estimates0)
    ## model nest area and crowdedness
    colony_estimates_glm0 <- bind_cols(lapply(1:nrow(colony_data), estimate_nest_counts_glm, nboot, colony_data))
    colony_estimates_glm <- gather(colony_estimates_glm0)
    colony_estimates_glm$samples <- as.numeric(unlist(lapply(colony_estimates_glm$key, 
                                                                  function(x) strsplit(x, "_")[[1]][2])))
    if (save) saveRDS(colony_estimates_glm, file = paste0(colony_name,"_estimates_glm.rds"))
    rm(colony_estimates_glm0)
    ## test out resampling with smaller sampling size
    kfold_samples <- rep(smaller_sample, num_draws)
    samples_actual_n <- kfold_samples #* length(unique(colony_data$crowd_bin))
    # glm
    colony_kfold_estimates <- lapply(X = kfold_samples,
                                         FUN = estimate_nests_kfold, colony_data, kfold_iters, kfold_k, model_type, quants)
    attr(colony_kfold_estimates, "sampling") <- paste0("k-fold: ", samples_actual_n, " out of ", nrow(colony_data), " plots")
    #names(colony_kfold_estimates) <- paste0(kfold_samples,"_",samples_actual_n)
    if (save) saveRDS(colony_kfold_estimates, file = paste0(colony_name,"_kfold_estimates.rds"))
    #rm(colony_kfold_estimates)
    # # gam
    # colony_kfold_estimates_gam <- lapply(X = kfold_samples,
    #                                      FUN = estimate_nests_kfold, colony_data, kfold_iters, kfold_k, 'gam')
    # #names(colony_kfold_estimates_gam) <- paste0(kfold_samples,"_",samples_actual_n)
    # saveRDS(colony_kfold_estimates_gam, file = paste0(colony_name,"_kfold_estimates_gam.rds"))
    # rm(colony_kfold_estimates_gam)
  }
  
  if (!run_iters) {
    colony_estimates <- readRDS(paste0(colony_name,"_estimates.rds"))
    colony_estimates_glm <- readRDS(paste0(colony_name,"_estimates_glm.rds"))
    colony_kfold_estimates <- readRDS(paste0(colony_name,"_kfold_estimates.rds"))
    #colony_kfold_estimates_gam <- readRDS(paste0(colony_name,"_kfold_estimates_gam.rds"))
  }
  
  if (plot_only) {
    area_plt <- plot_nest_estimates(colony_estimates, colony_data, error_level, paste0(title, " - area only"))
    glm_plt <- plot_nest_estimates(colony_estimates_glm, colony_data, error_level, paste0(title, " - GLM"))
    return(list(area_plt,glm_plt))
  } else {
    plot_nest_estimates(colony_estimates, colony_data, error_level, title)
    plot_nest_estimates(colony_estimates_glm, colony_data, error_level, title)
    plot_nest_estimates_kfold(colony_kfold_estimates, colony_data, error_level, attr(colony_kfold_estimates, "sampling"))
    #plot_nest_estimates_kfold(colony_kfold_estimates_gam, colony_data, error_level, "GAM k-fold")
    invisible(plot_nest_estimates_kfold)
  }
}

calc_mapping_accuracy <- function(background, nests){
  # build background points
  back <- read.dbf(background, as.is = T)
  back_df <- data.frame(map = back$gridcode,
                        ref = 0)
  # build nest points
  nest <- read.dbf(nests, as.is = T)
  nest_df <- data.frame(map = nest$gridcode,
                        ref = 1)
  # combine
  cases <- as.data.frame(rbind(back_df, nest_df))
  # make confusion matrix and print out
  conf_mat <- table(cases$map, cases$ref)
  print(conf_mat)
  print(paste0("Overall: ", round(percentage_agreement(conf_mat),4)))
  print(paste0("Row: ", row_error(conf_mat)))
  print(paste0("Col: ", column_error(conf_mat)))
  bootstrap_oa_ci(cases)
}

percentage_agreement <- function(conf_mat) {
  sum(diag(conf_mat)) / sum(conf_mat) # xtab method quicker?
}

row_error <- function(conf_mat) {
  round(1 - (diag(conf_mat) / apply(conf_mat, 1, sum)),4)
}

column_error <- function(conf_mat) {
  round(1 - (diag(conf_mat) / apply(conf_mat, 2, sum)),4)
}

bootstrap_oa_ci <- function(cases) {
  samp_dist <- replicate(n = 800, expr = {
    cases <- sample_frac(cases, 1, TRUE)
    conf_mat <- table(cases$map, cases$ref)
    percentage_agreement(conf_mat)
    }, simplify = T)
  quants <- round(quantile(samp_dist, c(0.025, 0.5, 0.975)),4)
  print(paste0("Bootstrap: ", quants[2], " (", quants[1], "-", quants[3], ")"))
}


