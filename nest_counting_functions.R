# functions ---------------------------------------------------------------

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

plot_nest_estimates <- function(estimates_long, data, count_error) {
  tot_nests <- sum(data$nest_count)
  nests_lwr <- tot_nests - tot_nests * count_error
  nests_upr <- tot_nests + tot_nests * count_error
  plt <- ggplot(estimates_long, aes(y = value, x = samples)) + 
    geom_boxplot(aes(group = samples), outlier.shape = NA) +
    scale_y_continuous(limits = quantile(estimates_long$value, c(0.01, 0.99))) +
    geom_hline(yintercept = c(tot_nests, nests_lwr, nests_upr), colour = 2) +
    xlab("number random transects used") + ylab("estimated nest count") + theme_bw()
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

plot_nest_estimates_kfold <- function(kfold_estimates, data, count_error) {
  tot_nests <- sum(data$nest_count)
  nests_lwr <- tot_nests - tot_nests * count_error
  nests_upr <- tot_nests + tot_nests * count_error
  #plot data
  kfold_estimates_long <- kfold_estimates %>%
    as.data.frame() %>%
    gather() %>%
    mutate(samples = as.numeric(unlist(lapply(key, function(x) strsplit(x, "_")[[1]][2]))),
           key = factor(key), samples = factor(samples))
  # plot
  options(scipen=100000)
  plt <- ggplot(kfold_estimates_long, aes(y = value)) + 
    geom_boxplot(aes(x = key), outlier.shape = NA) +
    scale_y_continuous(limits = quantile(kfold_estimates_long$value, c(0.01, 0.9))) +
    geom_hline(yintercept = c(tot_nests, nests_lwr, nests_upr), colour = 2) +
    facet_wrap(~samples) +
    xlab("number random transects used") + ylab("estimated nest count") + theme_bw()
  print(plt)
}