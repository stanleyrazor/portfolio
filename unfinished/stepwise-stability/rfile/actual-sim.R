

# Libs + dir --------------------------------------------------------------

library(pacman)
p_load(dplyr, ggplot2, purrr, MASS, parallel, stringi, stringr)

# Functions ---------------------------------------------------------------

# the function for computing stability with its ci
getStability <- function(X,alpha=0.05) {
  ## the input X is a binary matrix of size M*d where:
  ## M is the number of bootstrap replicates
  ## d is the total number of features
  ## alpha is the level of significance (e.g. if alpha=0.05, we will get 95% confidence intervals)
  ## it's an optional argument and is set to 5% by default
  ### first we compute the stability
  
  M<-nrow(X)
  d<-ncol(X)
  hatPF<-colMeans(X)
  kbar<-sum(hatPF)
  v_rand=(kbar/d)*(1-kbar/d)
  stability<-1-(M/(M-1))*mean(hatPF*(1-hatPF))/v_rand ## this is the stability estimate
  
  ## then we compute the variance of the estimate
  ki<-rowSums(X)
  phi_i<-rep(0,M)
  for(i in 1:M){ 
    phi_i[i]<-(1/v_rand)*((1/d)*sum(X[i,]*hatPF)-(ki[i]*kbar)/d^2-(stability/2)*((2*kbar*ki[i])/d^2-ki[i]/d-kbar/d+1))
  }
  phi_bar=mean(phi_i)
  var_stab=(4/M^2)*sum((phi_i-phi_bar)^2) ## this is the variance of the stability estimate
  
  ## then we calculate lower and upper limits of the confidence intervals
  z<-qnorm(1-alpha/2) # this is the standard normal cumulative inverse at a level 1-alpha/2
  upper<-stability+z*sqrt(var_stab) ## the upper bound of the (1-alpha) confidence interval
  lower<-stability-z*sqrt(var_stab) ## the lower bound of the (1-alpha) confidence interval
  
  return(list("stability"=stability,"variance"=var_stab,"lower"=lower,"upper"=upper))
  
}

# the function for getting median, 95% quantile CI from a vector
summary_stat <- function(vec) {
  qts <- quantile(vec, c(0.025, .5, 0.975)) |> as.numeric()
  list(ci_low = qts[1], median = qts[2], ci_high = qts[3])
}


# Data --------------------------------------------------------------------

simulation_grid <- expand.grid(
  sample_size = 2**(0:5) * 100,
  noise_variance = 1:5,
  true_vars = 10,
  noise_vars_ratio = 1:5,
  correlation_strength = (1:9)/10,
  
  # model estimates
  # full_model_aic = NA, reduced_model_aic = NA,
  fdr_est = NA, fdr.ci_low = NA, fdr.ci_high = NA,
  stability_est = NA, stability.ci_low = NA, stability.ci_high = NA
)

boot_num <- 100
pb <- txtProgressBar(min = 1, max = boot_num, style = 3)

for (i in 1:nrow(simulation_grid)) {
  
  n <- simulation_grid[i, "sample_size"]
  p <- simulation_grid[i, "true_vars"] + (simulation_grid[i, "true_vars"] * simulation_grid[i, "noise_vars_ratio"])
  p_true <- simulation_grid[i, "true_vars"]
  rho <- simulation_grid[i, "correlation_strength"]
  noise_variance_y <- simulation_grid[i, "noise_variance"]
  p_noise <- p - true
  
  # printing the progress:
  message(
    "\nScenario [ ", i, " ] " ,
    " Sample size: ", n, " | ",
    " Variables: ", p, " | ",
    " Rho: ", rho, " | ",
    " Noise variance: ", noise_variance_y, " | "
  )

  set.seed(7)
  f <- rnorm(n, mean = 0, sd = 1)
  y <- f + rnorm(n, mean = 0, sd = noise_variance_y)
  xt <- (data.frame(sqrt(1 - rho) * matrix(rnorm(n * p_true), ncol = p_true) + sqrt(rho) * f)) |>
    setNames(paste0('true_', 1:p_true))
  xn <- (data.frame(matrix(rnorm(n * p_noise), ncol = p_noise))) |>
    setNames(paste0('noise_', 1:p_noise))
  
  d1 <- cbind(xt, xn, y) #[, sample(p_true + p_noise + 1)]
  
  bt_index <- 1:boot_num |>
    map(\(x) sample(n, n, replace = T))
  
  var_choice <- list()
  # fm_aic <- sm_aic <- vector()
  # fm_adj_rsq <- sm_adj_rsq <- vector()
  
  for (b in 1:boot_num) {
    setTxtProgressBar(pb, b)

    temp_data <- d1[bt_index[[b]], ]
    
    fm <- lm(y ~ ., data = temp_data)
    sm <- MASS::stepAIC(fm, direction = "both", trace = 0)
    
    var_choice[[b]] <- (coef(sm) |> names())[-1]
    
    # fm_aic[i] <- AIC(fm)
    # sm_aic[i] <- AIC(sm)
    # fm_adj_rsq[i] <- summary(fm)$adj.r.squared
    # sm_adj_rsq[i] <- summary(sm)$adj.r.squared
    # 
    # rm(fm, sm, temp_data)
  }
  
  # getting the bootstrap matrix
  all_variables <- colnames(d1)[colnames(d1) != 'y']
  boot_matrix <- t(sapply(var_choice, function(vars) all_variables %in% vars)) |>
    as.data.frame() |>
    mutate(across(everything(), as.integer))
  colnames(boot_matrix) <- all_variables
  
  boot_matrix |> apply(MARGIN = 2, FUN = mean)
  stab_est <- getStability(boot_matrix)
  
  # computing the FDR
  fdr_vec <- var_choice |>
    lapply(FUN = \(x) str_starts(x, 'noise_') |> mean()) |>
    unlist()
  
  simulation_grid[i, "stability.ci_low"] <- stab_est$lower
  simulation_grid[i, "stability_est"] <- stab_est$stability
  simulation_grid[i, "stability.ci_high"] <- stab_est$upper
  
  simulation_grid[i, "fdr.ci_low"] <- quantile(fdr_vec, probs = .025) |> as.numeric()
  simulation_grid[i, "fdr_est"] <- quantile(fdr_vec, probs = .5) |> as.numeric()
  simulation_grid[i, "fdr.ci_high"] <- quantile(fdr_vec, probs = .975) |> as.numeric()
  
}





