

# Libs + dir --------------------------------------------------------------

library(pacman)
p_load(dplyr, ggplot2, purrr, MASS, parallel, stringi, stringr,
       doParallel, foreach, gtools, glmnet)

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
  sample_size = 2**(0:9) * 100,
  noise_variance = 1:5,
  true_vars = 10,
  noise_vars_ratio = 1:5
  # correlation_strength = (1:9)/10,
  
  # model estimates
  # full_model_aic = NA, reduced_model_aic = NA,
  # fdr_est = NA, fdr.ci_low = NA, fdr.ci_high = NA,
  # stability_est = NA, stability.ci_low = NA, stability.ci_high = NA
)

boot_num <- 100

# -------------------------------------------------------------------------

cl <- makeCluster(8)
registerDoParallel(cl)

sim_res <- foreach(i = 1:nrow(simulation_grid), .combine = rbind,
                   .packages = c("purrr", "gtools", "MASS", "stringr",
                                 "dplyr", "glmnet"),
                   .verbose = T) %dopar% {
                     
                     # unpacking the parameters
                     n <- simulation_grid[i, "sample_size"]
                     p <- simulation_grid[i, "true_vars"] + (simulation_grid[i, "true_vars"] * simulation_grid[i, "noise_vars_ratio"])
                     p_true <- simulation_grid[i, "true_vars"]
                     rho <- simulation_grid[i, "correlation_strength"]
                     noise_variance_y <- simulation_grid[i, "noise_variance"]
                     p_noise <- p - p_true
                     
                     # generating the dataset
                     set.seed(7)
                     selector <- c(rep(0, p_noise), rep(1, p_true))[sample(p)]
                     wts <- matrix(rnorm(p) * selector, nrow = p)
                     xmat <- matrix(rnorm(p*n), nrow = n, ncol = p)
                     y <- rnorm(n, mean = (xmat %*% wts), sd = noise_variance_y)
                     
                     d1 <- data.frame(xmat, y)
                     colnames(d1) <- c(paste0(ifelse(wts[, 1] == 0, "noise_", "true_"), 1:p), "y")
                     
                     # generating bayesian bootstrap weights
                     bt_wts <- rdirichlet(n = boot_num, alpha = rep(1, n)) |> t()
                     
                     var_choice <- 1:ncol(bt_wts) |>
                       map(\(iter) {
                         cn <- colnames(d1 |> dplyr::select(-y))
                         fm <- cv.glmnet(x = d1 |> dplyr::select(-y) |> as.matrix(),
                                         y = d1 |> pull(y),
                                         weights = bt_wts[, iter],
                                         type.measure = "deviance", 
                                         nfolds = 10,
                                         alpha = 1,
                                         intercept = T,
                                         standardize = T)
                         cfs <- (coef(fm, s = "lambda.1se") |> as.numeric())[-1]
                         cn[which(cfs != 0)]
                       })
                     
                     # Getting the bootstrap matrix
                     all_variables <- colnames(d1)[colnames(d1) != 'y']
                     boot_matrix <- t(sapply(var_choice, function(vars) all_variables %in% vars)) |>
                       as.data.frame() |>
                       mutate(across(everything(), as.integer))
                     colnames(boot_matrix) <- all_variables
                     
                     stab_est <- getStability(boot_matrix)
                     
                     # Computing the FDR
                     fdr_vec <- var_choice |>
                       lapply(FUN = \(x) str_starts(x, 'noise_') |> mean()) |>
                       unlist()
                     
                     # guarding against: LASSO kicking everything out thus FDR = NULL
                     fdr_vec <- na.omit(fdr_vec)
                     
                     # Returning the results for the grid
                     {
                       list(
                         fdr.ci_low = quantile(fdr_vec, probs = .025) |> as.numeric(),
                         fdr_est = quantile(fdr_vec, probs = .5) |> as.numeric(),
                         fdr.ci_high = quantile(fdr_vec, probs = .975) |> as.numeric(),
                         stability.ci_low = stab_est$lower,
                         stability_est = stab_est$stability,
                         stability.ci_high = stab_est$upper)
                       }
                   }

beepr::beep(2)
stopCluster(cl)

sim_res <- sim_res |> apply(MARGIN = 2, unlist)

simulation_grid <- cbind(simulation_grid, sim_res)
saveRDS(simulation_grid, "data/longer_lasso_simulation_grid.rds")

# -------------------------------------------------------------------------

# Now `simulation_grid` contains the results from the parallelized process
