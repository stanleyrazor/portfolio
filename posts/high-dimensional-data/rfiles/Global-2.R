
# Global files, functions and data ----------------------------------------

# polynomial regression
polyreg <- function(x, y, order)
{
  x <- as.matrix(x)
  xx <- matrix(NA, nrow = nrow(x), ncol = order)
  for (i in 1:ncol(xx))
  {
    xx[, i] <- x**i
  }
  colnames(xx) <- str_c("col_", 1:ncol(xx))
  md <- lm(y ~ xx)
  coef_mat <- as.matrix(md$coefficients)
  return(coef_mat)
}

# plotting fn
plot_poly <- function(model, xval, c)
{
  yhat <- vector()
  s <- 0
  xval <- seq(min(xval),max(xval),len=50)
  for (i in 1:length(model))
  {
    s <- s + (model[i] * (xval**(i-1)))
  }
  points(xval, s, type="l", col=c, lwd=2)
  
  return(s)
}

# Predicting function
pred_poly <- function(model, xval)
{
  s <- 0
  
  for (i in 1:length(model))
  {
    s <- s + (model[i] * (xval**(i-1)))
  }
  return(s)
}

# ridge regression
ridge_reg <- function(x, y, eta, lambda_start)
{
  stopifnot(nrow(x) == nrow(y))
  require(ModelMetrics)
  
  # standardize <- function(x) (x - mean(x)) / sd(x)
  # x <- apply(x, 2, standardize)
  
  # initializing storage and parameters
  train_error <- l_vec <- lambda <- ratio_l <- vector()
  lambda[1] <- lambda_start
  l_vec[1] <- exp(lambda)
  i = 1
  order <- ncol(x)
  xmat <- cbind(
    Intercept = rep(1, nrow(x)),
    x
  )
  
  while (l_vec[i] != 0)
  {
    # calculation of coefficients by minimizing SS-error
    mod <- glmnet(x = x, y = y, alpha = 0, family = "gaussian", lambda = l_vec[i])
    bvec <- (as.numeric(coef(mod)))   #original bvec = solve(exp(lambda[i]) * diag(order+1) + t(xmat) %*% xmat) %*% t(xmat) %*% y
    
    # prediction
    pred <- predict(mod, newx = x) |> as.numeric()
    
    # error(MSE)
    train_error[i] <- mse(y, pred) + (exp(lambda[i])*(sum(bvec[-1]**2)))
    # print(paste("MSE IS: ", train_error[i]))
    
    # gradient of error wrt to lambda
    d_E <- sum(bvec[-1]**2)
    lambda[i+1] <- lambda[i] - (eta * d_E)
    print(paste("Regularization parameter estimate(lambda) is: ", (lambda[i+1])))
    l_vec <- c(l_vec, exp(lambda[i+1]))
    
    # calculating the kappa(ratio), of successive lambda's
    ratio_l <- c(ratio_l, l_vec[i-1]/l_vec[i])
    if (i>2 && ratio_l[i-1] > 2*ratio_l[i-2])
    {
      print(paste("Most suitable regularization parameter(pre-break): ", l_vec[i-1]))
      print(paste("Most suitable regularization parameter(post-break): ", l_vec[i]))
      message("Convergence reached at lambda index: ", i-1)
      
      print("*********************************************************")
      print(paste("Consider using regurlarizer as: ", tail(l_vec, 2)))
      print("Coefficient estimates are: ")
      print(bvec)
      print("*********************************************************")
      
      return(
        list(
          ratio_l = ratio_l,
          l_vec = l_vec,
          lambdas = lambda,
          bvec = bvec
          
        )
      )
    }
    i=i+1
  }
  
}

# lasso regression
lasso_reg <- function(x, y, eta, lambda_start)
{
  stopifnot(nrow(x) == nrow(y))
  require(ModelMetrics)
  
  # initializing storage and parameters
  train_error <- l_vec <- lambda <- ratio_l <- vector()
  lambda[1] <- lambda_start
  l_vec[1] <- exp(lambda)
  i = 1
  order <- ncol(x)
  xmat <- cbind(
    Intercept = rep(1, nrow(x)),
    x
  )
  while (l_vec[i] != 0)
  {
    # calculation of coefficients by minimizing SS-error
    mod <- glmnet(x = x, y = y, alpha = 1, family = "gaussian", lambda = l_vec[i])
    bvec <- (as.numeric(coef(mod)))   #original bvec = solve(exp(lambda[i]) * diag(order+1) + t(xmat) %*% xmat) %*% t(xmat) %*% y
    
    # prediction
    pred <- predict(mod, newx = x) |> as.numeric()
    
    # error(MSE)
    train_error[i] <- mse(y, pred) + (exp(lambda[i])*(sum(bvec[-1]**2)))
    # print(paste("MSE IS: ", train_error[i]))
    
    # gradient of error wrt to lambda
    d_E <- sum(bvec[-1]**2)
    lambda[i+1] <- lambda[i] - (eta * d_E)
    print(paste("Regularization parameter estimate(lambda) is: ", (lambda[i+1])))
    l_vec <- c(l_vec, exp(lambda[i+1]))
    
    # calculating the kappa(ratio), of successive lambda's
    ratio_l <- c(ratio_l, l_vec[i-1]/l_vec[i])
    if (i>2 && ratio_l[i-1] > 2*ratio_l[i-2])
    {
      print(paste("Most suitable regularization parameter(pre-break): ", l_vec[i-1]))
      print(paste("Most suitable regularization parameter(post-break): ", l_vec[i]))
      message("Convergence reached at lambda index: ", i-1)
      
      print("*********************************************************")
      print(paste("Consider using regurlarizer as: ", tail(l_vec, 2)))
      print("Coefficient estimates are: ")
      print(bvec)
      print("*********************************************************")
      
      return(
        list(
          ratio_l = ratio_l,
          l_vec = l_vec,
          lambdas = lambda,
          bvec = bvec
          
        )
      )
    }
    i=i+1
  }
  
  
}


# sinusoidal dataset
# x <- seq(0,1, length = 10)
# set.seed(226)
# y <- sin(2*pi*x) + rnorm(n = 10, mean = 0, sd = 1)
# plot(x, y, type="p", pch=20, ylim=c(-5, 7),
#      xlab = "Predictor", ylab = "Response")
# pmd = polyreg(x, y, order=9)
# yhat <- plot_poly(pmd, x, c = "red")

