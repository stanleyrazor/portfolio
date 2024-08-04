set.seed(123)

xmat <- cbind(rep(1, 1000), matrix(runif(5000, min = .2, max = .7), nrow = 1000))
truepars <- c(.1, .2, .3, .9, .9, 3)
eta <- xmat %*% truepars
y <- rexp(1000, rate = exp(-eta))

nll_exp <- function(par) {
    par <- matrix(par, ncol = 1)
    yhat <- (xmat %*% par)^(-1)
    ll <- (y * yhat) - log(yhat, base = exp(1))
    sum(ll)
}

nll_exp2 <- function(par) {
    par <- matrix(par, ncol = 1)
    yhat <- exp(xmat %*% par)
    nll <- dexp(x = y, rate = yhat, log = T)
    -sum(nll)
}

(ovals <- optim(
    par = rep(1, 6),
    fn = nll_exp,
    method = "Nelder-Mead",
    control = list(maxit = 5e3)
))

(ovals$par) |> round(1)



inv <- \(x) 1 / x

# Defining the negative log-likelihood function for exponential regression
nll_exp <- function(par) {
    par <- matrix(par, ncol = 1)
    yhat <- inv(xmat %*% par)
    sum((y * lambda) - inv(lambda))
}

# Optimizing the parameters
ovals <- optim(
    par = rep(0, 6),
    fn = nll_exp,
    method = "Nelder-Mead",
    control = list(maxit = 5e3)
)

ovals
