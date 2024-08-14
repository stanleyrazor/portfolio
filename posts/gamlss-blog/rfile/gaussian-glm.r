# Generating the data
set.seed(123)

xmat <- cbind(matrix(runif(1000, min = .2, max = .7), nrow = 200))
xmat = cbind(rep(1, 200), xmat)
truepars <- matrix(c(.1, .2, .3, .9, .9, 3), ncol = 1)
y <- (xmat %*% truepars) + rnorm(200, sd = .2)

# using manually derived log likelihoods
nll_gaussian <- function(par) {
    par <- matrix(par, ncol = 1)
    yhat <- xmat %*% par
    sum((y - yhat)**2)
}

optim(
    par = rep(0, 6),
    fn = nll_gaussian,
    method = "BFGS"
)

# using the dnorm approach
nll_dnorm <- function(par) {
    par <- matrix(par, ncol = 1)
    yhat <- xmat %*% par
    ll <- dnorm(x = y, mean = yhat, sd = 1, log = T)
    -sum(ll)
}

optim(
    par = rep(0, 6),
    fn = nll_dnorm,
    method = "BFGS"
)
