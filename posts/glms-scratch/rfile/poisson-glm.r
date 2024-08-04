# Generating the data
set.seed(123)

xmat <- cbind(matrix(runif(1000, min = .2, max = .7), nrow = 200))
xmat = cbind(rep(1, 200), xmat)
truepars <- matrix(c(.1, .2, .3, .9, .9, 3), ncol = 1)
y <- exp(xmat %*% truepars) |> round(0)


# gradient descent route
dldb <- function(par) {
    initpars <- par |> matrix()
    ((t(xmat) %*% exp(xmat %*% initpars)) - (t(xmat) %*% y))
}

initpars <- rep(0, 6)
eta <- .00001
for (i in 1:1e4) {
    derr <- dldb(initpars)
    initpars <- initpars - eta * derr
    print(round(t(initpars), 3))
}

# the dpois route

# the optim route
nll2 <- function(par) {
    initpars = par |> matrix()
    yhat <- exp(xmat %*% initpars)
    -sum(y * log(yhat) - yhat)
}

# simplification to nll2 above using the d argument
nll3 <- function(par) {
    initpars <- matrix(par, ncol = 1)
    lambda <- exp(xmat %*% initpars)
    -sum(dpois(y, lambda, log = TRUE))
}

optim(
    par = matrix(rep(1, 6)),
    fn = nll3,
    method = "Nelder-Mead",
    control = list(maxit = 1e5)
)




