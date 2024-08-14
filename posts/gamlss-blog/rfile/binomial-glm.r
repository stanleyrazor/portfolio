set.seed(243)
xmat <- cbind(matrix(runif(1000, min = .2, max = .7), nrow = 200))
xmat = cbind(rep(1, 200), xmat)
truepars <- matrix(c(.1, .2, .3, .9, .9, 3), ncol = 1)
probs <- plogis(xmat %*% truepars)

n <- sample(100:200, 200, replace = T)
yvec <- cbind(n, rbinom(n = 200, size = n, prob = probs)) # matrix(n, y)

# using the derived log likelihood
nll_binom <- function(par) {
    par <- matrix(par, ncol = 1)
    p <- plogis(xmat %*% par) # Using the logistic function to get probabilities

    # Calculating negative log-likelihood
    nll <- -sum(yvec[, 2] * log(p) + (yvec[, 1] - yvec[, 2]) * log(1 - p))
    return(nll)
}

# using the dbinom one
nll_binom2 <- function(par) {
    par <- matrix(par, ncol = 1)
    p <- plogis(xmat %*% par)
    m <- nrow(xmat)

    ll <- dbinom(x = yvec[, 2], size = yvec[, 1], prob = p, log = T)
    -sum(ll)
}


optim(
    par = rep(1, 6),
    fn = nll_binom,
    method = "BFGS",
    control = list(maxit = 1e3)
)

dd <- data.frame(y = yvec[, 2], n = yvec[, 1], xmat)
glm(cbind(y, n - y) ~ . - 1, family = binomial(), data = dd)







# the user derived function evaluation runs twice faster
res <- microbenchmark::microbenchmark(derived = optim(
    par = rep(1, 6),
    fn = nll_binom,
    method = "BFGS",
    control = list(maxit = 1e3)
), dval = optim(
    par = rep(1, 6),
    fn = nll_binom2,
    method = "BFGS",
    control = list(maxit = 1e3)
), times = 500, control = list(warmup = 100))


### loading a dataset to do computations
dd <- read.csv("~/Documents/GitHub/KDHS Model Updates/may latest work/data/stratum_aggregates_2014.csv")

xmat <- dd |>
    select(mal_prev, bord, pop_dens, within_hr1yes) |>
    mutate(int = rep(1, nrow(dd))) |>
    select(int, everything()) |>
    as.matrix()
yvec <- cbind(dd$neonate_total_trials, dd$neonate_success)

(oval <- optim(
    par = coef(g1) |> as.numeric(),
    fn = nll_binom,
    method = "Nelder-Mead",
    control = list(maxit = 1e3)
))

g1 <- glm(cbind(neonate_success, neonate_total_trials - neonate_success) ~
    mal_prev + bord + pop_dens + within_hr1yes, family = binomial(), data = dd)
coef(g1)
