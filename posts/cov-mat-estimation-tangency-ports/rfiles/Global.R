


# Some hyper-parameters ---------------------------------------------------

# tuning hyper-parameters:
# covt has nu, for degrees of freedome (3, 10), we estimate from data
# covOGK is changed to make it robust for center estimates
# covMcd, has alpha(0.5, 1) - analysis shows no need
# covBagged, has a tuning parameter baggedR - analysis shows 100 does fine
# covARW has tuning parameter alpha, although 0.025 does quite well
# covShrink has parameters, although there are analytical ways of finding it


# Libs --------------------------------------------------------------------

library(pacman)
p_load(tufte, dplyr, stringr, stringi, ggplot2, xts, ggpubr,
       PortfolioAnalytics, PerformanceAnalytics, fAssets,
       mvoutlier, plotly, kableExtra, lubridate, fPortfolio,
       robustbase, corpcor, MASS, corrplot, tidyr, purrr, quantmod)


# Functions ---------------------------------------------------------------

# loading data
asset_loader <- function(asset)
{
  pre_cursor <- 'data/assets'
  l <- str_c(pre_cursor, '/', asset, '.csv')
  
  for (i in 1:length(l))
  {
    temp <- read.csv(l[i]) |>
      dplyr::select(Date, Close) |>
      mutate(Date = as.Date(Date, '%m/%d/%y')) |>
      setNames(c('Date', asset[i]))
    
    if (i==1) dfm <- temp
    else dfm <- merge(dfm, temp, by = 'Date', all.x = T, all.y = T)
  }

  rownames(dfm) <- dfm$Date
  dfm <- dplyr::select(dfm, !c(Date)) |>
    as.xts() |>
    na.locf() |>
    na.omit()
  
  return(dfm)
}

# computing equity assuming a starting value of 10000
# function for computing cumulative returns
data.cumret <- function(df_ret, initial_eq, geometric) {
  if (geometric == T) CumRet <- cumprod(na.omit(df_ret) + 1) - 1
  else CumRet = cumsum(df_ret) 
  
  Wealth <- initial_eq + (initial_eq * CumRet)
  Wealth <- reclass(Wealth, df_ret)
  
  return(Wealth)
}


# Multivariate student's t estimator
covt <- function(x, ...) MASS::cov.trob(x, ...)

# Adaptive re-weighted estimator
arw <- function(x, ...) {
  ans <- mvoutlier::arw(as.matrix(x), colMeans(x), cov(x), ...)
  list(center = ans$m, cov = ans$c)
}

# The estimators to use for the tangency portfolios - custom made

# Helper functionm for OGK
scaleTau2.custom <- function (x, ...)
{
  scaleTau2(x, mu.too = T, ...)
}

covtEstimator=function(x,spec = NULL,...) 
{
  # estimating nu
  nu.est = fMultivar::mstFit(x)
  nu.val = nu.est@fit$dp.complete$nu
  message('Multivariate t (df): ', nu.val)
  
  t.rob <- MASS::cov.trob(x, nu = nu.val)
  list(mu = t.rob$center, Sigma = t.rob$cov)
}

arwEstimator=function(x,spec=NULL,...) 
{
  ans <- mvoutlier::arw(as.matrix(x), colMeans(x), cov(x), ...)
  list(mu = ans$m, Sigma = ans$c)
}

baggedEstimator <- function(x,spec=NULL,...)
{
  b.rob <- assetsMeanCov(x, method = 'bagged', baggedR = 250, ...)
  list(mu = b.rob$center, Sigma = b.rob$cov)
}

nnvEstimator <- function(x,spec=NULL,...)
{
  n.rob <- assetsMeanCov(x, method = 'nnve', ...)
  list(mu = n.rob$mu, Sigma = n.rob$Sigma)
}

covOGKEstimator2 <- function (x, spec = NULL, ...) 
{
  stopifnot(inherits(x, "timeSeries"))
  x.mat = getDataPart(x)
  ogk.rob <- robustbase::covOGK(x.mat, sigmamu = robustbase::scaleTau2) # scaleTau2.custom
  mu = ogk.rob$center
  Sigma = ogk.rob$cov
  
  colnames(Sigma) <- rownames(Sigma) <- names(mu) <- colnames(x)
  list(mu = mu, Sigma = Sigma)
}

# the constituents of NSE 20
nse_20 <- list(
  `Banking Sector` = c(
    "ABSA",
    "SBIC",
    "DTK",
    "EQTY",
    "KCB",
    "NCBA",
    "SCBK",
    "COOP"
  ),
  `Commercial and Services Sector` = c("NMG", "SCAN"),
  `Construction and Allied Sector` = c("BAMB"),
  `Energy and Petroleum Sector` = c("KEGN", "KPLC"),
  `Insurance Sector` = c("BRIT", "KNRE"),
  `Investment Sector` = c("CTUM"),
  `Investment Services Sector` = c("NSE"),
  `Manufacturing and Allied Sector` = c("BAT", "EABL"),
  `Telecommunication Sector` = c("SCOM")
) |>
  unlist() |>
  unname()

nse_sec <- readxl::read_xlsx('data/Complete-List-of-Listed-Companies-on-Nairobi-Securities-Exchange-Jan-2020-Best.xlsx')

# data: price
dd <- asset_loader(nse_20) 
  
  # CalculateReturns() |> 
  # na.fill(fill = 0)

dd <- dd['2015::']

# constructing weekly returns from the price data
wdd_list <- 1:ncol(dd) |>
  map(\(x) weeklyReturn(dd[, x]))
for (i in 1:length(wdd_list)) {
  if (i == 1) wdd = wdd_list[[i]]
  else wdd = merge(wdd, wdd_list[[i]])
}
colnames(wdd) = colnames(dd)


