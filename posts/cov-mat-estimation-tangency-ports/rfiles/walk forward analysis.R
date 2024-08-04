

cov.est <- c('covEstimator','covMcdEstimator', 'covOGKEstimator2', 'mveEstimator',
             'shrinkEstimator', 'baggedEstimator', 'covtEstimator',  'arwEstimator'
             )
# 'mcdEstimator', its so slow
# 'nnvEstimator', its buggy and cant figure out why it doesnt work at times


# Walk forward analysis ---------------------------------------------------

# Function for constructing the indexes for the training and testing datasets
{
  ratio_optimize <- 4
  u.y <- unique(year(ymd(index(dd))))
  
  train.test <- data.frame(
    train.from = u.y,
    train.to = u.y + (ratio_optimize - 1)
  ) |>
    mutate(train = str_c(train.from, '::', train.to),
           test = train.to + 1) |>
    dplyr::filter(test %in% u.y) |>
    dplyr::select(train, test)
  train.test
}

# constructing weekly returns from the price data
wdd_list <- 1:ncol(dd) |>
  map(\(x) quantmod::weeklyReturn(dd[, x]))
for (i in 1:length(wdd_list)) {
  if (i == 1) wdd = wdd[[i]]
  else wdd = merge(wdd, wdd_list[[i]])
}
colnames(wdd) = colnames(dd)


wdf_list <- list()
for (i in 1:nrow(train.test))
{
  message('Optimizing on: ', train.test[i, 1], ' || Validating on: ', train.test[i, 2])
  # constructing the training and testing sets
  dd_train <- wdd[train.test[i, 1]]
  dd_test <- wdd[as.character(train.test[i, 2])]
  
  for (mthd in 1:length(cov.est))
  {
    message('Running estimator: ', mthd , ' / ', length(cov.est))
    
    pspec <- portfolioSpec()
    setEstimator(pspec) <- cov.est[mthd]
    setNFrontierPoints(pspec) <- nrow(dd_train)
    
    t.port <- tangencyPortfolio(data = as.timeSeries(dd_train),
                                spec = pspec,
                                constraints = "LongOnly")
    
    # storing the weights
    if (mthd == 1) wdf <- getWeights(t.port) |> data.frame() |> setNames(cov.est[mthd])
    else wdf <- cbind(wdf, getWeights(t.port) |> data.frame() |> setNames(cov.est[mthd]))
    
    rm('pspec', 't.port')
  }
  
  wdf_list[[i]] <- wdf
}

# degrees of freedom for the fitted t distributions
# t.df <- c(6.44682488225169, 4.31056854169181, 3.1541986057568, 6.21132148256873)
t.df <- c(8.44060731324762, 7.16408056323686, 6.85015733027238, 6.50181698339863)


# Extracting the weights and constructing the portfolios
test_data <- wdd[str_c(train.test[1, 2], '::', train.test[1, 4])]
test_weights <- xts(x = matrix(NA, nrow = nrow(test_data), ncol = ncol(test_data)),
                    order.by = index(test_data))
colnames(test_weights) <- colnames(dd)
test_port <- xts(x= matrix(NA, nrow = nrow(test_data), ncol = length(cov.est)),
                 order.by = index(test_data))
colnames(test_port) <- cov.est
u.y <- unique(year(index(test_data))) |> as.character()

for (cm in 1:length(cov.est))
{
  message('Validating estimator: ', cm, ' / ', length(cov.est))
  for (y in 1:length(u.y))
  {
    if (y == 1)
    {
      weight_holder <- xts(x = matrix(rep(wdf_list[[y]][, cov.est[cm]], 
                                          each = nrow(test_weights[u.y[y]])),
                                      ncol = 20),
                           order.by = index(test_weights[u.y[y]]))
      colnames(weight_holder) <- colnames(wdd)
    }
    else
    {
      temp_weight_holder <- xts(x = matrix(rep(wdf_list[[y]][, cov.est[cm]], 
                                               each = nrow(test_weights[u.y[y]])),
                                           ncol = 20),
                                order.by = index(test_weights[u.y[y]]))
      colnames(temp_weight_holder) <- colnames(wdd)
      
      weight_holder <- rbind(weight_holder, temp_weight_holder)
    }
  }
  test_weights <- weight_holder
  
  # computing the portfolio for that estimator
  test_port[, cov.est[cm]] <- Return.portfolio(R = test_data, weights = test_weights, geometric = T)
}

# write.csv(test_weights, 'data/test_weights.csv')
# write.csv(test_port, 'data/test_port.csv')

colnames(test_port) <- c('SAMPLE', 'MCD', 'OGK', 'MVE', 'SHRINK', 
                         'BAGGED', 'COV-T', 'ARW')

chart.CumReturns(test_port, geometric = T, wealth.index = T, plot.engine = 'plotly')
table.AnnualizedReturns(test_port, geometric = T)


# Benchmarking ------------------------------------------------------------

eqw <- Return.portfolio(test_data, geometric = T)
test_port$EQW <- eqw

table.SFM(Ra = test_port[, !c('SAMPLE', 'EQW')], Rb = test_port[, 'EQW'])
table.SFM(Ra = test_port[, -1], Rb = test_port[, 1])




# Results -----------------------------------------------------------------


# equity charts
equity <- test_port |> data.cumret(initial_eq = 10000, geometric = T)
equity <- data.frame(equity) |>
  setNames(colnames(test_port))

# table
.tbl_stats <- rbind(
  table.AnnualizedReturns(test_port),
  maxDrawdown(test_port),
  round(tail(equity, 1), 2)
) |>
  t()
.tbl_stats[, c(1,2,4)] <- apply(.tbl_stats[, c(1,2,4)], 2, scales::percent)
.tbl_stats <- t(.tbl_stats) |> data.frame() |>
  setNames(colnames(test_port))
rownames(.tbl_stats) <- c('Annualized Return', 'Annualized Volatility',
                          'Annualized Sharpe(Rf=0%)', 'Maximum Drawdown', 'End Equity')


# the graphs
pl.tbl <- ggtexttable(.tbl_stats, theme = ttheme("light")) 
pl.tbl <- pl.tbl |>
  table_cell_bg(row = 2:tab_nrow(pl.tbl), column = 9, fill = "darkolivegreen1")
pl.eq <- equity |>
  mutate(Date = ymd(rownames(equity))) |>
  pivot_longer(-Date) |>
  ggplot()+
  geom_line(aes(x = Date, y = value, col = name))+
  labs(title = 'Equity Chart',
       subtitle = 'Initial equity: 10,000 KES',
       x = '', y = 'Wealth',
       col = "Estimator:")+
  theme_pubclean()

ggarrange(pl.eq, pl.tbl, nrow = 2, heights = c(4,1))

