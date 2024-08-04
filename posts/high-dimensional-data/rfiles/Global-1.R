

# Global files, functions, packages and data ------------------------------


# Libs --------------------------------------------------------------------

library(pacman)
p_load(dplyr, ggplot2, zoo, xts, stringr, stringi, lubridate,
       PerformanceAnalytics, PortfolioAnalytics, corrplot, pls, glmnet,
       scales, leaps, gridExtra, gtools, ModelMetrics, forcats,
       caret, kernlab, fastICA, tidymodels, knitr, pROC,
       kableExtra)

# Functions ---------------------------------------------------------------

# function for tidy-ing the output of step_pca variance explained in the components
create_var_explained <- function(var_tidy_model)
{
  uu <- unique(var_tidy_model$terms)
  for (i in 1:length(uu))
  {
    if (i == 1)
    {
      temp_var <- var_tidy_model |>
        filter(terms == uu[i])
      true_length = nrow(temp_var)
      
      ret_df <- temp_var |>
        select(component, value) |>
        setNames(c("component", uu[i]))
    }
    else
    {
      temp_var <- var_tidy_model |>
        filter(terms == uu[i]) |>
        select(value)
      ret_df <- cbind(ret_df, temp_var) |>
        data.frame() |>
        setNames(c("component", uu[1:i]))
    }
  }
  return(ret_df)
}

# a function to plot model fit
plot_model_fit <- function(model, num_models)
{
  stat_df <- data.frame(
    id = 1:num_models,
    rss = model$rss,
    adj_rsq = model$adjr2,
    cp = model$cp,
    bic = model$bic
  )
  
  grid.arrange(
    ggplot(data = stat_df)+
      geom_line(aes(x = id, y = adj_rsq))+
      geom_point(aes(x = id, y = adj_rsq))+
      labs(title = "Adjusted R-squared", x = "Model", y = "Adj R-sq")+
      theme_minimal(),
    ggplot(data = stat_df)+
      geom_line(aes(x = id, y = cp))+
      geom_point(aes(x = id, y = cp))+
      labs(title = "Cp statistic", x = "Model", y = "Cp")+
      theme_minimal(),
    ggplot(data = stat_df)+
      geom_line(aes(x = id, y = bic))+
      geom_point(aes(x = id, y = bic))+
      labs(title = "Bayesian Information Criterion", x = "Model", y = "BIC statistic")+
      theme_minimal(),
    nrow = 1
  )
}

cn <- c("Annualized Return", "Annualized Std Dev", "Annualized Sharpe (Rf=30.48%)", 
        "rho1", "rho2", "rho3", "rho4", "rho5", "rho6", "Q(6) p-value", 
        "daily  Std Dev", "Skewness", "Kurtosis", "Excess kurtosis", 
        "Sample skewness", "Sample excess kurtosis", "Semi Deviation", 
        "Gain Deviation", "Loss Deviation", "Downside Deviation (MAR=40%)", 
        "Downside Deviation (Rf=30.48%)", "Downside Deviation (0%)", 
        "Maximum Drawdown", "Historical VaR (95%)", "Historical ES (95%)", 
        "Modified VaR (95%)", "Modified ES (95%)", "daily downside risk", 
        "Annualised downside risk", "Downside potential", "Omega", "Sortino ratio", 
        "Upside potential", "Upside potential ratio", "Omega-sharpe ratio", 
        "Sterling ratio", "Calmar ratio", "Burke ratio", "Pain index", 
        "Ulcer index", "Pain ratio", "Martin ratio", "Minimum", "Quartile 1", 
        "Median", "Arithmetic Mean", "Geometric Mean", "Quartile 3", 
        "Maximum", "SE Mean", "LCL Mean (0.95)", "UCL Mean (0.95)", "StdDev Sharpe (Rf=0.1%, p=95%):", 
        "VaR Sharpe (Rf=0.1%, p=95%):", "ES Sharpe (Rf=0.1%, p=95%):", 
        "Alpha", "Beta", "Beta+", "Beta-", "R-squared", "Annualized Alpha", 
        "Correlation", "Correlation p-value", "Tracking Error", "Active Premium", 
        "Information Ratio", "Treynor Ratio", 
        "Beta CoVariance", "Beta CoSkewness", "Beta CoKurtosis", "Specific Risk", 
        "Systematic Risk", "Total Risk", "Up Capture", "Down Capture", 
        "Up Number", "Down Number", "Up Percent", "Down Percent")

fdf <- read.csv("Data/full_data.csv")
yq <- fdf[, 1]
fdf <- fdf |>
  dplyr::select(!c(all_of("X"))) |>
  setNames(all_of(cn))
rownames(fdf) <- yq
  
