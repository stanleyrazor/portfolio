

library(pacman)

p_load(dplyr,
       ggplot2,
       tidyverse,
       stringr,
       stringi,
       fitdistrplus,
       readxl,
       lubridate,
       plotly)

# Loading Data ------------------------------------------------------------
# setwd('/Users/cema/Documents/GitHub/NCWorkforce/Claims-experience')
claimdata <- read_excel("/Users/cema/Documents/GitHub/NCWorkforce/Claims-experience/Data/claims2.xlsx")

claimdata <- claimdata |>
  dplyr::select(!c("CLAIM", "percentile")) |>
  mutate(
    REV_ACC_DATE = ymd("1900-01-01") + REV_ACC_DATE,
    min_report_date = ymd("1900-01-01") + min_report_date
  ) |>
  setNames(c("class", "reportdate", "amount", "setdate"))
