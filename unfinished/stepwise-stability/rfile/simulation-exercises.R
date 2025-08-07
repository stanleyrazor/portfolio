
library(pacman)
p_load(dplyr, purrr, ggplot2, stringi, stringr)

set.seed(7)
n <- 500; p <- 100; true <- 40; noise <- p - true; rho <- .8

# my intial attempt
# f <- rnorm(n, mean = 0, sd = 1)
# y <- rnorm(n, mean = f, sd = 1)
# xt <- 1:true |>
#   map(\(x) rnorm(n, mean = sqrt(rho) * f, sd = sqrt(1 - rho))) |>
#   list2DF() |>
#   setNames(paste0('true_', 1:true))
# xn <- 1:noise |>
#   map(\(x) rnorm(n, mean = 0, sd = 1)) |>
#   list2DF() |>
#   setNames(paste0('noise_', 1:noise))

# paper writers attempt
f <- rnorm(n, mean = 0, sd = 1)
y <- f + rnorm(n, mean = 0, sd = 1)
xt <- (data.frame(sqrt(1 - rho) * matrix(rnorm(n * true), ncol = true) + sqrt(rho) * f)) |>
  setNames(paste0('true_', 1:true))
xn <- (data.frame(matrix(rnorm(n * (p - true)), ncol = p - true))) |>
  setNames(paste0('noise_', 1:noise))

r_xt_f <- cor(xt, f) |> as.numeric() |> abs()
r_xn_f <- cor(xn, f) |> as.numeric() |> abs()
r_xt_y <- cor(xt, y) |> as.numeric() |> abs()
r_xn_y <- cor(xn, y) |> as.numeric() |> abs()

data.frame(
  variable = c(rep('True', true), rep('Noise', noise)),
  with_f = c(r_xt_f, r_xn_f),
  with_y = c(r_xt_y, r_xn_y)
) |>
  ggplot() + 
  geom_point(aes(x = with_y, with_f, col = variable),
             show.legend = F) + 
  labs(x = expression(abs(R(x[j], y))),
       y = expression(abs(R(x[j], f)))) +
  theme_bw(base_line_size = 0) + 
  theme(text = element_text(family = "serif", color = "black"))


# Dataset -----------------------------------------------------------------

d1 <- cbind(xt, xn, y)[, sample(true + noise + 1)]


# fm <- lm(y ~ ., data = d1)
# sm <- MASS::stepAIC(fm, direction = "both")
# summary(sm)

bt_index <- 1:20 |>
  map(\(x) sample(n, n, replace = T))
var_choice <- list()

for (i in 1:20) {
  message('// -- ', i)
  temp_data <- d1[bt_index[[i]], ]
  
  fm <- lm(y ~ ., data = temp_data)
  sm <- MASS::stepAIC(fm, direction = "both", trace = 0)
  
  var_choice[[i]] <- (coef(sm) |> names())[-1]
  rm(fm, sm, temp_data)
}

# in each run out of the 20, how many times were the true variables chosen ?
var_choice |>
  lapply(FUN = \(x) {
    tvec <- str_starts(x, "true")
    sum(tvec) / length(x)
  }) |>
  unlist()

# how many times was a particular variable being selected ?
(bif_data <- var_choice |>
  unlist() |>
  table() |>
  data.frame() |>
  mutate(Freq = Freq / 20))



