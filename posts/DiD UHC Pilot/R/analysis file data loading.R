
# temporary wdir: setwd("~/Documents/GitHub/KDHS Model Updates")

library(pacman)
p_load(stringr, haven, dplyr, ggplot2, lubridate, gghighlight, hstats, 
       gridExtra)

theme_stanley <- theme_bw(base_line_size = 0) + 
  theme(axis.title = element_text(family = "Times New Roman"),
        legend.text = element_text(size = 10, family = "Times New Roman"),
        plot.title = element_text(size = 14, face = "bold", family = "Times New Roman"),
        plot.subtitle = element_text(size = 10, family = "Times New Roman"),
        plot.caption = element_text(family = "Times New Roman"))

# Inflation data ----------------------------------------------------------

# a dataset for annual average inflation: world bank
inflation_data = data.frame(
  period = c(rep(2014, 6), rep(2022, 4)),
  average = c(9.23, 3.96, 14.02, 9.38, 5.72, 6.88,
              5.24, 5.4, 6.11, 7.66)
) |>
  group_by(period) |>
  reframe(inflation = median(average))

# Insurance data ----------------------------------------------------------

# 2022 insurance
pr22 <- read_dta('/Users/cema/Documents/GitHub/KDHS Model Updates/data/2022 DHS/KEPR8AFL.DTA') 
pr22.a <- pr22 |> select(hv001, hv002, hv003, bord = hc64, ,wt = hv005,
                        county = hv024, insurance = sh27) |>
  mutate(caseid = paste0(hv001, ' ', hv002, ' ', hv003),
         county = as_factor(county),
         wt = wt / 1e6) |>
  select(!starts_with('hv')) |>
  filter(!is.na(bord) & !is.na(insurance)) |>
  distinct() |>
  group_by(caseid) |>
  filter(bord == max(bord)) |>
  ungroup() |>
  select(-bord) |>
  group_by(county) |>
  reframe(insurance = weighted.mean(insurance, wt, na.rm = T)) |>
  mutate(period = 2022)


# 2014 insurance
pr14.a = i14 |>
  select(wt = v005, insurance = v481, county = sregion) |>
  mutate(wt = wt / 1e6,
         county = as_factor(county),
         insurance = as_factor(insurance),
         insurance = data.table::fifelse(insurance == 'no', 0, 1, NA)) |>
  group_by(county) |>
  reframe(insurance = weighted.mean(insurance, wt, na.rm = T)) |>
  mutate(period = 2014)

insurance_data = rbind(pr14.a, pr22.a) |>
  mutate(
    county = case_when(
      county == 'muranga' ~ "murang'a",
      county == 'tharaka' ~ 'tharaka-nithi',
      county == 'trans-nzoia' ~ 'trans nzoia',
      county == 'elgeyo marak' ~ 'elgeyo-marakwet',
      TRUE ~ as.character(county)
    )
  )



# Health accessibility data -----------------------------------------------

i22 <- read_dta('/Users/cema/Documents/GitHub/KDHS Model Updates/data/2022 DHS/KEIR8AFL.DTA')
i14 <- read_dta('/Users/cema/Documents/GitHub/KDHS Model Updates/data/2014 DHS/KEIR72FL.DTA')

# selecting the variables of interest
dd = rbind(
  i14 |>
    select(caseid, wt = v005, county = sregion, v467b, v467c, v467d, v467f) |>
    mutate(across(.cols = -wt, .fns = as_factor)) |>
    mutate(caseid = str_trim(caseid) |> str_squish(),
           wt = wt / 1e6,
           dhs = 2014,
           across(
             .cols = starts_with('v467'),
             .fns = \(x) {
               y = case_when(
                 x == 'big problem' ~ 1,
                 x == 'not a big problem' ~ 0,
                 TRUE ~ NA
               )
               y
             }
           )
    ),
  
  i22 |>
    select(caseid, wt = v005, county = v024, v467b, v467c, v467d, v467f) |>
    mutate(across(.cols = -wt, .fns = as_factor)) |>
    mutate(caseid = str_trim(caseid) |> str_squish(),
           wt = wt / 1e6,
           dhs = 2022,
           across(
             .cols = starts_with('v467'),
             .fns = \(x) {
               y = case_when(
                 x == 'big problem' ~ 1,
                 x == 'not a big problem' ~ 0,
                 TRUE ~ NA
               )
               y
             }
           )
    )
)

dd2 = dd |>
  group_by(county, dhs) |>
  reframe(
    across(
      .cols = starts_with('v467'),
      .fns = \(x, w = wt) weighted.mean(x = x, w = wt, na.rm = T)
    )
  ) |>
  mutate(
    county = case_when(
      county == 'muranga' ~ "murang'a",
      county == 'tharaka' ~ 'tharaka-nithi',
      county == 'trans-nzoia' ~ 'trans nzoia',
      county == 'elgeyo marak' ~ 'elgeyo-marakwet',
      TRUE ~ as.character(county)
    )
  ) |>
  setNames(c('county', 'period', 'permission', 'money', 'distance', 'alone'))

# Merging the datasets
dd3 = merge(dd2, insurance_data, by = c('county', 'period')) |>
  merge(inflation_data, by = 'period')

# dd4 = pivot_longer(dd3, -c(period, county))

# Visualizations ----------------------------------------------------------

# function for plotting data
plot_graph <- function(data, column, title, subtitle, xlab, ylab) {
  temp = data |> select(all_of(column), period, county) |>
    setNames(c('column', 'period', 'county'))
  
  ggplot(temp) +
    geom_point(aes(x = factor(period), y = column, col = county, group = county),
               show.legend = F) + 
    geom_line(aes(x = factor(period), y = column, col = county, group = county),
              show.legend = F, linewidth = 1.5) + 
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title = title,
      subtitle = subtitle,
      x = xlab,
      y = ylab
    ) +
    gghighlight(county %in% c('isiolo', 'kisumu', 'nyeri', 'machakos'),
                unhighlighted_params = list(linewidth = .8)) +
    theme_stanley
}

grid.arrange(
  plot_graph(dd3, 'money', 
             title = 'Getting medical help for self: ',
             subtitle = 'Difficulty getting money needed for treatment',
             xlab = 'Period',
             ylab = 'Proportion of women'),
  plot_graph(dd3, 'permission', 
             title = 'Getting medical help for self: ',
             subtitle = 'Difficulty getting permission needed for treatment',
             xlab = 'Period',
             ylab = 'Proportion of women'),
  plot_graph(dd3, 'distance', 
             title = 'Getting medical help for self: ',
             subtitle = 'Difficulty accessing treatment due to distance',
             xlab = 'Period',
             ylab = 'Proportion of women'),
  plot_graph(dd3, 'alone', 
             title = 'Getting medical help for self: ',
             subtitle = 'Difficulty accessing treatment due to fear of going alone',
             xlab = 'Period',
             ylab = 'Proportion of women'),
  nrow = 2
)

ggplot(dd3) +
  geom_point(aes(x = factor(period), y = insurance, col = county, group = county),
             show.legend = F) + 
  geom_line(aes(x = factor(period), y = insurance, col = county, group = county),
            show.legend = F, linewidth = 1.5) + 
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = 'Insurance coverage',
    # subtitle = 'Difficulty getting money needed for treatment',
    x = 'Period',
    y = 'Proportion of women'
  ) +
  gghighlight(county %in% c('isiolo', 'kisumu', 'nyeri', 'machakos'),
              unhighlighted_params = list(linewidth = .8)) +
  theme_bw(base_line_size = 0)



# Visualizing marginals ---------------------------------------------------

# c: money | d: distance
dd4 = dd |>
  select(caseid, wt, county, dhs, money = v467c, distance = v467d) |>
  mutate(
    money_alone = data.table::fifelse(money == 1 & distance == 0, 1, 0, NA),
    distance_alone = data.table::fifelse(money == 0 & distance == 1, 1, 0, NA),
    joint_md = data.table::fifelse(money == 1 & distance == 1, 1, 0, NA)
  )

dd4.a <- dd4 |>
  group_by(county, dhs) |>
  reframe(
    across(
      .cols = c(money_alone, distance_alone, joint_md),
      .fns = \(x, w = wt) weighted.mean(x = x, w = wt, na.rm = T)
    )
  ) |>
  mutate(
    county = case_when(
      county == 'muranga' ~ "murang'a",
      county == 'tharaka' ~ 'tharaka-nithi',
      county == 'trans-nzoia' ~ 'trans nzoia',
      county == 'elgeyo marak' ~ 'elgeyo-marakwet',
      TRUE ~ as.character(county)
    )
  ) |>
  setNames(c('county', 'period', 'money', 'distance', 'joint'))

dd4.a
grid.arrange(
  plot_graph(dd4.a, 'money', 
             title = 'Difficulty getting medical help for self: ',
             subtitle = 'Women who responded with\nmoney as being the only factor.',
             xlab = NULL,
             ylab = 'Proportion of women'),
  plot_graph(dd4.a, 'distance', 
             title = '', #'Difficulty getting medical help for self: ',
             subtitle = 'Women who responded with\ndistance as being the only factor.',
             xlab = NULL,
             ylab = NULL),
  plot_graph(dd4.a, 'joint', 
             title = NULL, #'Difficulty getting medical help for self: ',
             subtitle = 'Women who responded with\nmoney and distance jointly.',
             xlab = 'Period',
             ylab = 'Proportion of women'),
  nrow = 2
)





# Modelling ---------------------------------------------------------------

dd3.b = dd3 |>
  mutate(
    treatment = ifelse(county %in% c('isiolo', 'kisumu', 'nyeri', 'machakos'), 'UHC', 'Non-UHC')
  ) |>
  mutate(period = factor(period)) 

dd5 = dd3.b |>
  select(period, county, money, insurance, inflation, treatment) |>
  filter(
    county %in% c('isiolo', 'kisumu', 'nyeri', 'machakos',
                  'samburu', 'siaya', 'kiambu', 'makueni')
  ) |>
  mutate(type = case_when(
    county %in% c('isiolo', 'samburu') ~ 'Pastoral',
    county %in% c('kisumu', 'siaya') ~ 'HIV',
    county %in% c('nyeri', 'kiambu') ~ 'MMR',
    county %in% c('machakos', 'makueni') ~ 'Road traffic',
    TRUE ~ as.character(county)    
  ))


m1 = glm(money ~ period*treatment*insurance, data = dd5, family = binomial(link = 'probit'))
m2 = lme4::glmer(money ~ period*treatment*insurance + (1 | county), data = dd5, family = binomial())

pp = predict(m2, se.fit = T, type = 'link') |>
  data.frame() |>
  mutate(conf.low = -2*se.fit, conf.high = 2*se.fit,
         across(.cols = c('fit', 'conf.low', 'conf.high'),
                .fns = plogis)) |>
  cbind(dd5 |> select(period, county, money, treatment, type)) |>
  select(period, county, money, treatment, type, fit, starts_with('conf'))

ggplot(pp, aes(x = factor(period), y = money)) + 
  geom_point(aes(x = factor(period), y = money, col = county, group = county),
             show.legend = F) + 
  geom_line(aes(x = factor(period), y = fit, col = county, group = county),
            show.legend = F, linewidth = 1) + 
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high, group = county, col = county),
                size=.2,width=.1,show.legend = F) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = 'Health inaccessibility due to money',
    subtitle = 'Difficulty getting money needed for treatment',
    x = 'Period',
    y = 'Proportion of women'
  ) + 
  theme_stanley

# using gaussian link
m1 = glm(lp ~ period*treatment*insurance, 
         data = dd5 |> mutate(lp = log(money / (1 - money))),
         family = gaussian())
m2 = lme4::lmer(lp ~ period*treatment*insurance + (1 | county),
                 data = dd5 |> mutate(lp = log(money / (1 - money))))




pp = predict.lm(m2, interval = 'confidence') |>
  data.frame() |>
  mutate(across(.cols = c('fit', 'lwr', 'upr'),
                .fns = plogis)) |>
  cbind(dd5 |> select(period, county, money, treatment, type)) |>
  select(period, county, money, treatment, type, fit, conf.low=lwr, conf.high=upr)

ggplot(pp, aes(x = factor(period), y = money)) + 
  # geom_point(aes(x = factor(period), y = money, col = treatment, group = county),
  #            show.legend = F, alpha=.2) + 
  geom_line(aes(x = factor(period), y = fit, col = treatment, group = treatment),
            linewidth = 1) + 
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high, group = county, col = treatment),
                size=.2,width=.1,show.legend = F,linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = 'Health inaccessibility due to money',
    subtitle = 'Difficulty getting money needed for treatment',
    x = 'Period',
    y = 'Proportion of women'
  ) + 
  facet_wrap(~type) +
  theme_stanley



pp = partial_dep(m2,
                 X = dd4[, c('period', 'treatment', 'money', 'insurance')],
                 pred_fun = \(model, newdata) predict(model, newdata, type = 'response'),
                 n_max = nrow(dd4),
                 v = 'period',
                 BY = 'treatment',
                 grid_size = 1e6)

(rplot <- plot(pp, show_points = T) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x= 'Year', y = 'Partial dependence\n(Average prediction of health inaccessibility due to money)',
         title = 'Health inaccessibility due to money') )

cor(dd4$money[dd4$period == 2022], dd4$insurance[dd4$period == 2022])


dd6 = dd4 |>
  select(period, county, money) |>
  mutate(spred = predict(m1, type = 'response'),
         rpred = predict(m2, type = 'response'))






