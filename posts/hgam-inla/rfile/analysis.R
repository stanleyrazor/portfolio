
library(pacman)
p_load(gamlss, dplyr, ggplot2, performance, datasets, INLA, mgcv)

# my theme
my_theme <- theme(
  # Text elements
  text = element_text(family = "serif", color = "black"),
  plot.title = element_text(size = 12, face = "plain", hjust = 0.5),
  axis.title = element_text(size = 12, face = "plain"),
  axis.text = element_text(size = 12),
  axis.text.x = element_text(angle = 0, hjust = 0.5),
  axis.text.y = element_text(angle = 0, hjust = 1),
  legend.title = element_text(size = 12),
  legend.text = element_text(size = 12),
  
  # Plot background and grid
  panel.background = element_rect(fill = "white"),
  panel.grid = element_blank(),
  
  # Axis lines and ticks
  axis.line = element_line(color = "black"),
  axis.ticks = element_line(color = "black"),
  
  # Remove the right and top axis lines (bty="l" equivalent)
  axis.line.y.right = element_blank(),
  axis.line.x.top = element_blank(),
  
  # Legend
  legend.background = element_rect(fill = "white"),
  legend.key = element_rect(fill = "white", color = NA),
  
  # Plot margins (approximating mar = c(5, 5, 3, 5))
  plot.margin = margin(t = 3, r = 5, b = 5, l = 5, unit = "pt"),
  
  # Expand axes to touch the data (xaxs="i", yaxs="i" equivalent)
  panel.spacing = unit(0, "lines"),
  plot.title.position = "plot"
)


d1 <- CO2 |>
  mutate(plant_uo = as.character(Plant) |> factor(),
         log_uptake = log(uptake), log_conc = log(conc)) 
glimpse(d1)

# model G
g_mgcv <- gam(log_uptake ~ s(log_conc, bs = 'tp', k=5) + s(plant_uo, k=12, bs='re'),
    family = gaussian(), method = "REML", data = d1)

m2 <- gam(log_uptake ~ s(log_conc, k=5, bs="tp") + s(plant_uo, k=12, bs="re"),
    data=d1, method="REML", family="gaussian")

g_inla <- inla(log_uptake ~ f(log_conc, model = "rw2") + f(plant_uo, model = "iid"),
           family = "gaussian", data = d1)

d1 |>
  mutate(
    mgcv = predict(m1, type = 'response') |> as.numeric(),
    inla = i1$summary.fitted.values$mean,
    ci.low = i1$summary.fitted.values$`0.025quant`,
    ci.high = i1$summary.fitted.values$`0.975quant`
  ) |>
  ggplot() + 
  geom_point(aes(x = log_conc, y = log_uptake)) + 
  geom_line(aes(x = log_conc, y = mgcv), col = 'blue') + 
  geom_line(aes(x = log_conc, y = inla), col = 'black') + 
  # geom_ribbon(aes(x = log_conc, ymin = ci.low, ymax = ci.high), alpha = .2) +
  facet_wrap(~plant_uo, scales = 'free_y') +
  labs(title = "Model G: Global smoothers only", x = 'Log(concentration)',
       y = 'Log(uptake)') +
  theme_bw() + 
  scientific_theme
  

# -------------------------------------------------------------------------


# mgcv model
gs_mgcv <- gam(log_uptake ~ 
                 s(log_conc, k = 5, m = 2) + 
                 s(log_conc, plant_uo, k = 5, bs = "fs", m = 2),
               data=d1, method="REML")

# inla model
gs_inla <- inla(log_uptake ~ 
                  f(plant_uo, model="iid") +
                  f(log_conc_global, model = 'rw2') + 
                  f(log_conc, model = "rw2", replicate = plant_uo_id),
                family = "gaussian",
                data = d1 |>
                  mutate(plant_uo_id = as.integer(plant_uo),
                         log_conc_global = log_conc))

# or
# gs_inla <- inla(log_uptake ~ 
#                   f(plant_uo, model="iid") + 
#                   f(log_conc_group, model = "rw2", group = plant_uo_id),
#                 family = "gaussian",
#                 data = d1 |>
#                   mutate(plant_uo_id = as.integer(plant_uo),
#                          log_conc_group = inla.group(log_conc)))

d1 |>
  mutate(
    mgcv = predict(gs_mgcv, type = 'response') |> as.numeric() |> exp(),
    inla = gs_inla$summary.fitted.values$mean |> exp(),
  ) |>
  ggplot() + 
  geom_point(aes(x = conc, y = uptake)) + 
  geom_line(aes(x = conc, y = mgcv), col = 'red', lty='dashed') + 
  geom_line(aes(x = conc, y = inla), col = 'black') + 
  # geom_ribbon(aes(x = log_conc, ymin = ci.low, ymax = ci.high), alpha = .2) +
  facet_wrap(~plant_uo, scales = 'free_y') +
  labs(
    title = "Model GS: Global Smoothers + group smoothers with same wiggliness",
    caption = "The black fitted line is the INLA model, whereas, the red dashed line is the MGCV GAM model",
    x = expression('CO'[2]~''~'Concentration (mL L'^-1*')'),
    y = expression('CO'[2]~'Uptake (µmol m'^-2~')'))+
  theme_bw() + 
  my_theme


# -------------------------------------------------------------------------

# mgcv model
gi_mgcv <- gam(log_uptake ~ 
                 s(plant_uo, bs = 're', k = 12) + 
                 s(log_conc, k = 5, m = 2, bs = 'tp') + 
                 s(log_conc, k = 5, m = 1, bs = "tp", by = plant_uo),
               data=d1, method="REML")

gi_inla <- inla(log_uptake ~ 
                  f(plant_uo, model="iid") +
                   f(log_conc_global, model = "rw2") + 
                  f(log_conc, model = "rw2", group = plant_uo_id),
                family = "gaussian",
                data = d1 |>
                  mutate(plant_uo_id = as.integer(plant_uo),
                         log_conc_global = log_conc))


# or
# gs_inla <- inla(log_uptake ~ 
#                   f(plant_uo, model="iid") + 
#                   f(log_conc_group, model = "rw2", group = plant_uo_id),
#                 family = "gaussian",
#                 data = d1 |>
#                   mutate(plant_uo_id = as.integer(plant_uo),
#                          log_conc_group = inla.group(log_conc)))

d1 |>
  mutate(
    mgcv = predict(gi_mgcv, type = 'response') |> as.numeric() |> exp(),
    inla = gi_inla$summary.fitted.values$mean |> exp(),
  ) |>
  ggplot() + 
  geom_point(aes(x = conc, y = uptake)) + 
  geom_line(aes(x = conc, y = mgcv), col = 'red', lty='dashed') + 
  geom_line(aes(x = conc, y = inla), col = 'black') +
  # geom_ribbon(aes(x = log_conc, ymin = ci.low, ymax = ci.high), alpha = .2) +
  facet_wrap(~plant_uo, scales = 'free_y') +
  labs(
    title = "Model GI: Global Smoothers + group smoothers with differing wiggliness",
    caption = "The black fitted line is the INLA model, whereas, the red dashed line is the MGCV GAM model",
    x = expression('CO'[2]~''~'Concentration (mL L'^-1*')'),
    y = expression('CO'[2]~'Uptake (µmol m'^-2~')'))+
  theme_bw() + 
  my_theme


# -------------------------------------------------------------------------


