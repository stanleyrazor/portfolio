
# plotting theme
{scientific_theme <- theme(
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
)}


# -------------------------------------------------------------------------


# Example code for plotting
ggplot(simulation_grid, aes(x = sample_size, y = stability_est, 
                            color = as.factor(noise_vars_ratio))) +
  geom_point() + geom_line() +   
  geom_errorbar(aes(ymin = stability.ci_low, ymax = stability.ci_high), 
                width = 0.2) + 
  facet_wrap(~ noise_variance, nrow=1) + 
  labs(
    x = "Sample Size",
    y = "Stability",
    color = "Noise Variables Ratio",
    title = "Stability vs Sample Size",
    subtitle = "Faceted by Noise Variance"
  ) +
  theme_bw(base_line_size = 0) +
  scientific_theme


# -------------------------------------------------------------------------

ggplot(simulation_grid, aes(x = sample_size, y = fdr_est, 
                            color = as.factor(noise_variance))) +
  geom_point() + geom_line() +   
  geom_errorbar(aes(ymin = fdr.ci_low, ymax = fdr.ci_high), 
                width = 0.2) + 
  facet_wrap( ~ noise_vars_ratio, nrow=1) + 
  labs(
    x = "Sample Size",
    y = "FDR",
    color = "Noise Variables Ratio",
    title = "FDR vs Sample Size",
    subtitle = "Faceted by Noise Variance"
  ) +
  theme_bw(base_line_size = 0) +
  scientific_theme

