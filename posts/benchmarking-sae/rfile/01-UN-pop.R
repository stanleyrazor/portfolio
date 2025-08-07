
pacman::p_load(dplyr, INLA, raster, sf, stringr, patchwork, ggplot2, 
               tidyr, gridExtra)


scientific_theme <- theme(
  # Text elements
  text = element_text(family = "serif", color = "black"),
  plot.title = element_text(size = 12, face = "plain", hjust = 0.5),
  plot.subtitle = element_text(size = 10, hjust = 0.5),
  axis.title = element_text(size = 12, face = "plain"),
  axis.text = element_text(size = 12),
  # axis.text.x = element_text(angle = 0, hjust = 0.5),
  # axis.text.y = element_text(angle = 0, hjust = 1),
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


pop <- raster("data/ken_pd_2020_1km_UNadj.tif")

# Function to replace NA with 0 in each raster
replace_na_with_zero <- \(rst) {
  rst[is.na(rst)] <- 0
  return(rst)
}

projMercator<-"+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0
+x_0=0 +y_0=0 +k=1 +units=km +nadgrids=@null +wktext +no_defs"

bnd <- (rKenyaCensus::KenyaCounties_SHP |> st_as_sf()) |> 
  dplyr::select(county = County) |>
  st_transform(projMercator) |>
  st_make_valid()


# masking to be within kenyan boundary and replacing NA
p1 <- projectRaster(pop, crs = projMercator)
p2 <- crop(p1, bnd) |> mask(bnd)
p3 <- na.omit(p2)
# p3 <- calc(p2, replace_na_with_zero)

locs <- st_as_sf(cbind(as.data.frame(coordinates(p3)), vals = getValues(p3)),
                 coords = c("x", "y"), crs = st_crs(bnd))
l1 <- st_join(locs, bnd)
l2 <- na.omit(l1)

# extracting into a dataset
l3 <- l2 |>
  st_drop_geometry() |>
  cbind(as.data.frame(st_coordinates(l2)))

# getting population of counties from census
# target_pop <- rKenyaCensus::V1_T2.2 |>
#   dplyr::select(county = County, target = Total) |>
#   mutate(county = str_to_upper(county))

# 2020 projections
target_pop <- readRDS("data/cleaned_population_data.rds") |>
  filter(gender == "Total" & year == "2020") |>
  group_by(county) |>
  reframe(target = sum(population)) |>
  mutate(county = str_to_upper(county),
         county = case_when(county == "ELGEYO-\nMARAKWET" ~ "ELGEYO/MARAKWET",
                            county == "TAITA-TAVETA" ~ "TAITA/TAVETA",
                            county == "TRANS-NZOIA" ~ "TRANS NZOIA",
                            TRUE ~ county))


l4 <- merge(l3, target_pop, by = "county", all.x = T)

temp <- l4 |>
  group_by(county) |>
  reframe(pop = sum(vals),
          target = unique(target))
with(temp, plot(pop, target, pch = '*'))
abline(a = 0, b = 1, col = 'red')


l5 <- l4 |>
  group_by(county) |>
  mutate(wt = vals / sum(vals),
         pop = sum(vals),
         deviation = target - pop,
         
         newvals = vals + (wt * deviation)) |>
  ungroup()

agg <- l5 |>
  group_by(county) |>
  reframe(oldpop = sum(vals),
          newpop = sum(newvals),
          benchmark = unique(target))

i1 <- ggplot(agg) + 
  geom_point(aes(x = oldpop, y = benchmark)) + 
  geom_abline(aes(intercept = 0, slope = 1), col = 'red') + 
  labs(title = "Initial distribution", x = "Aggregated county population (WorldPop)",
       y = "Target county population (Census)") +
  theme_bw(base_line_size = 0) + 
  scientific_theme

i2 <- ggplot(agg) + 
  geom_point(aes(x = newpop, y = benchmark)) + 
  geom_abline(aes(intercept = 0, slope = 1), col = 'red') + 
  labs(title = "Benchmark distribution", x = "Aggregated county population (WorldPop)",
       y = "Target county population (Census)") +
  theme_bw(base_line_size = 0) + 
  scientific_theme
  
temp <- l5 |>
  mutate(
    old_scale = case_when(
      between(vals, 0, 10) ~ "0-10",
      between(vals, 11, 50) ~ "10-50",
      between(vals, 51, 100) ~ "51-100",
      between(vals, 101, 500) ~ "100-500",
      between(vals, 501, 2000) ~ "501-2000",
      TRUE ~ "2000+"
    ),
    old_scale = factor(old_scale, levels = c("0-10" , "10-50" , "51-100" , 
                                             "100-500" , "501-2000", "2000+")),
    
    new_scale = case_when(
      between(vals, 0, 10) ~ "0-10",
      between(vals, 11, 50) ~ "10-50",
      between(vals, 51, 100) ~ "51-100",
      between(vals, 101, 500) ~ "100-500",
      between(vals, 501, 2000) ~ "501-2000",
      TRUE ~ "2000+"
    ),
    new_scale = factor(new_scale, levels = c("0-10" , "10-50" , "51-100" , 
                                             "100-500" , "501-2000", "2000+"))
  )


o1 <- ggplot(temp) + 
  geom_tile(aes(x = X, y = Y, fill = old_scale),
            show.legend = F) + 
  theme_bw(base_line_size = 0) + 
  labs(x = NULL, y = NULL, title = "Un-benchmarked") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  scale_fill_viridis_d() +
  scientific_theme

n1 <- ggplot(temp) + 
  geom_tile(aes(x = X, y = Y, fill = new_scale),
            show.legend = F) + 
  theme_bw(base_line_size = 0) + 
  labs(x = NULL, y = NULL, title = "Benchmarked") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  scale_fill_viridis_d() +
  scientific_theme

ggsave(filename = "output/img/pop_dens_2.png", plot = (i1 / o1) | (i2 / n1),
       units = "in", height = 10, width = 10)



# Urban stratification ----------------------------------------------------

residence <- rbind(rKenyaCensus::V2_T2.2a |> 
                     mutate(residence = "Rural") |> 
                     dplyr::select(county = County, residence, pop = Sex_Total),
                   rKenyaCensus::V2_T2.2b |> 
                     mutate(residence = "Urban") |> 
                     dplyr::select(county = County, residence, pop = Sex_Total)
) |>
  pivot_wider(names_from = residence, values_from = pop) |>
  mutate(Rural = ifelse(is.na(Rural), 0, Rural),
         Urban = ifelse(is.na(Urban), 0, Urban),
         pop = Rural + Urban,
         uprop = Urban / pop) |>
  dplyr::select(county, uprop)


l6 <- l5 |>
  dplyr::select(county, X, Y, popdens = newvals) |>
  merge(residence, by = "county", all.x = T)


# the fuction for urban proportion finding
urban_prop <- \(pop_dens, target) {
  
  require(e1071)
  
  # x <- (pop_dens - min(pop_dens)) / max(pop_dens - min(pop_dens))
  x <- (pop_dens - mean(pop_dens)) / sd(pop_dens) # works better for sigmoid
  
  ll <- list()
  ll[[1]] <- x
  mu <- unique(target)
  
  for (i in 1:1e4) {
    if (abs(mu - mean(sigmoid(ll[[i]]))) < 1e-3) {break}
    if (i == 1) {
      grup <- ll[[i]] + ((2 * 1e-1) * (mu - mean(sigmoid(ll[[i]]))) * mean(dsigmoid(ll[[i]])))
      ll[[i+1]] <- grup
    } else {
      grup <- ll[[i]] + ((2 * 1e-1) * (mu - mean(sigmoid(ll[[i]]))) * mean(dsigmoid(ll[[i]])))
      momo <- 1e-2 * (ll[[i]] - ll[[i-1]])
      ll[[i+1]] <- momo + grup
    }
  }
  message("Max iter: ", i)
  return(sigmoid(last(ll)))
}

dhs_unit_adjust <- \(logit_uncalib_pred, benchmark_target, weights) {

    require(e1071)
    
    # x <- (logit_uncalib_pred - mean(logit_uncalib_pred)) / sd(logit_uncalib_pred) # works better for sigmoid
    x <- logit_uncalib_pred
    
    ll <- list()
    ll[[1]] <- x
    mu <- unique(benchmark_target)
    
    for (i in 1:2e4) {
      if (abs(mu - weighted.mean(sigmoid(ll[[i]]), weights)) < 1e-3) {break}
      if (i == 1) {
        grup <- ll[[i]] + ((2 * 1e-1) * (mu - weighted.mean(sigmoid(ll[[i]]), weights)) * weighted.mean(dsigmoid(ll[[i]]), weights))
        ll[[i+1]] <- grup
      } else {
        grup <- ll[[i]] + ((2 * 1e-1) * (mu - weighted.mean(sigmoid(ll[[i]]), weights)) * weighted.mean(dsigmoid(ll[[i]]), weights))
        momo <- 1e-2 * (ll[[i]] - ll[[i-1]])
        ll[[i+1]] <- momo + grup
      }
    }
    message("Max iter: ", i)
    return(sigmoid(last(ll)))
  
}

# computing the proportion urbn per grid
l7 <- l6 |>
  group_by(county) |>
  mutate(grid_urban = urban_prop(pop_dens = popdens, target = uprop)) |>
  ungroup()

p1 <- ggplot(l7) + 
  geom_point(aes(x = popdens, y = grid_urban), pch = '.') + 
  scale_y_continuous(labels = scales::percent_format()) + 
  labs(title = "Relationship between populaiton density and urban proportion of a location",
       x = "Population density", y = "Urban proportion") + 
  facet_wrap(~county, scales = "free") +
  theme_bw(base_line_size = 0) + 
  scientific_theme

agg_data <- l7 |>
  group_by(county) |>
  reframe(computed = mean(grid_urban),
          target = unique(uprop))

l7 <- l7 |>
  mutate(urban_category = cut(
    grid_urban * 100,  
    breaks = seq(0, 100, by = 5), 
    labels = paste0(seq(0, 95, by = 5), "-", seq(5, 100, by = 5)),
    include.lowest = TRUE
  ))

p2 <- ggplot(agg_data) + 
  geom_point(aes(x = computed, y = target)) + 
  geom_abline(aes(slope = 1, intercept = 0), col = "blue") +
  labs(title = "Agreement between the benchmarked \nand target urban proportions (county level)",
       # subtitle = "Proportion of a county that is urban for the 47 counties.",
       x = "Aggregated proportions\n(Benchmarked)",
       y = "Census proportions\n(KPHC 2019)") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) + 
  scientific_theme


p3_cont <- ggplot(l7) +
  geom_tile(aes(x = X, y = Y, fill = grid_urban),
            show.legend = T) +
  theme_bw(base_line_size = 0) +
  labs(x = NULL, y = NULL, title = "\n",
       fill = "(%) Urban") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  scientific_theme +
  viridis::scale_fill_viridis(labels = scales::percent_format(),
                              option = "viridis", 
                              direction = 1, limits = c(0, 1))

p3 <- ggplot(l7) +
  geom_tile(aes(x = X, y = Y, fill = urban_category), show.legend = TRUE) +
  theme_bw(base_line_size = 0) +
  labs(x = NULL, y = NULL, title = "\n", fill = "(%) Urban") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",                # Position legend at the bottom
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 7),
    legend.box = "horizontal",                 # Arrange legend items horizontally
    legend.spacing.x = unit(0.1, "cm"),        # Adjust spacing between legend items
    legend.key.width = unit(.5, "cm")           # Adjust key width for better alignment
  ) +
  guides(
    fill = guide_legend(nrow = 4, byrow = TRUE)  # Ensure 4 rows in the legend
  ) +
  viridis::scale_fill_viridis(
    discrete = TRUE,
    option = "viridis",
    direction = 1
  )

ggsave(filename = "output/img/pop_dens_to_urbanprop.png", plot = p1,
       units = "in", height = 8, width = 14)

ggsave(filename = "output/img/urban_props.png", plot = grid.arrange(p2, p3_cont, nrow = 1),
       units = "in", height = 5, width = 10)


# Demographic indicators --------------------------------------------------

# load your KDHS file: birth recode
load("/Users/cema/Documents/GitHub/KDHS Model Updates/data/KEBR8AFL_2022.RData")

b1 <- birth |>
  dplyr::select(caseid, psu = v001, county = v024, sba = m15, wealthindex = v190,
                year = b2, month = b1) |>
  haven::as_factor() |>
  mutate(across(everything(), as.character))

b2 <- b1 |> 
  mutate(
    sba = dplyr::case_when(
      sba %in% c("respondent's home", "other home", "other") ~ 0,
      sba %in% c("government hospital", "government health center", 
                 "private hospital", "government dispensary", 
                 "private clinic", "fbo/mission hospital", "ngo hospital", 
                 "fbo/mission clinic") ~ 1,  
      TRUE ~ NA_real_  
    ),
    wealthindex = dplyr::case_when(
      wealthindex %in% c("poorer", "poorest") ~ 1,
      wealthindex %in% c("middle", "richer", "richest") ~ 0, 
      TRUE ~ NA_real_
    )
  )

# creating the two datasets
sba <- b2 |>
  dplyr::select(psu, year, month, county, sba) |>
  mutate(dob = paste0(year, '-', month) |> lubridate::ym()) |>
  filter(between(dob, ymd('2018-7-1'), ymd('2019-9-1'))) |>
  na.omit() |>
  group_by(psu) |>
  reframe(
    county = first(county),
    total = n(),
    success = sum(sba)
  )

wealth <- b2 |>
  dplyr::select(caseid, psu, county, wealthindex) |>
  distinct() |>
  group_by(psu) |>
  reframe(
    county = first(county),
    total = n(),
    success = sum(wealthindex)
  )

# loading the data from census
benchmark_data <- readxl::read_xlsx("data/census-benchmarks-sba-wealth.xlsx") |>
  mutate(benchmark_poor = (poor + poorest) / 100,
         benchmark_sba = sba / 100) |>
  dplyr::select(county, benchmark_sba, benchmark_poor)


# -------------------------------------------------------------------------

projMercator<-"+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0
+x_0=0 +y_0=0 +k=1 +units=km +nadgrids=@null +wktext +no_defs"

# population raster
cov_pop_dens <- pop
cov_pop_dens <- projectRaster(cov_pop_dens, crs = projMercator, method = "ngb")
cov_pop_dens <- aggregate(cov_pop_dens, fact = 5, fun = sum, na.rm = T)

# Define common resolution and extent
# target_res <- c(0.04166665, 0.04166665) # 5*5 kms
# target_extent <- extent(33, 42, -6, 6) 
# target_crs <- "+proj=longlat +datum=WGS84 +no_defs"
# template_raster <- raster(ext = target_extent, res = target_res, crs = target_crs)

# Preparing a grid (5*5 km) ---

# Constructing a 5*5 km spatial grid over kenya
target_res <- c(0.04166665, 0.04166665) 
target_extent <- terra::ext(33, 42, -6, 6) 
target_crs <- "+proj=longlat +datum=WGS84 +no_defs"

# Create an empty raster with the specified resolution, extent, and CRS
grid <- terra::rast(ext = target_extent, res = target_res, crs = target_crs)

# Convert the raster grid to spatial points
xy <- terra::xyFromCell(grid, 1:ncell(grid)) # Extract cell coordinates
p_grid <- st_as_sf(as.data.frame(xy), coords = c("x", "y"), crs = 'WGS84')
p_grid <- p_grid |> st_transform(crs = st_crs(bnd))

bnd_grid <- st_make_valid(bnd) 
p_grid <- st_filter(p_grid, bnd_grid)

# Mesh construction ---

# coordinates for observed and prediction grid
geo22 <- st_read('~/Documents/GitHub/KDHS Model Updates/data/2022 GeoData/KEGE8AFL/KEGE8AFL.shp')
o_grid <- geo22 |>
  dplyr::select(psu = DHSCLUST, lng = LONGNUM, lat = LATNUM, 
                residence = URBAN_RURA,
                county = ADM1NAME)

o_grid <- st_transform(o_grid, crs = projMercator)
p_grid <- st_transform(p_grid, crs = projMercator)

# obtaining the residence covariate surface: using benchmarking ----

# constructing the grid, with population density values:
pop_grid <- st_as_sf(cbind(raster::extract(cov_pop_dens, st_coordinates(p_grid), df = T),
               st_coordinates(p_grid)),
         coords = c('X', 'Y'),
         crs = projMercator) |>
  dplyr::select(pop = ken_pd_2020_1km_UNadj) |>
  mutate(pop = ifelse(is.na(pop), 0, pop))

residence_estimates <- readRDS("~/Documents/GitHub/small-area-vs-spde-agg/data/output/pixel-level-residence-estimation.rds")

pg1 <- st_join(pop_grid, bnd_grid) |> st_drop_geometry() |>
  merge(residence, by = "county")

pg2 <- pg1 |>
  group_by(county) |>
  mutate(grid_urban = urban_prop(pop_dens = pop, target = uprop)) |>
  ungroup()

# -------------------------------------------------------------------------

o_grid <- o_grid |>
  mutate(residence = ifelse(residence == "U", 1, 0))

p_grid <- p_grid |>
  mutate(lng = st_coordinates(p_grid)[, 1], lat = st_coordinates(p_grid)[, 2]) |>
  mutate(residence = pg2 |> pull(grid_urban))

clust_data <- list(sba = sba, wealth = wealth)
geo_clust_data <- clust_data |>
  lapply(FUN = \(x) merge(o_grid, x |> dplyr::select(-county),  by = c("psu"), all.y = T))

# making the grid use the appropriate CRS
bnd_grid <- st_transform(bnd_grid, crs = projMercator)

unit_spde_list <- 1:length(geo_clust_data) |>
  map(\(i) {
    
    obs_grid <- geo_clust_data[[i]]
    
    message("Running: ", names(geo_clust_data)[i])
    co_o <- st_coordinates(obs_grid)
    co_p <- st_coordinates(p_grid)
    
    # mesh construction
    mesh <- inla.mesh.2d(loc = co_o, max.edge = c(100,1000),
                         # boundary = bdry,
                         offset = c(50, 100),
                         cutoff = 1) # .05
    
    # plot(mesh)
    # points(co_o, col = "red", pch = '*')
    
    # building spde on mesh: with some priors on the same
    spde <- inla.spde2.pcmatern(mesh = mesh, alpha = 2, constr = TRUE,
                                prior.range = c(50, .95),
                                prior.sigma = c(1, .5))
    
    # index set construction
    indexs <- inla.spde.make.index("s", spde$n.spde)
    lengths(indexs)
    
    # projection matrix for observation and prediction grids
    A_obs <- inla.spde.make.A(mesh = mesh, loc = co_o)
    A_pred <- inla.spde.make.A(mesh = mesh, loc = co_p)
    dim(A_obs); dim(A_pred)
    
    stk.e <- inla.stack(tag = "est",
                        data = list(y = obs_grid$success, 
                                    total_pop = obs_grid$total),
                        A = list(1, A_obs),
                        effects = list(data.frame(b0 = rep(1, nrow(A_obs)),
                                                  residence = obs_grid$residence),
                                       s = indexs))
    
    # stack for prediction stk.p
    stk.p <- inla.stack(tag = "pred",
                        data = list(y = NA),
                        A = list(1, A_pred),
                        effects = list(data.frame(b0 = rep(1, nrow(A_pred)),
                                                  residence = p_grid$residence),
                                       s = indexs))
    
    # stk.full has stk.e and stk.p
    stk.full <- inla.stack(stk.e, stk.p)
    
    ## modelling using iNLA
    formula <- y ~ 0 + b0 + residence + f(s, model = spde)
    
    m1 <- inla(formula, family = "betabinomial",
               Ntrials = total_pop,
               # weights = wts,
               data = inla.stack.data(stk.full),
               control.predictor = list(compute = TRUE,
                                        link = 1,
                                        A = inla.stack.A(stk.full)),
               control.compute = list(return.marginals.predictor = TRUE,
                                      config = T))
    
    # predicting over the entire grid
    index_pred <- inla.stack.index(stack = stk.full, tag = "pred")$data
    pred_mean <- m1$summary.fitted.values[index_pred, "mean"]
    
    # generating samples
    samp <- inla.posterior.sample(1000, m1)
    draws <- inla.posterior.sample.eval("APredictor", samp) |>
      apply(MARGIN = 2, FUN = plogis) |>
      data.frame()
    draws <- draws[index_pred, ]
    draws <- rowMeans(draws) |> as.numeric()
    
    rm(samp, m1)
    
    grid_locs <- data.frame(pop = pg2 |> pull(pop),
                            lat = co_p[, 1], lng = co_p[, 2]) |>
      cbind(pred = draws)
    
    rm(draws)
    
    # Convert grid_locs to an sf object
    grid_locs_sf <- grid_locs %>%
      st_as_sf(coords = c("lat", "lng"), crs = st_crs(projMercator))
    grid_with_counties <- st_join(grid_locs_sf, bnd_grid)
    
    grid_with_counties
  })

uncalibrated <- unit_spde_list
names(uncalibrated) <- c('sba', 'wealth')

rm(grid_locs_sf, grid_locs)

# merge with actuals
uncalibrated <- 1:2 |>
  map(\(i) {
    merge(uncalibrated[[i]], benchmark_data[, c(1, i+1)], by = "county")
  })
names(uncalibrated) <- c('sba', 'wealth')

# aggregating for plotting
uncalib_data <- lapply(uncalibrated, \(x) (x |> cbind(st_coordinates(x))) |> st_drop_geometry())


calib_data <- uncalib_data |>
  lapply(\(df) {
    df |>
      setNames(c('county', 'pop', 'pred', 'benchmark', 'X', 'Y')) |>
      group_by(county) |>
      mutate(calib_pred = dhs_unit_adjust(logit_uncalib_pred = qlogis(pred), 
                                          benchmark_target = benchmark,
                                          weights = pop)) |>
      ungroup()
  })

names(calib_data) <- c('sba', 'wealth')


# -------------------------------------------------------------------------

p1 <- bind_rows(
  (calib_data[[1]]) |>
    dplyr::select(county, pop, benchmark, pred, calib_pred) |>
    pivot_longer(-c(county, pop, benchmark)) |>
    mutate(name = ifelse(name == "pred", "Unbenchmarked", "Benchmarked"),
           name = factor(name, levels = c("Unbenchmarked", "Benchmarked")),
           variable = "Skilled Birth Attendance"),
  (calib_data[[2]]) |>
    dplyr::select(county, pop, benchmark, pred, calib_pred) |>
    pivot_longer(-c(county, pop, benchmark)) |>
    mutate(name = ifelse(name == "pred", "Unbenchmarked", "Benchmarked"),
           name = factor(name, levels = c("Unbenchmarked", "Benchmarked")),
           variable = "Wealth index (poor)")
) |>

  group_by(county, name, variable) |>
  reframe(benchmark = unique(benchmark),
          pred = weighted.mean(value, pop)) |>
  ggplot() + 
  geom_point(aes(x = pred, y = benchmark)) + 
  geom_abline(aes(intercept = 0, slope = 1), col = 'red') + 
  labs(title = NULL,
       x = "Survey (County aggregated estimates)",
       y = "Census (County direct estimates)") +
  scale_x_continuous(labels = scales::percent_format(), 
                     limits = c(0, 1)) + 
  scale_y_continuous(labels = scales::percent_format(), 
                     limits = c(0, 1)) + 
  facet_grid(variable ~ name, scales = "free") +
  theme_bw(base_line_size = 0) + 
  scientific_theme
  
m1 <- bind_rows(
  (calib_data[[1]]) |>
    dplyr::select(county, pred, calib_pred, X, Y) |>
    pivot_longer(-c(county, X, Y)) |>
    mutate(name = ifelse(name == "pred", "Unbenchmarked", "Benchmarked"),
           name = factor(name, levels = c("Unbenchmarked", "Benchmarked")),
           variable = "Skilled Birth Attendance"),
  (calib_data[[2]]) |>
    dplyr::select(county, pred, calib_pred, X, Y) |>
    pivot_longer(-c(county, X, Y)) |>
    mutate(name = ifelse(name == "pred", "Unbenchmarked", "Benchmarked"),
           name = factor(name, levels = c("Unbenchmarked", "Benchmarked")),
           variable = "Wealth index (poor)")
) |>
  
  ggplot() + 
  geom_tile(aes(x = X, y = Y, fill = value),
            show.legend = T) +
  theme_bw(base_line_size = 0) +
  labs(x = NULL, y = NULL,
       fill = "(%)") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  facet_grid(variable ~ name, scales = "free") +
  scientific_theme +
  viridis::scale_fill_viridis(labels = scales::percent_format(),
                              option = "viridis", 
                              direction = 1, limits = c(0, 1))
  

ggsave(filename = "output/img/dhs_vals.png", 
       plot = (p1 | m1),
       units = "in", height = 6, width = 12)
