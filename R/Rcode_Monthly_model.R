
# libraries ------

library(patchwork)
library(lme4)
library(tidyverse)
library(here)
library(lattice)
library(ggplot2)
library(MASS)
library(DHARMa)
library(performance)
library(glmmTMB)
library(lme4)
library(gamm4)
library(mgcv)
library(gratia)
library(pscl)
library(MuMIn)
library(lmtest)
library(visreg)
library(marginaleffects)
library(ggeffects)
library(patchwork)
library(ggplot2)
library(effects)
library(tidygam)
library(modelsummary)
library(sjPlot)
library(gtsummary)


# data import ----
monthly_data <- readRDS("path/monthly_data.rds")

# model -----
# ├ country as random factor -----
monthly_mod <- gamm4::gamm4(contact_mean_round ~  s(month, bs="cc", k = 12, by = sex) +
                                 sex + log(dist_to_farm) + enet_gpkg_1km_mean + # wild boar variables
                                 np_1km + forest_cover + hfp19_mean_1km + log(farm_area) + building_prop +
                                 pigs_density_cat + pigs_density_cat:log(farm_area), # farm variables
                               random = ~(1|country) + (1|anifarm),# include country# + (1|year),
                               family = negative.binomial(theta = 2),
                               data = monthly_data)
summary(monthly_mod$gam)

# # ├ ecoregion as random factor------
mod_ecoregion_rnd <- gamm4::gamm4(contact_mean_round ~ s(month, bs = "cc", k = 12, by = sex) +
                                    age + sex + log(dist_to_farm) + enet_gpkg_1km_mean + np_1km +
                                    forest_cover + hfp19_mean_1km + log(farm_area) + building_prop +
                                    pigs_density_cat + pigs_density_cat:log(farm_area),
                                  random =  ~(1|ecoregion) +(1|anifarm),
                                  family = MASS::negative.binomial(theta = 2),
                                  data = monthly_data)
# # ├ ecoregion as fixed effect ------
mod_ecoregion_fixed <- gamm4::gamm4(contact_mean_round ~ s(month, bs = "cc", k = 12, by = sex) +
                                          sex + log(dist_to_farm) + enet_gpkg_1km_mean + np_1km +
                                          forest_cover + hfp19_mean_1km + log(farm_area) + building_prop +
                                          pigs_density_cat + pigs_density_cat:log(farm_area) +
                                          ecoregion,  # <-- NEW FIXED EFFECT
                                        random =  ~(1|country) + (1|anifarm),
                                        family = MASS::negative.binomial(theta = 2),
                                        data = monthly_data)


AIC(monthly_mod$mer,  mod_ecoregion_rnd$mer, mod_ecoregion_fixed$mer)
summary(monthly_mod$gam)
summary(mod_ecoregion_rnd$gam)
summary(mod_ecoregion_fixed$gam)





# table ----

tbl_regression(monthly_mod$gam,exponentiate = TRUE)
tab_model(monthly_mod$gam)#, file="doc/Paper/results/tbl_mod4_gamm4_2025_12.doc")
tbl_regression(monthly_mod$gam)

# Response plots  ----
# ├  Age-Month ----

# Create new data frame for prediction
new_data <- expand.grid(
  month = seq(1, 12, length.out = 100),
  sex = c("m", "f"),
  pigs_density_cat="[5,50)",
  forest_cover = mean(monthly_data$forest_cover),
  np_1km = mean(monthly_data$np_1km),
  enet_gpkg_1km_mean = mean(monthly_data$enet_gpkg_1km_mean),
  hfp19_mean_1km = mean(monthly_data$hfp19_mean_1km),
  dist_to_farm = mean(monthly_data$dist_to_farm),
  building_prop = mean(monthly_data$building_prop),
  farm_area = mean(monthly_data$farm_area),
  distance_river = mean(monthly_data$distance_river),
  distForest = mean(monthly_data$distForest),
  distCulture = mean(monthly_data$distCulture),
  swf_mean_100m= mean(monthly_data$swf_mean_100m),
  pigs_density= mean(monthly_data$pigs_density)
)

# Generate predictions with confidence intervals
predictions <- predict(monthly_mod$gam, newdata = new_data, type = "link", se.fit = TRUE)

# Create a data frame with predictions and standard errors

pred_data <- cbind(new_data, fit = predictions$fit, se.fit = predictions$se.fit) %>%
  mutate(sex = case_when(sex=='m'~'males',
                         sex=='f'~'females')) %>%
  mutate(fit_exp = exp(fit),
         lower_ci = exp(fit - 1.96 * se.fit),
         upper_ci = exp(fit + 1.96 * se.fit))

ggplot(pred_data, aes(x = month, y = fit_exp, color = sex, fill=sex)) + #, color = age
  geom_line(size=1.2) +
  geom_ribbon(
    data = subset(pred_data),
    aes(ymin = lower_ci, ymax = upper_ci), #, fill = age
    alpha = 0.2, linetype = 0
  ) +
  scale_x_continuous(
    breaks = seq_along(month.abb),
    labels = month.abb,
    expand = c(0, 0)
  ) +
  labs(title = NULL,
       x = NULL,
       y = "Predicted monthly contact rate") +
  scale_color_viridis_d(option = "D", begin = 0.1, end = 0.7) +
  scale_fill_viridis_d(option = "D", begin = 0.1, end = 0.7) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.x = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = c(0.01, 0.99),  # Setting the legend position
    legend.justification = c(0, 1),
    legend.title = element_blank(),
    plot.margin = unit(c(1, 1, 1.5, 1), "lines")# Removing the legend title
  )

# ├  Forest cover ----


# Create new data frames for prediction
new_data <- expand.grid(
  forest_cover = seq(min(monthly_data$forest_cover), max(monthly_data$forest_cover), length.out = 100),
  month = 8,
  sex = "m",
  #age = "adult",
  #forest_cover = mean(df3$forest_cover),
  np_1km = mean(monthly_data$np_1km),
  enet_gpkg_1km_mean = mean(monthly_data$enet_gpkg_1km_mean),
  hfp19_mean_1km = mean(monthly_data$hfp19_mean_1km),
  dist_to_farm = mean(monthly_data$dist_to_farm),
  building_prop = mean(monthly_data$building_prop),
  farm_area = mean(monthly_data$farm_area),
  distance_river = mean(monthly_data$distance_river),
  distForest = mean(monthly_data$distForest),
  distCulture = mean(monthly_data$distCulture),
  swf_mean_100m= mean(monthly_data$swf_mean_100m),
  pigs_density= mean(monthly_data$pigs_density),
  pigs_density_cat="[5,50)"
)

# Generate predictions for distForest1ha
pred_forest_cover <- predict(monthly_mod$gam, newdata = new_data, type = "link", se.fit = TRUE)
new_data$fit <- exp(pred_forest_cover$fit)
new_data$lower <- exp(pred_forest_cover$fit - 1.96 * pred_forest_cover$se.fit)
new_data$upper <- exp(pred_forest_cover$fit + 1.96 * pred_forest_cover$se.fit)


# Plot
p1 <- ggplot(new_data, aes(x = forest_cover*100, y = fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_rug(data = monthly_data, aes(x = forest_cover*100, y = contact_median_round), color = "black", sides = "b") +
  labs(title = NULL,
       x = "Forest cover (%)",
       y = "Predicted monthly contact rate") +
  #ylim(c(0,12)) +
  scale_y_continuous(limits=c(0, 3)) + #, breaks = c(2, 4, 6, 8, 10) ) +
  annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1.5, fontface = "bold", size = 5) +
  theme_bw() +
  theme(panel.border = element_blank(), axis.line = element_line(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12) )
p1

# ├  Number of patches 1 km ----

# Create new data frames for prediction
new_data <- expand.grid(
  forest_cover = mean(monthly_data$forest_cover),
  month = 8,
  sex = "m",
  #age = "adult",
  #forest_cover = mean(df3$forest_cover),
  np_1km = seq(min(monthly_data$np_1km), max(monthly_data$np_1km), length.out = 100),
  enet_gpkg_1km_mean = mean(monthly_data$enet_gpkg_1km_mean),
  hfp19_mean_1km = mean(monthly_data$hfp19_mean_1km),
  dist_to_farm = mean(monthly_data$dist_to_farm),
  building_prop = mean(monthly_data$building_prop),
  farm_area = mean(monthly_data$farm_area),
  distance_river = mean(monthly_data$distance_river),
  distForest = mean(monthly_data$distForest),
  distCulture = mean(monthly_data$distCulture),
  swf_mean_100m= mean(monthly_data$swf_mean_100m),
  pigs_density= mean(monthly_data$pigs_density),
  pigs_density_cat="[5,50)"
)

# Generate predictions for distForest1ha
pred_np_1km <- predict(gamm4_final$gam, newdata = new_data, type = "link", se.fit = TRUE)
new_data$fit <- exp(pred_np_1km$fit)
new_data$lower <- exp(pred_np_1km$fit - 1.96 * pred_np_1km$se.fit)
new_data$upper <- exp(pred_np_1km$fit + 1.96 * pred_np_1km$se.fit)


# Plot

p2 <- ggplot(new_data, aes(x = np_1km, y = fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_rug(data = monthly_data, aes(x = np_1km, y = contact_median_round), color = "black", sides = "b") +
  labs(title = NULL,
       x = "Number of forest patches",
       y = NULL) +
  #ylim(c(0,12)) +
  scale_y_continuous(limits=c(0, 3)) + #, breaks = c(2, 4, 6, 8, 10) ) +
  annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1.5, fontface = "bold", size = 5) +
  theme_bw() +
  theme(panel.border = element_blank(), axis.line = element_line(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12) )
p2
# ├  Human foot print -----
# Create new data frames for prediction
new_data <- expand.grid(
  hfp19_mean_1km = seq(min(monthly_data$hfp19_mean_1km), max(monthly_data$hfp19_mean_1km), length.out = 100),
  month = 8,
  sex = "m",
  #age = "adult",
  #forest_cover = mean(df3$forest_cover),
  np_1km = mean(monthly_data$np_1km),
  enet_gpkg_1km_mean = mean(monthly_data$enet_gpkg_1km_mean),
  forest_cover = mean(monthly_data$forest_cover),
  dist_to_farm = mean(monthly_data$dist_to_farm),
  building_prop = mean(monthly_data$building_prop),
  farm_area = mean(monthly_data$farm_area),
  distance_river = mean(monthly_data$distance_river),
  distForest = mean(monthly_data$distForest),
  distCulture = mean(monthly_data$distCulture),
  swf_mean_100m= mean(monthly_data$swf_mean_100m),
  pigs_density= mean(monthly_data$pigs_density),
  pigs_density_cat="[5,50)"
)
#new_data$cultivated_areas <- seq(min(monthly_data$cultivated_areas), max(monthly_data$cultivated_areas), length.out = 100)
pred_hfp19_mean_1km <- predict(gamm4_final$gam, newdata = new_data, type = "link", se.fit = TRUE)
new_data$fit <- exp(pred_hfp19_mean_1km$fit)
new_data$lower <- exp(pred_hfp19_mean_1km$fit - 1.96 * pred_hfp19_mean_1km$se.fit)
new_data$upper <- exp(pred_hfp19_mean_1km$fit + 1.96 * pred_hfp19_mean_1km$se.fit)
#new_data$hfp19_mean_1km_orig <- datawizard::unstandardize(new_data$hfp19_mean_1km, reference=df$hfp19_mean_1km)

p3 <- ggplot(new_data, aes(x = hfp19_mean_1km, y = fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_rug(data = monthly_data, aes(x = hfp19_mean_1km, y = contact_median_round), color = "black", sides = "b") +
  labs(title = NULL,
       x = "Human Footprint Index",
       y = NULL) +
  #ylim(c(0,12)) +
  scale_y_continuous(limits=c(0, 3)) + #, breaks = c(2, 4, 6, 8, 10) ) +
  annotate("text", x = -Inf, y = Inf, label = "C", hjust = -0.5, vjust = 1.5, fontface = "bold", size = 5) +
  theme_bw() +
  theme(panel.border = element_blank(), axis.line = element_line(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12) )
p3
# ├  wild boar density -----
# Create new data frames for prediction
new_data <- expand.grid(
  enet_gpkg_1km_mean = seq(min(monthly_data$enet_gpkg_1km_mean), max(monthly_data$enet_gpkg_1km_mean), length.out = 100),
  month = 8,
  sex = "m",
  #age = "adult",
  #forest_cover = mean(df3$forest_cover),
  np_1km = mean(monthly_data$np_1km),
  hfp19_mean_1km = mean(monthly_data$hfp19_mean_1km),
  forest_cover = mean(monthly_data$forest_cover),
  dist_to_farm = mean(monthly_data$dist_to_farm),
  building_prop = mean(monthly_data$building_prop),
  farm_area = mean(monthly_data$farm_area),
  distance_river = mean(monthly_data$distance_river),
  distForest = mean(monthly_data$distForest),
  distCulture = mean(monthly_data$distCulture),
  swf_mean_100m= mean(monthly_data$swf_mean_100m),
  pigs_density= mean(monthly_data$pigs_density),
  pigs_density_cat="[5,50)"
)

pred_enet_gpkg_1km_mean <- predict(gamm4_final$gam, newdata = new_data, type = "link", se.fit = TRUE)
new_data$fit <- exp(pred_enet_gpkg_1km_mean$fit)
new_data$lower <- exp(pred_enet_gpkg_1km_mean$fit - 1.96 * pred_enet_gpkg_1km_mean$se.fit)
new_data$upper <- exp(pred_enet_gpkg_1km_mean$fit + 1.96 * pred_enet_gpkg_1km_mean$se.fit)
#new_data$enet_gpkg_1km_mean_orig <- datawizard::unstandardize(new_data$enet_gpkg_1km_mean, reference=df$enet_gpkg_1km_mean)

p4 <- ggplot(new_data, aes(x = enet_gpkg_1km_mean, y = fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_rug(data = monthly_data, aes(x = enet_gpkg_1km_mean, y = contact_median_round), color = "black", sides = "b") +
  ylim(c(0,2)) +
  labs(title = NULL,
       x = "Wild boar density (ind/km²)",
       y = "Predicted monthly contact rate") +
  annotate("text", x = -Inf, y = Inf, label = "D", hjust = -0.5, vjust = 1.5, fontface = "bold", size = 5) +
  theme_bw() +
  theme(panel.border = element_blank(), axis.line = element_line(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12) )
p4


# ├ Distance to farms -----
new_data <- expand.grid(
  dist_to_farm = seq(min(monthly_data$dist_to_farm), max(monthly_data$dist_to_farm), length.out = 100),  # Raw scale
  month = 8,
  sex = "m",
  #age = "adult",
  np_1km = mean(monthly_data$np_1km),
  hfp19_mean_1km = mean(monthly_data$hfp19_mean_1km),
  forest_cover = mean(monthly_data$forest_cover),
  enet_gpkg_1km_mean = mean(monthly_data$enet_gpkg_1km_mean),
  building_prop = mean(monthly_data$building_prop),
  farm_area = mean(monthly_data$farm_area),  # Already log-transformed
  distCulture = mean(monthly_data$distCulture),
  pigs_density = mean(monthly_data$pigs_density),
  pigs_density_cat="[5,50)"
)

# Predict using the model
pred_dist_to_farm <- predict(gamm4_final$gam, newdata = new_data, type = "link", se.fit = TRUE)

# Convert predictions back to the response scale
new_data$fit <- exp(pred_dist_to_farm$fit)  # Convert from link (log scale) to response scale
new_data$lower <- exp(pred_dist_to_farm$fit - 1.96 * pred_dist_to_farm$se.fit)
new_data$upper <- exp(pred_dist_to_farm$fit + 1.96 * pred_dist_to_farm$se.fit)

# Plot on the original (non-log) scale of dist_to_farm

p5 <- ggplot(new_data, aes(x = dist_to_farm, y = fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_rug(data = monthly_data, aes(x = dist_to_farm, y = contact_median_round),
           color = "black", sides = "b", alpha = 1/2, position = "jitter") +
  #ylim(c(0, 10)) +
  labs(title = NULL,
       x = "Distance to farm (m, log scale)",
       y = "Predicted monthly contact (log scale)") +
  annotate("text", x = 100, y = 95, label = "E", fontface = "bold", size = 5) +
  theme_bw() +
  theme(panel.border = element_blank(), axis.line = element_line(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12)) +
  scale_x_log10(labels = scales::comma)+
  scale_y_log10(labels = scales::comma)
p5


# ├  Farm area -----

new_data <- expand.grid(
  farm_area = seq(min(monthly_data$farm_area), max(monthly_data$farm_area), length.out = 100),
  month = 8,
  sex = "m",
  #age = "adult",
  #forest_cover = mean(df3$forest_cover),
  np_1km = mean(monthly_data$np_1km),
  hfp19_mean_1km = mean(monthly_data$hfp19_mean_1km),
  forest_cover = mean(monthly_data$forest_cover),
  enet_gpkg_1km_mean = mean(monthly_data$enet_gpkg_1km_mean),
  building_prop = mean(monthly_data$building_prop),
  dist_to_farm = mean(monthly_data$dist_to_farm),
  distance_river = mean(monthly_data$distance_river),
  distForest = mean(monthly_data$distForest),
  distCulture = mean(monthly_data$distCulture),
  swf_mean_100m= mean(monthly_data$swf_mean_100m),
  pigs_density= mean(monthly_data$pigs_density),
  pigs_density_cat="[5,50)"
)

pred_farm_area <- predict(gamm4_final$gam, newdata = new_data, type = "link", se.fit = TRUE)

new_data$fit <- exp(pred_farm_area$fit)
new_data$lower <- exp(pred_farm_area$fit - 1.96 * pred_farm_area$se.fit)
new_data$upper <- exp(pred_farm_area$fit + 1.96 * pred_farm_area$se.fit)
#new_data <- new_data %>%  filter(farm_area < 100000)
#new_data$farm_area_orig <- datawizard::unstandardize(new_data$farm_area, reference=df$farm_area)
#new_data$farm_area_orig <- exp(new_data$farm_area_log)
p6 <- ggplot(new_data, aes(x = farm_area, y = fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_rug(data = monthly_data, aes(x = farm_area, y = contact_median_round), color = "black", sides = "b", position = "jitter") +
  ylim(c(0,6)) +  #xlim(c(0,10000)) +
  labs(title = NULL,
       x = "Farm area (m²)",
       y = "Predicted monthly contact rate") +
  annotate("text", x = -Inf, y = Inf, label = "F", hjust = -0.5, vjust = 1.5, fontface = "bold", size = 5) +
  theme_bw() +
  theme(panel.border = element_blank(), axis.line = element_line(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12)) +
  scale_x_log10(labels = scales::comma)+
  scale_y_log10(labels = scales::comma)
p6
# ├  farm_buildings -----
new_data <- expand.grid(
  building_prop = seq(min(monthly_data$building_prop), max(monthly_data$building_prop), length.out = 100),
  month = 8,
  sex = "m",
  #age = "adult",
  farm_area = mean(monthly_data$farm_area),
  np_1km = mean(monthly_data$np_1km),
  hfp19_mean_1km = mean(monthly_data$hfp19_mean_1km),
  forest_cover = mean(monthly_data$forest_cover),
  enet_gpkg_1km_mean = mean(monthly_data$enet_gpkg_1km_mean),
  #building_prop = mean(df3$building_prop),
  dist_to_farm = mean(monthly_data$dist_to_farm),
  pigs_density= mean(monthly_data$pigs_density),
  pigs_density_cat="[5,50)"
)

pred_building_prop <- predict(gamm4_final$gam, newdata = new_data, type = "link", se.fit = TRUE)
new_data$fit <- exp(pred_building_prop$fit)
new_data$lower <- exp(pred_building_prop$fit - 1.96 * pred_building_prop$se.fit)
new_data$upper <- exp(pred_building_prop$fit + 1.96 * pred_building_prop$se.fit)
#new_data$building_prop_orig <- datawizard::unstandardize(new_data$building_prop, reference=df$building_prop)
p7 <- ggplot(new_data, aes(x = building_prop*100, y = fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_rug(data = monthly_data, aes(x = building_prop*100, y = contact_median_round), color = "black", sides = "b", position = "jitter") +
  ylim(c(0,1)) + #xlim(c(0,10000)) +
  labs(title = NULL,
       x = "Farm building cover (%)",
       y = "Predicted monthly contact rate") +
  annotate("text", x = -Inf, y = Inf, label = "F", hjust = -0.5, vjust = 1.5, fontface = "bold", size = 5) +
  theme_bw() +
  theme(panel.border = element_blank(), axis.line = element_line(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12))
p7

# ├  Pigs density -----

new_data <- expand.grid(
  month = 8,#seq(1, 12, length.out = 100),
  sex = c("m"),
  #age = c("adult"),
  forest_cover = mean(monthly_data$forest_cover),
  np_1km = mean(monthly_data$np_1km),
  enet_gpkg_1km_mean = mean(monthly_data$enet_gpkg_1km_mean),
  hfp19_mean_1km = mean(monthly_data$hfp19_mean_1km),
  dist_to_farm = mean(monthly_data$dist_to_farm),
  building_prop = mean(monthly_data$building_prop),
  farm_area = seq(100, 200000, length.out = 100),
  pigs_density_cat= c("[0,5)","[5,50)","[50,1e+03)")
)

predictions <- predict(mod4_gamm4_sex$gam, newdata = new_data, type = "link", se.fit = TRUE)


pred_data <- cbind(new_data, fit = predictions$fit, se.fit = as.vector(predictions$se.fit)) %>%
  mutate(pigs_density = case_when(pigs_density_cat=='[0,5)'~'low',
                                  pigs_density_cat=='[5,50)'~'medium',
                                  pigs_density_cat=='[50,1e+03)'~'high'),
         pigs_density = factor(pigs_density, levels = c("low","medium","high"))) %>%
  mutate(fit_exp = exp(fit),
         lower_ci = exp(fit - 1.96 * se.fit),
         upper_ci = exp(fit + 1.96 * se.fit))


range(pred_data$farm_area)
p8 <- ggplot(data = pred_data,
       aes(x = farm_area, y = fit_exp, color = pigs_density,
           ymin = lower_ci, ymax = upper_ci, fill = pigs_density)) +
  geom_line() +  # Adjust line type and size based on significance
  geom_ribbon(alpha = 0.1, linetype = 0) +

  #facet_grid(~pigs_density)+
  scale_x_log10(labels = scales::comma)+
  scale_y_log10(labels = scales::comma)  +
  geom_rug(data = monthly_data, mapping = aes(x = farm_area), sides = "b", color = "black", alpha = 0.3,     inherit.aes = FALSE) +
  annotate("text", x = 100, y = 20, label = "G", fontface = "bold", size = 5) +
  theme_bw() +
  theme(
    legend.position = "top",
    panel.border = element_blank(), axis.line = element_line(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12)  # Increase size of facet labels
  ) +
  labs(
    title = NULL,
    x = "Farm Area (m², log scale)",
    y = "Predicted monthly contact rate (log scale)",
    color = "Pig density category (GLW Model)",  # Legend title for lines
    fill = "Pig density category (GLW Model)"   # Legend title for ribbons
  )
p8


library(patchwork)
# Set theme for annotations
thm <- theme(plot.title = element_text(face = 2, size = 14))

top_plot      <- wrap_elements(p1 +  p2 + p3 + plot_annotation(title = "Local environment variables (1-km buffer around farm)", theme = thm))
middle_plot   <- wrap_elements(p4 + p5 + plot_annotation(title = "Wild boar variables", theme = thm))
bottom_plot   <- wrap_elements(p7 + p8  + plot_annotation(title = "Farm variables", theme = thm))
#bottom_plot2   <- wrap_elements( p8  + plot_annotation(title = NULL, theme = thm))
top_plot / middle_plot / bottom_plot +
  plot_annotation(tag_levels = "A") # / bottom_plot2
thm <- theme(plot.title = element_text(face = 2, size = 14))

# Combine plots with section titles
combined_plot <- (
  (p1 + p2 + p3 + plot_annotation(title = "Environmental covariates", theme = thm)) /
    (p4 + p5 + plot_annotation(title = "Wild boar covariates", theme = thm)) /
    (p7 + p8 + plot_annotation(title = "Farm covariates", theme = thm))
) +
  plot_annotation(tag_levels = "A")  # Automatically add letters (A, B, C...)
# Combine all
combined_plot <- (top_plot / middle_plot / bottom_plot) #+ plot_annotation(tag_levels = "A")
# Display combined plot
combined_plot


# country effect ----
# Extract random effects
random_effects <- ranef(monthly_mod$mer, condVar = TRUE)  # Includes conditional variances
country_effects <- random_effects$country
country_effects_df <- as.data.frame(country_effects)
country_var <- attr(country_effects, "postVar")  # Variance information

# Extract the random effects for country and their confidence intervals
country_effects_df <- as.data.frame(country_effects) %>%
  rownames_to_column(var = "country") %>%  # Convert rownames into a column
  dplyr::rename(condval = `(Intercept)`) %>%
  mutate(
    condvar = country_var[1, 1, ],  # Variance for each country
    lower = condval - 1.96 * sqrt(condvar),  # Lower bound of 95% CI (log scale)
    upper = condval + 1.96 * sqrt(condvar),  # Upper bound of 95% CI (log scale)

    # Convert to RR (Relative Risk) by exponentiating
    RR = exp(condval),
    RR_lower = exp(lower),
    RR_upper = exp(upper)
  )

# Print the table
print(country_effects_df)
ggplot(country_effects_df, aes(x = reorder(country, condval), y = condval)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() +  # Flip coordinates for readability
  theme_minimal() +
  labs(
    title = NULL,
    x = NULL,
    y = "Random intercept Estimate (Country)"
  )

















