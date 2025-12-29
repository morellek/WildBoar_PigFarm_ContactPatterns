# libraries ----
library(ggplot2)
library(mgcv)
library(ggeffects)
library(glmmTMB)
library(performance)
library(gamm4)
library('brms')
library(tidyverse)
library(DHARMa)
# data import ----

hourly_data <- readRDS("path/hourly_data.rds")
hourly_data$sex <- factor(hourly_data$sex)
hourly_data$month <- factor(hourly_data$month)

# model ----


hourly_mod <- gamm4::gamm4(contact_mean ~ s(hour_round, bs = "cc", k=24, by = sex) + sex,
                                    random =  ~(1|country/individual.id) + (1|month),
                                    family = negative.binomial(2),
                                    data = hourly_data )
summary(hourly_data$gam)



# Response plots  ----

# Create new data frame for prediction
new_data <- expand.grid(
  hour_round = seq(-19, 9, length.out = 100),
  sex = c("m", "f")
)

# Generate predictions with confidence intervals

predictions <- predict(hourly_mod$gam, newdata = new_data, type = "link", se.fit = TRUE) #mod25_gamm4_theta23

# Create a data frame with predictions and standard errors
pred_data <- cbind(new_data, fit = predictions$fit, se.fit = predictions$se.fit) %>%
  mutate(fit_exp = exp(fit),
         lower_ci = exp(fit - 1.96 * se.fit),
         upper_ci = exp(fit + 1.96 * se.fit))

pred_data$sex <- factor(pred_data$sex, levels = c("f", "m"), labels = c("females", "males"))
ggplot(pred_data, aes(x = hour_round, y = fit_exp, color = sex, fill = sex)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2, linetype = 0) +
  geom_vline(xintercept = 0, color = "red", size = 0.8) +


  scale_x_continuous(
    breaks = c(-20, -15, -10, -5, 0, 5, 10),
    limits = c(-19, 9),
    expand = c(0, 0)
  ) +
  labs(
    x = "hour relative to sunset",
    y = "Predicted hourly contact rate",
    color = NULL, fill = NULL
  ) +
  scale_color_viridis_d(option = "D", begin = 0.1, end = 0.7) +
  scale_fill_viridis_d(option = "D", begin = 0.1, end = 0.7) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    legend.position = c(.9,.9)
  )


# table
library(gtsummary)
hourly_mod %>% tbl_regression()

# country effect ----
# Extract random effects
random_effects <- ranef(hourly_mod$mer, condVar = TRUE)  # Includes conditional variances
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

