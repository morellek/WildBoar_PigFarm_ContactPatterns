##%######################################################%##
#                                                          #
#          Aim of this script is to provide                #
#    descriptive analysis of the contact data              #
#     across farms and wild boar (General patterns)        #
#                                                          #
##%######################################################%##

#libraries -----
library(here)
library(readr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(sf)
library(ggplot2)
library(ggforce)
library(Rmisc)
library(amt)
library(tidymv)
library(ineq)
library(rstatix)
library(flextable)
library(ineq)
# data import ----
contact_data <- readRDS("path/contact_data.rds")

# Some key number -----
length(unique(contact_data$individual.id)) # number of wild boar in final dataset
# ...with at least one contact
contact_data %>% mutate(contact_class=case_when(round(contact_mean,0) < 1 ~ "no contact",
                                                round(contact_mean,0) >= 1 ~"contact")) %>%
  group_by(contact_class) %>% tally()
# out of the 2091 individual-farm-month combination, 436 add at least one contact events detected
contact_data %>% filter(round(contact_mean,0) >= 1) %>% .$individual.id %>% unique() %>% length() # number of individual with at least one contact
# 85 individuals out of the 187 had at least one contact occurrence
# Number of farms ----
length(unique(contact_data$id_farm))
contact_data %>% filter(contact_median >= 1) %>% .$id_farm %>% unique() %>% length() # number of farms with at least one contact
# 91 farm out of the 457 had at least one contact occurrence

# Wild boar perspective ----
# ├  Contact rate with a given farm -----
contact_data
contact_given_farm <- contact_data %>% ungroup() %>%
  dplyr::summarize(
    mean_mean_contacts = mean(contact_mean),
    sd_mean_contacts = sd(contact_mean),
    proportion_zero = mean(contact_mean == 0)  # Proportion of zero-contact months
  )

print(contact_given_farm)
n <- nrow(contact_data)  # Number of individual-farm-months
mean_contacts <- 1.59
sd_contacts <- 6.05
se_contacts <- sd_contacts / sqrt(n)  # Standard error of the mean

# 95% Confidence interval
z <- 1.96
ci_lower <- mean_contacts - z * se_contacts
ci_upper <- mean_contacts + z * se_contacts

ci_lower
ci_upper

# ├  Contact rate with any farm (in the homerange) ----
contact_any_farms <- contact_data %>%
  group_by(individual.id, month_yr) %>%
  summarize(
    total_contacts_all_farms = sum(contact_mean),  # Sum contacts across all farms
    n_farms = n_distinct(id_farm),                  # Number of unique farms in the home range
    .groups = "drop"
  )
contact_any_farms <- contact_any_farms %>%
  mutate(
    average_contacts_per_farm = total_contacts_all_farms / n_farms  # Average contacts per farm
  )

contact_any_farms <- contact_any_farms  %>%
  dplyr::summarize(
    mean_mean_contacts = mean(average_contacts_per_farm),
    sd_mean_contacts = sd(average_contacts_per_farm),
    proportion_zero = mean(average_contacts_per_farm == 0)  # Proportion of zero-contact months
  )
print(contact_any_farms)
mean_contacts <- 3.43
sd_contacts <- 8.91
se_contacts <- sd_contacts / sqrt(n)  # Standard error of the mean

# 95% Confidence interval
z <- 1.96
ci_lower <- mean_contacts - z * se_contacts
ci_upper <- mean_contacts + z * se_contacts

ci_lower
ci_upper

#├ By country ----
contacts_per_boar <- contact_data %>%
  group_by(country, individual.id, month_yr) %>%
  summarise(
    n_farms_in_range = n_distinct(id_farm),
    n_contacted_farms = sum(contact_mean > 0),
    prop_farms_contacted = n_contacted_farms / n_farms_in_range,
    .groups = "drop"
  )

boar_summary <-contacts_per_boar %>% group_by(country) %>%
  dplyr::summarise(n=n(),
                   mean_n_farm_inHR = mean(n_farms_in_range, na.rm = TRUE),
                   sd_n_farm_inHR = sd(n_farms_in_range, na.rm = TRUE),
                   min_n_farm_inHR = min(n_farms_in_range, na.rm = TRUE),
                   max_n_farm_inHR = max(n_farms_in_range, na.rm = TRUE),
                   mean_prop_farm = mean(prop_farms_contacted),
                   median_prop_farm = median(prop_farms_contacted),
                   sd_prop_farm = sd(prop_farms_contacted)) %>% arrange(mean_prop_farm)

# Farms' perspective ----
contact_farm <- contact_data  %>% group_by(id_farm) %>% mutate(contact_mean_round=round(contact_mean,0)) %>%
  group_by(individual.id, id_farm) %>%
  summarise(
    ever_contact = any(contact_mean_round > 0, na.rm = TRUE),  # TRUE if at least one non-zero month
    mean_contact_farm = mean(contact_mean_round, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(id_farm) %>%
  dplyr::summarise(contactTot=sum(mean_contact_farm),
                   n_boars_visiting = n_distinct(individual.id),   # number of farms visited
                   meancontact=mean(mean_contact_farm),
                   medcontact=median(mean_contact_farm),
                   minContacts=min(mean_contact_farm),
                   maxContacts=max(mean_contact_farm),
                   n_total_boars = n_distinct(individual.id),
                   n_boars_contacting = sum(ever_contact),           # farms contacted at least once
                   n_boars_notcontacting = sum(!ever_contact),        # farms never contacted (all zeros)
                   gini_contact = ineq(mean_contact_farm, type = "Gini")) %>% arrange(desc(contactTot))  %>%
  mutate(contactCum=cumsum(contactTot),
         id_serial=row_number()) %>% #filter(contactTot > 0) %>%
  mutate(
    prop_contacting_boars = n_boars_contacting / n_total_boars,
    top99 = if_else(id_serial < 79, 1, 0),
    top75 = if_else(id_serial < 16, 1, 0),
    top50 = if_else(id_serial < 7, 1, 0)
  )
summary_farm <- contact_farm %>%
  arrange(desc(meancontact)) %>%
  summarize(
    mean_contacts = mean(meancontact, na.rm = TRUE),  # Mean total contacts across farms
    sd_contacts = sd(meancontact, na.rm = TRUE),      # SD of total contacts across farms
    n_farms = n(),                                         # Number of farms
    se_contacts = sd_contacts / sqrt(n_farms), # SE for mean
    ci_lower_contacts = mean_contacts - 1.96 * se_contacts,  # Lower 95% CI
    ci_upper_contacts = mean_contacts + 1.96 * se_contacts   # Upper 95% CI
  )
print(summary_farm)


# ├  country level ----
contacts_per_farm <- contact_data %>%
  group_by(country, id_farm, month_yr) %>%
  summarise(
    n_boars_present = n_distinct(individual.id),
    n_boars_contacted = sum(contact_mean > 0),
    prop_boars_contacting = n_boars_contacted / n_boars_present,
    .groups = "drop"
  ) %>% arrange(desc(n_boars_present))
farm_summary <- contacts_per_farm %>% group_by(country) %>%
  dplyr::summarise(n=n(),
                   mean_boars_present = mean(n_boars_present),
                   sd_boars_present = sd(n_boars_present, na.rm = TRUE),
                   min_boars_present = min(n_boars_present, na.rm = TRUE),
                   max_boars_present = max(n_boars_present, na.rm = TRUE),
                   mean_boars_contacted = mean(n_boars_contacted),
                   mean_prop_boars = mean(prop_boars_contacting),
                   median_prop_boars = median(prop_boars_contacting),
                   sd_prop_boars = sd(prop_boars_contacting)) %>% arrange(mean_prop_boars)

# Synthetic table -----
# ├  country level ----
synthetic_summary <- left_join(boar_summary, farm_summary, by = "country") %>%
  mutate(
    `Mean farms in boar HR` = sprintf("%.2f ± %.2f (%d–%d)",
                                      mean_n_farm_inHR, sd_n_farm_inHR,
                                      min_n_farm_inHR, max_n_farm_inHR),
    `Boar: contacted farms (%)` = sprintf("%.2f",
                                          mean_prop_farm),
    `Mean boars near farm` = sprintf("%.2f ± %.2f (%d–%d)",
                                     mean_boars_present, sd_boars_present,
                                     min_boars_present, max_boars_present),
    `Farm: contacting boars (%)` = sprintf("%.2f",
                                           mean_prop_boars)
  ) %>%
  dplyr::select(country,
         `Mean farms in boar HR`,
         `Boar: contacted farms (%)`,
         `Mean boars near farm`,
         `Farm: contacting boars (%)`)

synthetic_summary <- synthetic_summary %>%
  mutate(
    `Boar: contacted farms (%)` = round(as.numeric(sub(" ±.*", "", `Boar: contacted farms (%)`)) * 100, 1),
    `Farm: contacting boars (%)` = round(as.numeric(sub(" ±.*", "", `Farm: contacting boars (%)`)) * 100, 1))
# Compute overall summary
overall_summary <- synthetic_summary %>%
  mutate(across(where(is.character), ~ as.numeric(str_extract(., "^[0-9\\.]+")))) %>%
  summarise(
    `Mean farms in boar HR` = sprintf("%.2f ± %.2f",
                                      mean(`Mean farms in boar HR`, na.rm = TRUE),
                                      sd(`Mean farms in boar HR`, na.rm = TRUE)),
    `Boar: contacted farms (%)` = sprintf("%.0f",mean(`Boar: contacted farms (%)`, na.rm = TRUE)),
    `Mean boars near farm` = sprintf("%.2f ± %.2f",
                                     mean(`Mean boars near farm`, na.rm = TRUE),
                                     sd(`Mean boars near farm`, na.rm = TRUE)),
    `Farm: contacting boars (%)` = sprintf("%.0f",mean(`Farm: contacting boars (%)`, na.rm = TRUE))) %>%
  mutate(country = "Overall") %>%
  dplyr::select(country, everything())
synthetic_summary_with_overall <- bind_rows(
  synthetic_summary %>% mutate(across(everything(), as.character)),
  overall_summary %>% mutate(across(everything(), as.character))
)



ft <- flextable(synthetic_summary_with_overall) %>%
  set_header_labels(
    country = "Country",
    `Mean farms in boar HR` = "N farms in HR",
    `Boar: contacted farms (%)` = "Contacted farms (%)",
    `Mean boars near farm` = "N wild boars nearby",
    `Farm: contacting boars (%)` = "Contacting boars (%)"
  ) %>%
  add_header_row(
    values = c("", "Boar perspective", "Boar perspective", "Farm perspective", "Farm perspective"),
    colwidths = c(1, 1, 1, 1, 1)
  ) %>%
  align(align = "center", part = "all") %>%
  #ft <- flextable(synthetic_summary_with_overall) %>%
  bold(i = ~ country == "Overall", bold = TRUE) %>%
  autofit()
# Super-contact farms/individuals ----
# ├  Wild boar ----
individual_contact_summary <- contact_home_range %>%
  group_by(individual.id) %>%
  summarize(
    mean_contacts_all_farms = mean(total_contacts_all_farms),  # Mean monthly contacts (all farms)
    mean_contacts_per_farm = mean(average_contacts_per_farm),  # Mean monthly contacts per farm
    .groups = "drop"
  )

# Population-level summary with confidence intervals
population_summary_NoSuperContact <- individual_contact_summary %>%
  arrange(desc(mean_contacts_all_farms)) %>% slice_tail(n=-10) %>% # test for estimated when removing 10 boars accounting for the most contact (for the discussion)
  ungroup() %>%
  dplyr::summarize(
    mean_all_farms = mean(mean_contacts_all_farms, na.rm = TRUE),
    sd_all_farms = sd(mean_contacts_all_farms, na.rm = TRUE),
    se_all_farms = sd_all_farms / sqrt(n()),
    ci_lower_all_farms = mean_all_farms - 1.96 * se_all_farms,
    ci_upper_all_farms = mean_all_farms + 1.96 * se_all_farms,

    mean_per_farm = mean(mean_contacts_per_farm, na.rm = TRUE),
    sd_per_farm = sd(mean_contacts_per_farm, na.rm = TRUE),
    se_per_farm = sd_per_farm / sqrt(n()),
    ci_lower_per_farm = mean_per_farm - 1.96 * se_per_farm,
    ci_upper_per_farm = mean_per_farm + 1.96 * se_per_farm
  )


population_summary_tidy_NoSuperContact <- population_summary_NoSuperContact %>%
  pivot_longer(
    cols = everything(),  # Include all columns
    names_to = c("metric", ".value"),  # Split column names into "metric" and "value type"
    names_pattern = "(.*)_(all_farms|per_farm)"
  )

print(population_summary_tidy_NoSuperContact)


# now other way around: focus on the 10 individuals with most contacts
only_10Most <- individual_contact_summary %>%
  arrange(desc(mean_contacts_all_farms)) %>% slice_head(n=10) %>% # test for estimated when removing 5 boars accounting for the most contact (for the discussion)
  ungroup() %>%
  dplyr::summarize(
    mean_all_farms = mean(mean_contacts_all_farms, na.rm = TRUE),
    sd_all_farms = sd(mean_contacts_all_farms, na.rm = TRUE),
    se_all_farms = sd_all_farms / sqrt(n()),
    ci_lower_all_farms = mean_all_farms - 1.96 * se_all_farms,
    ci_upper_all_farms = mean_all_farms + 1.96 * se_all_farms,

    mean_per_farm = mean(mean_contacts_per_farm, na.rm = TRUE),
    sd_per_farm = sd(mean_contacts_per_farm, na.rm = TRUE),
    se_per_farm = sd_per_farm / sqrt(n()),
    ci_lower_per_farm = mean_per_farm - 1.96 * se_per_farm,
    ci_upper_per_farm = mean_per_farm + 1.96 * se_per_farm
  )

only_10Most_tidy <- only_10Most %>%
  pivot_longer(
    cols = everything(),  # Include all columns
    names_to = c("metric", ".value"),  # Split column names into "metric" and "value type"
    names_pattern = "(.*)_(all_farms|per_farm)"
  )
contact_ind <- contact_data %>% group_by(individual.id) %>% mutate(contact_mean_round=round(contact_mean,0)) %>%
  group_by(individual.id, id_farm) %>%
  summarise(
    ever_contact = any(contact_mean_round > 0, na.rm = TRUE),  # TRUE if at least one non-zero month
    mean_contact_farm = mean(contact_mean_round, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(individual.id) %>%
  summarise(
    contactTot = sum(mean_contact_farm, na.rm = TRUE),
    meancontact = mean(mean_contact_farm, na.rm = TRUE),
    medcontact = median(mean_contact_farm, na.rm = TRUE),
    minContacts = min(mean_contact_farm, na.rm = TRUE),
    maxContacts = max(mean_contact_farm, na.rm = TRUE),
    n_total_farms = n_distinct(id_farm),
    n_farms_contact = sum(ever_contact),           # farms contacted at least once
    n_farms_nocontact = sum(!ever_contact),        # farms never contacted (all zeros)
    gini_contact = ineq(mean_contact_farm, type = "Gini")
  ) %>%
  arrange(desc(contactTot))  %>%
  mutate(contactCum=cumsum(contactTot),
         id_serial=row_number()) %>% #filter(contactTot > 0) %>%
  ungroup() %>%
  mutate(
    prop_farms_contacted = n_farms_contact / n_total_farms,
    top99 = if_else(id_serial < 73, 1, 0),
    top75 = if_else(id_serial < 24, 1, 0),
    top50 = if_else(id_serial < 11, 1, 0)
  )

contact_ind %>% #filter(contactTot > 0) %>%
  group_by(top50) %>% #tally()
  summarise(
    mean_farms_total = mean(n_total_farms, na.rm = TRUE),
    median_farms_total = median(n_total_farms, na.rm = TRUE),
    sd_farms_total = sd(n_total_farms, na.rm = TRUE),
    mean_farms_contact = mean(n_farms_contact, na.rm = TRUE),
    median_farms_contact = median(n_farms_contact, na.rm = TRUE),
    sd_farms_contact = sd(n_farms_contact, na.rm = TRUE),
    mean_prop=mean(prop_farms_contacted),
    mean_gini = mean(gini_contact, na.rm = TRUE),
    sd_gini = sd(gini_contact, na.rm = TRUE),
    median_gini = median(gini_contact, na.rm = TRUE),
    n = n()
  )
# did top wild boar had more farm in their home range? --> NO!
wilcox.test(n_total_farms ~ top99, data = contact_ind)
wilcox.test(n_total_farms ~ top75, data = contact_ind)
wilcox.test(n_total_farms ~ top50, data = contact_ind)
# did top wild boar visited more farms? --> YES ! (but obious becasue they are the top wild boar...)
wilcox.test(n_farms_contact ~ top99, data = contact_ind)
wilcox.test(n_farms_contact ~ top75, data = contact_ind)
wilcox.test(n_farms_contact ~ top50, data = contact_ind)
# did they visit farm equally (gini score) --> YES ! (but obious becasue they are the top wild boar...)
wilcox.test(gini_contact ~ top99, data = contact_ind)
wilcox.test(gini_contact ~ top75, data = contact_ind)
wilcox.test(gini_contact ~ top50, data = contact_ind)

# ├  Farms -----

# Statistical test
contact_farm %>%
  group_by(top75) %>%
  dplyr::summarise(
    mean_boars = mean(n_boars_visiting, na.rm = TRUE),
    sd_boars = sd(n_boars_visiting, na.rm = TRUE),
    median_boars = median(n_boars_visiting, na.rm = TRUE),
    max_boars = max(n_boars_visiting, na.rm = TRUE),
    n = n()
  )
wilcox.test(n_boars_visiting ~ top99, data = contact_farm)
wilcox.test(n_boars_visiting ~ top75, data = contact_farm)
wilcox.test(n_boars_visiting ~ top50, data = contact_farm)


# Print the population-level summary
print(summary_farm)
print(summary_farm_No5Most)
 # Figures ----
#  ├ Figure S4 -----
# Calculate thresholds for 50%, 75%, and 99% of total contacts
total_contacts_farm <- max(contact_farm$contactCum)
thresholds_farm <- c(0.5, 0.75, 0.99) * total_contacts_farm
individuals_thresholds_farm <- sapply(thresholds_farm, function(th) {
  contact_farm %>% filter(contactCum >= th) %>% slice_head(n = 1) %>% pull(id_serial)
})

# Create the plot
p_farms <- ggplot(contact_farm, aes(x = id_serial, y = contactCum)) +
  # Line for cumulative contacts
  geom_line(color = "blue", size = 1) +
  # Points for individuals
  geom_point(size = 2, color = "red", alpha = 0.7) +
  # Shade for 0-50%
  geom_area(data = filter(contact_farm, id_serial <= individuals_thresholds_farm [1]),
            aes(x = id_serial, y = contactCum),
            fill = "grey40", alpha = 0.5) +
  # Shade for 50-75% (include the lower range for seamless overlap)
  geom_area(data = filter(contact_farm, id_serial <= individuals_thresholds_farm [2]),
            aes(x = id_serial, y = contactCum),
            fill = "grey60", alpha = 0.5) +
  # Shade for 75-99% (include the lower range for seamless overlap)
  geom_area(data = filter(contact_farm, id_serial <= individuals_thresholds_farm [3]),
            aes(x = id_serial, y = contactCum),
            fill = "grey80", alpha = 0.5) +
  # Annotations for thresholds
  annotate(
    "text",
    x = 2,  # Offset for better visibility
    y = thresholds_farm[1]/2,
    label = paste0(individuals_thresholds_farm [1], " farms (50%)"),
    color = "black", size = 6, hjust = 0
  ) +
  annotate(
    "text",
    x = 6,
    y = thresholds_farm[2]/2,
    label = paste0(individuals_thresholds_farm [2], " farms (75%)"),
    color = "black", size = 6, hjust = 0
  ) +
  annotate(
    "text",
    x = 20,
    y = thresholds_farm[3]/2,
    label = paste0(individuals_thresholds_farm [3], " farms (99%)"),
    color = "black", size = 6, hjust = 0
  ) +
  # Labels and titles
  labs(
    x = "Ranked farms",
    y = NULL#,
    #title = "Cumulative Distribution of Contacts Across Individuals"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(size = .5, color = "black"),  # Make axis lines thicker and black
    axis.title = element_text(size = 14),  # Larger, bold axis titles
    axis.text = element_text(size = 12),  # Larger axis tick labels
    plot.title = element_text(size = 16, hjust = 0.5),  # Title adjustments
    panel.grid.major = element_blank()  # Optionally remove gridlines
    #panel.grid.minor = element_blank()
  ) +
  # Remove padding on the x-axis to align with the y-axis
  scale_x_continuous(expand = c(0, 0)) +
  # Optionally, adjust y-axis to ensure the range is tight
  scale_y_continuous(expand = c(0, 0))

p_farms



# Calculate thresholds for 50%, 75%, and 99% of total contacts
total_contacts_boar <- max(contact_ind$contactCum)
thresholds_boar <- c(0.5, 0.75, 0.99) * total_contacts_boar
individuals_thresholds_boar <- sapply(thresholds_boar, function(th) {
  contact_ind %>% filter(contactCum >= th) %>% slice_head(n = 1) %>% pull(id_serial)
})

# Create the plot
p_ind <- ggplot(contact_ind, aes(x = id_serial, y = contactCum)) +
  # Line for cumulative contacts
  geom_line(color = "blue", size = 1) +
  # Points for individuals
  geom_point(size = 2, color = "red", alpha = 0.7) +
  # Shade for 0-50%
  geom_area(data = filter(contact_ind, id_serial <= individuals_thresholds_boar[1]),
            aes(x = id_serial, y = contactCum),
            fill = "grey40", alpha = 0.5) +
  # Shade for 50-75% (include the lower range for seamless overlap)
  geom_area(data = filter(contact_ind, id_serial <= individuals_thresholds_boar[2]),
            aes(x = id_serial, y = contactCum),
            fill = "grey60", alpha = 0.5) +
  # Shade for 75-99% (include the lower range for seamless overlap)
  geom_area(data = filter(contact_ind, id_serial <= individuals_thresholds_boar[3]),
            aes(x = id_serial, y = contactCum),
            fill = "grey80", alpha = 0.5) +
  # Annotations for thresholds
  annotate(
    "text",
    x = 3,  # Offset for better visibility
    y = thresholds_boar[1]/2,
    label = paste0(individuals_thresholds_boar[1], " individuals (50%)"),
    color = "black", size = 6, hjust = 0
  ) +
  annotate(
    "text",
    x = 8,
    y = thresholds_boar[2]/2,
    label = paste0(individuals_thresholds_boar[2], " individuals (75%)"),
    color = "black", size = 6, hjust = 0
  ) +
  annotate(
    "text",
    x = 20,
    y = thresholds_boar[3]/2,
    label = paste0(individuals_thresholds_boar[3], " individuals (99%)"),
    color = "black", size = 6, hjust = 0
  ) +
  # Labels and titles
  labs(
    x = "Ranked individuals",
    y = "Cumulative number of contacts"#,
    #title = "Cumulative Distribution of Contacts Across Individuals"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(size = .5, color = "black"),  # Make axis lines thicker and black
    axis.title = element_text(size = 14),  # Larger, bold axis titles
    axis.text = element_text(size = 12),  # Larger axis tick labels
    plot.title = element_text(size = 16, hjust = 0.5),  # Title adjustments
    panel.grid.major = element_blank()  # Optionally remove gridlines
    #panel.grid.minor = element_blank()
  ) +
  # Remove padding on the x-axis to align with the y-axis
  scale_x_continuous(expand = c(0, 0)) +
  # Optionally, adjust y-axis to ensure the range is tight
  scale_y_continuous(expand = c(0, 0))
# combining plots
library(patchwork)
p_ind + p_farms +
  plot_annotation(
    tag_levels = "A"  # Automatically labels plots with A, B, C, etc.
  )





# # Visit duration ------
# Summarize duration data
contact_events <- readRDS("path/contact_events.rds") %>%
  filter(id_month %in% unique(boars$id_month)) %>%  # make sure boars and contact data align
  arrange(individual.id, Start_date)
duration_summary <- contact_events %>%
  summarize(
    mean_duration = mean(Duration_mins, na.rm = TRUE),  # Mean duration
    sd_duration = sd(Duration_mins, na.rm = TRUE),      # Standard deviation of duration
    min_duration = min(Duration_mins, na.rm = TRUE),      # Standard deviation of duration
    max_duration = max(Duration_mins, na.rm = TRUE),      # Standard deviation of duration
    n_events = n(),                                     # Number of events
    se_duration = sd_duration / sqrt(n_events),         # Standard error of the mean
    ci_lower_duration = mean_duration - 1.96 * se_duration,  # Lower 95% CI
    ci_upper_duration = mean_duration + 1.96 * se_duration   # Upper 95% CI
  )

# Print the summary
print(duration_summary)



