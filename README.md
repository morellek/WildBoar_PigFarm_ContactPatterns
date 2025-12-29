This archive contains R scripts and data files necessary to reproduce the main analyses presented in the manuscript 'Spatio-temporal patterns and risk factors of wild boar-pig farm contact across Europe'.

# Contents
## R Scripts

Rcode_GeneralPatterns.R
Performs exploratory analyses and visualizations of general contact patterns between wild boar and pig farms. Includes summaries at the individual and farm level, and identifies high-contact individuals/farms.

Rcode_Monthly_model.R
Runs the Generalized Additive Mixed Model (GAMM) to model monthly contact rates between wild boar and pig farms. This includes temporal smooths by sex and the set of wild boar, environmental and farm-level covariates.

Rcode_Hourly_model.R
Models the probability of contact at an hourly scale using binomial GAMMs. 

## Data Files

contact_data.rds
Monthly-level data linking individual wild boar, farms, and months. Contains metadata including individual ID, farm ID, month, and summary contact statistics.

contact_events.rds
Data on individual contact events derived from 5-min GPS fixes and simulated trajectories (via ctmm). Each row represents a discrete contact event, with duration, location, and timestamp.

monthly_data.rds
Input dataset used for the monthly GAMM. Includes the response variable (contact rate per individual–farm–month) and covariates such as farm area, land cover, wild boar density, distance to farm, etc.

hourly_data.rds
Input dataset for the hourly GAMM. Contains hourly contact data.
