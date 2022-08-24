###############################################################################
##
## Script to all the analysis
##
## make.R
##
## 12/08/2022
##
## Camille Magneville
##
###############################################################################




# Clean the environnement:
rm(list = ls(all.names = TRUE), envir = .GlobalEnv)

# Install dependencies:
devtools::install_deps()


# Load the functions so make them available for use:
devtools::load_all()


# Load the analysis scripts:

# 1:
source(here::here("analysis/1_Clean_raw_data_and_sp_richn.R"))

# 2:
source(here::here("analysis/2_Traits_data.R"))

# 3:
source(here::here("analysis/3_Phylogenetic_data.R"))

# 4:
source(here::here("analysis/4_Exploratory_mFD_beta_FD.R"))

# 5:
source(here::here("analysis/5_Beta_TD_PD_sites.R"))

# 6:
source(here::here("analysis/6_Intra_days_accumul.R"))

# 7:
source(here::here("analysis/7_Inter_day_accum.R"))

# 8:
source(here::here("analysis/8_Rarity_Commonness.R"))

# 9
source(here::here("analysis/9_Beta_inter_intra_days.R"))

# 10
source(here::here("analysis/10_Lose_species_first.R"))









