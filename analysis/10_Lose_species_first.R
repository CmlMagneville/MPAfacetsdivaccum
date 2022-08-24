###############################################################################
##
## Script to compute and plot the evolution of FD and PD when loosing ...
## ... rarest/most common/random species first
##
## 10_Lose_species_first.R
##
## 23/08/2022
##
## Camille Magneville
##
###############################################################################



# Step 1: Call data ####


rarcom_df <- readRDS(here::here("transformed_data/rarcom_df.rds"))
sp_faxes_coord <- readRDS(here::here("transformed_data/sp_faxes_coord.rds"))
day_asb_sp_df <- readRDS(here::here("transformed_data", "asb_sp_site_day.rds"))


# create one subset for N'Gouja:
rarcom_df_NG <- rarcom_df[which(rarcom_df$site == "N'Gouja"), ]

# create one subset for Boueni:
rarcom_df_B <- rarcom_df[which(rarcom_df$site == "Boueni"), ]


# Step 2: Compute N'Gouja species lost dataframe ####

asb_sp_site <- day_asb_sp_df

pal <- harrypotter::hp(n = 5, house = "Ravenclaw")
rarity_colors <- c(pal[3], "grey80", pal[4])
site_color <- "#80cdc1"

rarcom_df_site <- rarcom_df_NG

