###############################################################################
##
## Script to see rare/medium/common species in both sites and where they ...
## ... are in the functional space and the phylogenetic tree
##
## 8_Rarity_Commonness.R
##
## 12/08/2022
##
## Camille Magneville
##
###############################################################################


# Step 1: Call data ####


# load the basic_accum_df (created and saved in 6_Intra_days_accumul.R):
basic_accum_df <- readRDS(here::here("transformed_data", "basic_accumul_df.rds"))


# Step 2: Compute and plot the nb of videos in which each species occur #####


# Compute the dataframe with site, species_nm and vid_occ_nb and rarity info:
# > 50% common, 25-50 medium, < 25% rare:

rarcom_df <- rarcom.computation(basic_accum_df)
saveRDS(rarcom_df, here::here("transformed_data", "rarcom_df.rds"))


# Compute figures:

perc_rare_sp_NG <- (nrow(rarcom_df[which(rarcom_df$site == "N'Gouja" & rarcom_df$rarity == "rare"), ])/
  nrow(rarcom_df[which(rarcom_df$site == "N'Gouja"), ]))*100
perc_common_sp_NG <- (nrow(rarcom_df[which(rarcom_df$site == "N'Gouja" & rarcom_df$rarity == "common"), ])/
                      nrow(rarcom_df[which(rarcom_df$site == "N'Gouja"), ]))*100
perc_med_sp_NG <- (nrow(rarcom_df[which(rarcom_df$site == "N'Gouja" & rarcom_df$rarity == "medium"), ])/
                        nrow(rarcom_df[which(rarcom_df$site == "N'Gouja"), ]))*100

perc_rare_sp_B <- (nrow(rarcom_df[which(rarcom_df$site == "Boueni" & rarcom_df$rarity == "rare"), ])/
                      nrow(rarcom_df[which(rarcom_df$site == "Boueni"), ]))*100
perc_common_sp_B <- (nrow(rarcom_df[which(rarcom_df$site == "Boueni" & rarcom_df$rarity == "common"), ])/
                        nrow(rarcom_df[which(rarcom_df$site == "Boueni"), ]))*100
perc_med_sp_B <- (nrow(rarcom_df[which(rarcom_df$site == "Boueni" & rarcom_df$rarity == "medium"), ])/
                     nrow(rarcom[which(rarcom_df$site == "Boueni"), ]))*100

perc_unique_sp_NG <- (nrow(rarcom_df[which(rarcom_df$site == "N'Gouja" & rarcom_df$site_presence == "N'Gouja only"), ])/
                        nrow(rarcom_df[which(rarcom_df$site == "N'Gouja"), ]))*100

perc_unique_sp_B <- (nrow(rarcom_df[which(rarcom_df$site == "Boueni" & rarcom_df$site_presence == "Boueni only"), ])/
                        nrow(rarcom_df[which(rarcom_df$site == "Boueni"), ]))*100


# Plot (and save) for each site species occurrence:
sites_colors <- c("grey85", "#bf812d", "#80cdc1")

plot.rarcom(rarcom_df, sites_colors)




