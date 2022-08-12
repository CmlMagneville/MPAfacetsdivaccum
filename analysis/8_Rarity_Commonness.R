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



# Step 3: Where are the rare/medium/common species in the functional sp for each site? ####


# Call data:

basic_fd_accum_df <- readRDS(here::here("transformed_data", "basic_FD_accum_df.rds"))

sp_tr <- readRDS(here::here("transformed_data/fe_tr.rds"))
tr_cat <- readRDS(here::here("transformed_data/tr_cat_df.rds"))

site_asb_fe_df <- readRDS(here::here("transformed_data/site_asb_fe_df.rds"))


# argument:
rarity_shapes <- c(24, 22, 21)
rarity_colors_NG <- c("#00441b", "#238b45", "#66c2a4")
rarity_colors_B <- c("#543005", "#8c510a", "#bf812d")
sites_colors <- c("#bf812d", "#80cdc1")

# Plot:
spot.rare.sp.fd(basic_fd_accum_df,
                            site_asb_fe_df,
                            sp_tr,
                            tr_cat,
                            rarcom_df,
                            rarity_shapes,
                            rarity_colors_B,
                            rarity_colors_NG,
                            sites_colors)
