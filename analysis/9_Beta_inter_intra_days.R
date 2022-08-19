###############################################################################
##
## Script to compute and plot beta diversities inter and intra days
##
## 9_Beta_inter_intra_days.R
##
## 19/08/2022
##
## Camille Magneville
##
###############################################################################



# Step 1: Call data ####


# TD and PD:
basic_accum_df <- readRDS(here::here("transformed_data", "basic_accumul_df.rds"))

# FD:
basic_fd_accum_df <- readRDS(here::here("transformed_data", "basic_FD_accum_df.rds"))
fe_faxes_coord <- readRDS(here::here("transformed_data", "fe_faxes_coord_5D.rds"))


# Step 2: Compute beta diversities avec save them ####


# TD:
rownames(basic_accum_df) <- basic_accum_df$vid_id
basic_accum_df <- basic_accum_df[, -c(151:154)]
basic_accum_df <- apply(basic_accum_df, 2, as.numeric)

beta_TD <- betapart::beta.pair(basic_accum_df, index.family = "jaccard")
saveRDS(beta_TD, here::here("transformed_data", "beta_TD_videos_dist.rds"))

# PD:

## Change 11 species names which are not in the fishtree phylogeny in the ...
## ... basic_accum_df:

basic_accum_df <- as.data.frame(basic_accum_df)
basic_accum_df <- dplyr::rename(basic_accum_df, "Gomphosus_varius" = "Gomphosus_caeruleus")
basic_accum_df <- dplyr::rename(basic_accum_df, "Scolopsis_bilineata" = "Scolopsis_frenata")
basic_accum_df <- dplyr::rename(basic_accum_df, "Scolopsis_bimaculata" = "Scolopsis_ghanam")
basic_accum_df <- dplyr::rename(basic_accum_df, "Cetoscarus_bicolor" = "Cetoscarus_ocellatus")
basic_accum_df <- dplyr::rename(basic_accum_df, "Scarus_altipinnis" = "Scarus_falcipinnis")
basic_accum_df <- dplyr::rename(basic_accum_df, "Scarus_oviceps" = "Scarus_scaber")
basic_accum_df <- dplyr::rename(basic_accum_df, "Tylosurus_crocodilus_crocodilus" = "Tylosurus_crocodilus")
basic_accum_df <- dplyr::rename(basic_accum_df, "Chlorurus_microrhinos" = "Chlorurus_strongylocephalus")
basic_accum_df <- dplyr::rename(basic_accum_df, "Canthigaster_coronata" = "Canthigaster_cyanospilota")
basic_accum_df <- dplyr::rename(basic_accum_df, "Labropsis_australis" = "Labropsis_xanthonota")
basic_accum_df <- dplyr::rename(basic_accum_df, "Ctenochaetus_striatus" = "Ac_Cten_dark")


sp_nm_all <- colnames(basic_accum_df)

phylo <- fishtree::fishtree_phylogeny(species = sp_nm_all)

# Compute beta PD:
beta_PD <- betapart::phylo.beta.pair(basic_accum_df, phylo, index.family = "jaccard")
saveRDS(beta_PD, here::here("transformed_data", "beta_PD_videos_dist.rds"))


# FD:
beta_FD <- mFD::beta.fd.multidim(
   sp_faxes_coord   = fe_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4", "PC5")],
   asb_sp_occ       = basic_fd,
   check_input      = TRUE,
   beta_family      = c("Jaccard"),
  details_returned = TRUE)
saveRDS(beta_FD, here::here("transformed_data", "beta_FD_videos_dist.rds"))



# Step 3: Compute 3 datatables which beta div values and pairs informations ####


# Here, we compute 3 dataframes: one for each diversity facets. Each contains ...
# ... sp1|sp2|value_beta|same_day|same_vid|same_site|site_nm columns (for FD sp = FE) ...
# ... if same_day = TRUE and same_video = FALSE -> intraday variation
# ... if same_day = FALSE and same_video = TRUE and site = same -> interday variation


beta_TD_df <- beta.video.clean.df(beta_TD$)
beta_PD_df <- beta.video.clean.df(beta_PD$)
beta_FD_df <- beta.video.clean.df(beta_FD$)

saveRDS(beta_TD_df, here::here("transformed_data", "beta_TD_videos_df.rds"))
saveRDS(beta_PD_df, here::here("transformed_data", "beta_PD_videos_df.rds"))
saveRDS(beta_FD_df, here::here("transformed_data", "beta_FD_videos_df.rds"))


# Step 4: Plot boxplots beta intra and inter days for each facet ####



