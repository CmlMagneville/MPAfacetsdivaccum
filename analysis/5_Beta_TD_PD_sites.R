###############################################################################
##
## Script to compute beta TD and PD (FD computed in 4_Exploratory_mFD_beta_FD.R)
##
## 5_Beta_TD_PD.R
##
## 08/08/2022
##
## Camille Magneville
##
###############################################################################


# Step 1: Gather dataframes from the same days (two cameras dataframes) ####


# Load presabs dataframes:

presabs_03_A1 <- readRDS(here::here("transformed_data", "pres_abs_vid_final_A1_03.rds"))
presabs_04_A1 <- readRDS(here::here("transformed_data", "pres_abs_vid_final_A1_04.rds"))
presabs_05_A1 <- readRDS(here::here("transformed_data", "pres_abs_vid_final_A1_05.rds"))
presabs_06_A1 <- readRDS(here::here("transformed_data", "pres_abs_vid_final_A1_06.rds"))
presabs_08_A1 <- readRDS(here::here("transformed_data", "pres_abs_vid_final_A1_08.rds"))
presabs_09_A1 <- readRDS(here::here("transformed_data", "pres_abs_vid_final_A1_09.rds"))

presabs_03_C2 <- readRDS(here::here("transformed_data", "pres_abs_vid_final_C2_03.rds"))
presabs_04_C2 <- readRDS(here::here("transformed_data", "pres_abs_vid_final_C2_04.rds"))
presabs_05_C2 <- readRDS(here::here("transformed_data", "pres_abs_vid_final_C2_05.rds"))
presabs_06_C2 <- readRDS(here::here("transformed_data", "pres_abs_vid_final_C2_06.rds"))
presabs_08_C2 <- readRDS(here::here("transformed_data", "pres_abs_vid_final_C2_08.rds"))
presabs_09_C2 <- readRDS(here::here("transformed_data", "pres_abs_vid_final_C2_09.rds"))


# Compte dfs for each day and save them:
NG_03 <- merge.cam.vid.df(list_df = list(presabs_03_A1, presabs_03_C2))
NG_05 <- merge.cam.vid.df(list_df = list(presabs_05_A1, presabs_05_C2))
NG_08 <- merge.cam.vid.df(list_df = list(presabs_08_A1, presabs_08_C2))
B_04 <- merge.cam.vid.df(list_df = list(presabs_04_A1, presabs_04_C2))
B_06 <- merge.cam.vid.df(list_df = list(presabs_06_A1, presabs_06_C2))
B_09 <- merge.cam.vid.df(list_df = list(presabs_09_A1, presabs_09_C2))

saveRDS(NG_03, here::here("transformed_data", "presabs_NG_03.rds"))
saveRDS(NG_05, here::here("transformed_data", "presabs_NG_05.rds"))
saveRDS(NG_08, here::here("transformed_data", "presabs_NG_08.rds"))
saveRDS(B_04, here::here("transformed_data", "presabs_B_04.rds"))
saveRDS(B_06, here::here("transformed_data", "presabs_B_06.rds"))
saveRDS(B_09, here::here("transformed_data", "presabs_B_09.rds"))


# Create the sp*asb df with asb being site_day:

list1 <- list(NG_03, NG_05, NG_08)
names(list1) <- c("NG_03", "NG_05", "NG_08")
list2 <- list(B_04, B_06, B_09)
names(list2) <- c("B_04", "B_06", "B_09")

asb_sp_df <- create.asb.sp.site(list1, list2)
saveRDS(asb_sp_df, here::here("transformed_data", "asb_sp_site_day.rds"))


# Create a sp*site dataframe with only N'Gouja and Boueni information:
site_asb_sp_df <- asb_sp_df
site_asb_sp_df[1, ] <- site_asb_sp_df[1, ] + site_asb_sp_df[2, ] +
  site_asb_sp_df[3, ]
site_asb_sp_df[4, ] <- site_asb_sp_df[4, ] + site_asb_sp_df[5, ] +
  site_asb_sp_df[6, ]
site_asb_sp_df <- site_asb_sp_df[-c(2, 3, 5, 6), ]
site_asb_sp_df[site_asb_sp_df  > 1] <- 1
rownames(site_asb_sp_df) <- c("N'Gouja", "Boueni")

# save the sp*site df:
saveRDS(site_asb_sp_df, here::here("transformed_data", "site_asb_sp_df.rds"))


# Step 2: Compute beta TD ####


beta_TD <- betapart::beta.pair(site_asb_sp_df, index.family = "jaccard")
beta_TD


# Step 3: Compute beta PD ####


# Change 11 species names which are not in the fishtree phylogeny in the ...
# ... site_asb_sp_df:

site_asb_sp_df <- dplyr::rename(site_asb_sp_df, "Gomphosus_varius" = "Gomphosus_caeruleus")
site_asb_sp_df <- dplyr::rename(site_asb_sp_df, "Scolopsis_bilineata" = "Scolopsis_frenata")
site_asb_sp_df <- dplyr::rename(site_asb_sp_df, "Scolopsis_bimaculata" = "Scolopsis_ghanam")
site_asb_sp_df <- dplyr::rename(site_asb_sp_df, "Cetoscarus_bicolor" = "Cetoscarus_ocellatus")
site_asb_sp_df <- dplyr::rename(site_asb_sp_df, "Scarus_altipinnis" = "Scarus_falcipinnis")
site_asb_sp_df <- dplyr::rename(site_asb_sp_df, "Scarus_oviceps" = "Scarus_scaber")
site_asb_sp_df <- dplyr::rename(site_asb_sp_df, "Tylosurus_crocodilus_crocodilus" = "Tylosurus_crocodilus")
site_asb_sp_df <- dplyr::rename(site_asb_sp_df, "Chlorurus_microrhinos" = "Chlorurus_strongylocephalus")
site_asb_sp_df <- dplyr::rename(site_asb_sp_df, "Canthigaster_coronata" = "Canthigaster_cyanospilota")
site_asb_sp_df <- dplyr::rename(site_asb_sp_df, "Labropsis_australis" = "Labropsis_xanthonota")
site_asb_sp_df <- dplyr::rename(site_asb_sp_df, "Ctenochaetus_striatus" = "Ac_Cten_dark")


sp_nm_all <- colnames(site_asb_sp_df)

phylo <- fishtree::fishtree_phylogeny(species = sp_nm_all)

# Compute beta PD:
beta_PD <- betapart::phylo.beta.pair(site_asb_sp_df, phylo, index.family = "jaccard")
beta_PD

# Compute PD for each site:
PD_values <- picante::pd(site_asb_sp_df, phylo, include.root=FALSE)
PD_values

# And compute relative values according to total PD of both sites:
site_asb_sp_df[nrow(site_asb_sp_df) + 1, ] <- rep(1, ncol(site_asb_sp_df))
PD_values_all <- picante::pd(site_asb_sp_df, phylo, include.root=FALSE)


# Compute PD for each day:
day_asb_sp_df <- readRDS(here::here("transformed_data", "asb_sp_site_day.rds"))

day_asb_sp_df <- dplyr::rename(day_asb_sp_df, "Gomphosus_varius" = "Gomphosus_caeruleus")
day_asb_sp_df <- dplyr::rename(day_asb_sp_df, "Scolopsis_bilineata" = "Scolopsis_frenata")
day_asb_sp_df <- dplyr::rename(day_asb_sp_df, "Scolopsis_bimaculata" = "Scolopsis_ghanam")
day_asb_sp_df <- dplyr::rename(day_asb_sp_df, "Cetoscarus_bicolor" = "Cetoscarus_ocellatus")
day_asb_sp_df <- dplyr::rename(day_asb_sp_df, "Scarus_altipinnis" = "Scarus_falcipinnis")
day_asb_sp_df <- dplyr::rename(day_asb_sp_df, "Scarus_oviceps" = "Scarus_scaber")
day_asb_sp_df <- dplyr::rename(day_asb_sp_df, "Tylosurus_crocodilus_crocodilus" = "Tylosurus_crocodilus")
day_asb_sp_df <- dplyr::rename(day_asb_sp_df, "Chlorurus_microrhinos" = "Chlorurus_strongylocephalus")
day_asb_sp_df <- dplyr::rename(day_asb_sp_df, "Canthigaster_coronata" = "Canthigaster_cyanospilota")
day_asb_sp_df <- dplyr::rename(day_asb_sp_df, "Labropsis_australis" = "Labropsis_xanthonota")
day_asb_sp_df <- dplyr::rename(day_asb_sp_df, "Ctenochaetus_striatus" = "Ac_Cten_dark")

# Compute PD for each site:
PD_values_day <- picante::pd(day_asb_sp_df, phylo, include.root=FALSE)
PD_values_day

# Test differences in PD and TD between sites at day scale:
NG_PD <- PD_values_day$PD[c(1:3)]
B_PD <- PD_values_day$PD[c(4:6)]
kruskal.test(NG_PD, B_PD)

NG_TD <- PD_values_day$SR[c(1:3)]
B_TD <- PD_values_day$SR[c(4:6)]
kruskal.test(NG_TD, B_TD)



