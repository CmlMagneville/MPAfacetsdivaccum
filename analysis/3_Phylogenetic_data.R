###############################################################################
##
## Script to get the phylogeny of species seen in Mayotte and check ok
##
## 3_Phylogenetic_data.R
##
## 02/08/2022
##
## Camille Magneville
##
###############################################################################



# Step 1: Load the presence absence dataframes to get species names ####


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


# get species names:
sp_nm_all <- unique(c(colnames(presabs_03_A1), colnames(presabs_03_C2),
                      colnames(presabs_04_A1), colnames(presabs_04_C2),
                      colnames(presabs_05_A1), colnames(presabs_05_C2),
                      colnames(presabs_06_A1), colnames(presabs_06_C2),
                      colnames(presabs_08_A1), colnames(presabs_08_C2),
                      colnames(presabs_09_A1), colnames(presabs_09_C2)))

# remove time and video name info:
sp_nm_all <- sp_nm_all[which(! sp_nm_all %in% c("time", "video_nm"))]

# get sp name:
sort(sp_nm_all)
length(sp_nm_all)

# Ac_Cten_dark will not be found on phylogeny -> Ctenochaetus_striatus ...
# ... in fact according to expert diving there mostly that species:
sp_nm_all <- c(sp_nm_all, "Ctenochaetus_striatus")
sp_nm_all <- sp_nm_all[which(sp_nm_all != "Ac_Cten_dark")]


# Step 2: Get phylogenetic data ####


# Retrieve phylogeny:
phylo <- fishtree::fishtree_phylogeny(species = sp_nm_all)

# But 14 species are missing:
# To be able to replace them, we must find their sister species ...
# ... or check updated taxonomy:
# (https://docs.google.com/spreadsheets/d/1E7BiCuo6WLpZX44-Ko4N6F26ICvBFnrFkEz7BV1l-pc/edit?usp=sharing)

# So change the species accordingly:
# Gomphosus caeruleus -> Gomphosus varius (id pacifique)
# Scolopsis frenata -> Scolopsis bilineata (Seb Villeger)
# Scolopsis ghanam -> Scolopsis bimaculata (Seb Villeger)
# Cetoscarus ocellatus -> Cetoscarus bicolor (id mer rouge)
# Scarus falcipinnis -> Scarus alipinnis (id pacifique)

sp_nm_all <- c(sp_nm_all, "Gomphosus_varius")
sp_nm_all <- sp_nm_all[which(sp_nm_all != "Gomphosus_caeruleus")]

#sp_nm_all <- c(sp_nm_all, "Scolopsis_bilineata")
#sp_nm_all <- sp_nm_all[which(sp_nm_all != "Scolopsis_frenata")]

sp_nm_all <- c(sp_nm_all, "Scolopsis_bimaculata")
sp_nm_all <- sp_nm_all[which(sp_nm_all != "Scolopsis_ghanam")]

sp_nm_all <- c(sp_nm_all, "Cetoscarus_bicolor")
sp_nm_all <- sp_nm_all[which(sp_nm_all != "Cetoscarus_ocellatus")]

sp_nm_all <- c(sp_nm_all, "Scarus_altipinnis")
sp_nm_all <- sp_nm_all[which(sp_nm_all != "Scarus_falcipinnis")]


# Scarus scaber ->  Scarus oviceps (id pacifique)
# Tylosurus crocodilus -> Tylosurus crocodilus crocodilus
# Chlorurus strongylocephalus -> Chlorurus microrhinos (id pacifique)
# Coris variegata -> Coris batuensis (erreur variegata mer rouge, autre indien)
# Pomacentrus similis -> Pomacentrus caeruleus
# Canthigaster cyanospilota -> Canthigaster coronata


sp_nm_all <- c(sp_nm_all, "Scarus_oviceps")
sp_nm_all <- sp_nm_all[which(sp_nm_all != "Scarus_scaber")]

sp_nm_all <- c(sp_nm_all, "Tylosurus_crocodilus_crocodilus")
sp_nm_all <- sp_nm_all[which(sp_nm_all != "Tylosurus_crocodilus")]

#sp_nm_all <- c(sp_nm_all, "Chlorurus_microrhinos")
#sp_nm_all <- sp_nm_all[which(sp_nm_all != "Chlorurus_strongylocephalus")]

#sp_nm_all <- c(sp_nm_all, "Coris_batuensis")
#sp_nm_all <- sp_nm_all[which(sp_nm_all != "Coris_variegata")]

#sp_nm_all <- c(sp_nm_all, "Pomacentrus_caeruleus")
#sp_nm_all <- sp_nm_all[which(sp_nm_all != "Pomacentrus_similis")]

#sp_nm_all <- c(sp_nm_all, "Canthigaster_coronata")
#sp_nm_all <- sp_nm_all[which(sp_nm_all != "Canthigaster_cyanospilota")]


# Labropsis xanthonota -> Labropsis australis (esp Labropsis australienne)
sp_nm_all <- c(sp_nm_all, "Labropsis_australis")
sp_nm_all <- sp_nm_all[which(sp_nm_all != "Labropsis_xanthonota")]


# now ok?
phylo <- fishtree::fishtree_phylogeny(species = sp_nm_all)


# Step 3: Plot phylo ####

ape::plot.phylo(phylo, type = "fan")
