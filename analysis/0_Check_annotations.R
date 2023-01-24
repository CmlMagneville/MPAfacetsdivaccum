################################################################################
##
## Script to check if some poorly annotated species are not misidentified
##
## 0_Check_annotations.R
##
## 18/11/2022
##
##
##
################################################################################



# Step 1: Call the rarcom_df ####

rar_com_df <- readRDS(here::here("transformed_data", "rarcom_df.rds"))


# Step 2: Get super rare species names ####

superrare_to_check <- unique(rar_com_df$species_nm[which(rar_com_df$rarity == "super rare")])


# Step 3: Get the presabs dataframe to build a dataframe with superrare species occurrences ####


presabs_03_A1 <- readRDS(here::here("raw_data", "pres_abs_vid_A1_03.rds"))
presabs_04_A1 <- readRDS(here::here("raw_data", "pres_abs_vid_A1_04.rds"))
presabs_05_A1 <- readRDS(here::here("raw_data", "pres_abs_vid_A1_05.rds"))
presabs_06_A1 <- readRDS(here::here("raw_data", "pres_abs_vid_A1_06.rds"))
presabs_08_A1 <- readRDS(here::here("raw_data", "pres_abs_vid_A1_08.rds"))
presabs_09_A1 <- readRDS(here::here("raw_data", "pres_abs_vid_A1_09.rds"))

presabs_03_C2 <- readRDS(here::here("raw_data", "pres_abs_vid_C2_03.rds"))
presabs_04_C2 <- readRDS(here::here("raw_data", "pres_abs_vid_C2_04.rds"))
presabs_05_C2 <- readRDS(here::here("raw_data", "pres_abs_vid_C2_05.rds"))
presabs_06_C2 <- readRDS(here::here("raw_data", "pres_abs_vid_C2_06.rds"))
presabs_08_C2 <- readRDS(here::here("raw_data", "pres_abs_vid_C2_08.rds"))
presabs_09_C2 <- readRDS(here::here("raw_data", "pres_abs_vid_C2_09.rds"))

# only keep videos from 7:30 to 17:00:

presabs_03_A1 <- presabs_03_A1[c(6:38), ] # 33 videos
presabs_03_C2 <- presabs_03_C2[c(5:37), ] # 33 videos

presabs_04_A1 <- presabs_04_A1[c(5:37), ] # 33 videos
presabs_04_C2 <- presabs_04_C2[c(3:35), ] # 33 videos

presabs_05_A1 <- presabs_05_A1[c(4:36), ] # 33 videos
presabs_05_C2 <- presabs_05_C2[c(2:34), ] # 33 videos

presabs_06_A1 <- presabs_06_A1[c(4:36), ] # 33 videos
presabs_06_C2 <- presabs_06_C2[c(2:35), ] # 34 videos -> complicated when pooling
# but no species on the last video:
apply(presabs_06_C2[, -c(50, 51)], 1, sum)
presabs_06_C2 <- presabs_06_C2[c(1:33), ]

presabs_08_A1 <- presabs_08_A1[c(4:37), ] # 34 videos -> complicated when pooling
# but no species on the last video:
apply(presabs_08_A1[, -c(57,58)], 1, sum)
presabs_08_A1 <- presabs_08_A1[c(1:33), ]

presabs_08_C2 <- presabs_08_C2[c(3:35), ] # 33 videos

presabs_09_A1 <- presabs_09_A1[c(6:38), ] # 33 videos
presabs_09_C2 <- presabs_09_C2[c(5:37), ] # 33 videos


# build the dataframe with the names of the videos on which the species occur:
dfs_list <- list(presabs_03_A1, presabs_03_C2, presabs_04_A1, presabs_04_C2,
                 presabs_05_A1, presabs_05_C2, presabs_06_A1, presabs_06_C2,
                 presabs_08_A1, presabs_08_C2, presabs_09_A1, presabs_09_C2)

sp_vect <- superrare_to_check

species_occ_check_df <- find.occurrences(dfs_list, sp_vect)
write.csv(species_occ_check_df, here::here("transformed_data", "0_check_annot.csv"))
