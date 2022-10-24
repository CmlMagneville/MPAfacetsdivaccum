###############################################################################
##
## Script to know the data I am using and clean pres-abs df
## Annotations from 7:30 to 17:00 for 2 cameras 6 days/ sites 2 sites
##
## 1_Clean_raw_data_and_sp_richn.R
##
## 01/08/2022
##
## Camille Magneville
##
###############################################################################

# A - For dataat the video scale ####

# Step 1: call the 12 tables showing species occurrences in videos ####
## two cameras, 6 days / site, 2 sites

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


###


# Step 2: Clean data so only keep videos from 7:30 to 17:00 ####

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


###


# Step 3: Get all species names ####

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


# get species nb:
length(sp_nm_all)
# 164 but Chromis_NA, hortulanus_hortulanus (?) and ...
# ... Ac_Cten_dark = A. nigrofuscus + C. striatus + C. binotatus ...
# ... so only 160 species and 149 fish species (Chelonia mydas)


# get the nb of species on each site:

# MPA - N'Gouja:
sp_nm_NG <- unique(c(colnames(presabs_03_A1), colnames(presabs_03_C2),
                      colnames(presabs_05_A1), colnames(presabs_05_C2),
                      colnames(presabs_08_A1), colnames(presabs_08_C2)))

# remove time and video name info:
sp_nm_NG <- sp_nm_NG[which(! sp_nm_NG %in% c("time", "video_nm"))]
length(sp_nm_NG)
sort(sp_nm_NG)
# 134 fish species MPA

# Fished Area - Boueni:
sp_nm_B <- unique(c(colnames(presabs_04_A1), colnames(presabs_04_C2),
                      colnames(presabs_06_A1), colnames(presabs_06_C2),
                      colnames(presabs_09_A1), colnames(presabs_09_C2)))

# remove time and video name info:
sp_nm_B <- sp_nm_B[which(! sp_nm_B %in% c("time", "video_nm"))]
length(sp_nm_B)
sort(sp_nm_B)
# 119 fish species Fished Area


###


# Step 4 - Remove or merge columns ####

# Remove Chromis_NA and Chelonia_... , Octopus_cyanea, hortulanus_hortulanus (?) columns and ...
# ... merge A. nigrofuscus + C. striatus + C. binotatus into Ac_Cten_dark ...
# ... because can not be distinguished on videos ...
# ... and add Scarus sordidus and Chlorurus sordidus - keep C. sord (same species) ...
# ... Chlorurus spilurus: delete column as Pacific species (annot error) ...
# ... Neotrygon kuhlii: delete column (raie pastenague)
# ... Rename Halichoeres lamarii into Halichoeres marginatus (new name) ...
# ... Rename Coris variegata into Coris batuensis because variegata is endemic from red sea
# ... Add Pomacentrus similis and P. caeruleus as P. similis does not belong in Mayotte and ...
# ... data checked on WAVE
# ... Aulostomus_maculatus -> Aulostomus chinenesis as A. maculatus only in North America



# 03 - A1:

sort(colnames(presabs_03_A1))
presabs_03_A1 <- presabs_03_A1[, which(! colnames(presabs_03_A1) %in% c("Chelonia_mydas"))]
presabs_03_A1 <- presabs_03_A1[, which(! colnames(presabs_03_A1) %in% c("hortulanus_hortulanus"))]

presabs_03_A1$Ac_Cten_dark <- presabs_03_A1$Ac_Cten_dark + presabs_03_A1$Ctenochaetus_striatus
presabs_03_A1$Ac_Cten_dark[which(presabs_03_A1$Ac_Cten_dark > 1)] <- 1
presabs_03_A1 <- presabs_03_A1[, which(! colnames(presabs_03_A1) %in% c("Ctenochaetus_striatus"))]

presabs_03_A1$Chlorurus_sordidus <- presabs_03_A1$Chlorurus_sordidus +
                                            presabs_03_A1$Scarus_sordidus
presabs_03_A1$Chlorurus_sordidus[which(presabs_03_A1$Chlorurus_sordidus > 1)] <- 1
presabs_03_A1 <- presabs_03_A1[, which(! colnames(presabs_03_A1) %in% c("Scarus_sordidus"))]


# 03 - C2:

sort(colnames(presabs_03_C2))
presabs_03_C2 <- presabs_03_C2[, which(! colnames(presabs_03_C2) %in% c("Chelonia_mydas"))]
presabs_03_C2 <- presabs_03_C2[, which(! colnames(presabs_03_C2) %in% c("hortulanus_hortulanus"))]


# 04 - A1:

sort(colnames(presabs_04_A1))
presabs_04_A1 <- presabs_04_A1[, which(! colnames(presabs_04_A1) %in% c("Chelonia_mydas"))]


# 04 - C2:

sort(colnames(presabs_04_C2))
presabs_04_C2 <- presabs_04_C2[, which(! colnames(presabs_04_C2) %in% c("Chelonia_mydas"))]
presabs_04_C2 <- presabs_04_C2[, which(! colnames(presabs_04_C2) %in% c("Chromis_NA"))]
presabs_04_C2 <- dplyr::rename(presabs_04_C2, "Coris_batuensis" = "Coris_variegata")

presabs_04_C2$Ac_Cten_dark <- presabs_04_C2$Ac_Cten_dark +
                                  presabs_04_C2$Ctenochaetus_striatus +
                                  presabs_04_C2$Acanthurus_nigrofuscus +
                                  presabs_04_C2$Ctenochaetus_binotatus
presabs_04_C2$Ac_Cten_dark[which(presabs_04_C2$Ac_Cten_dark > 1)] <- 1
presabs_04_C2 <- presabs_04_C2[, which(! colnames(presabs_04_C2) %in% c("Ctenochaetus_striatus",
                                                                        "Acanthurus_nigrofuscus",
                                                                        "Ctenochaetus_binotatus"))]


# 05 - A1:
# Acanthurus tristis has been wrongly annontated (video BI fr 219): ...
# ... it's in fact A.nigricauda so I add the two columns and remove ...
# ... A. tristis one:
# ... and P. similis wrongly annotated -> P. caeruleus (similis not in Mayotte):

sort(colnames(presabs_05_A1))
presabs_05_A1 <- presabs_05_A1[, which(! colnames(presabs_05_A1) %in% c("Chelonia_mydas"))]
presabs_05_A1 <- presabs_05_A1[, which(! colnames(presabs_05_A1) %in% c("hortulanus_hortulanus"))]

presabs_05_A1 <- dplyr::rename(presabs_05_A1, "Pomacentrus_caeruleus" = "Pomacentrus_similis")

presabs_05_A1$Ac_Cten_dark <- presabs_05_A1$Ac_Cten_dark +
  presabs_05_A1$Acanthurus_nigrofuscus +
  presabs_05_A1$Ctenochaetus_binotatus
presabs_05_A1$Ac_Cten_dark[which(presabs_05_A1$Ac_Cten_dark > 1)] <- 1
presabs_05_A1 <- presabs_05_A1[, which(! colnames(presabs_05_A1) %in% c("Acanthurus_nigrofuscus",
                                                                        "Ctenochaetus_binotatus"))]
presabs_05_A1$Chlorurus_sordidus <- presabs_05_A1$Chlorurus_sordidus +
  presabs_05_A1$Scarus_sordidus
presabs_05_A1$Chlorurus_sordidus[which(presabs_05_A1$Chlorurus_sordidus > 1)] <- 1
presabs_05_A1 <- presabs_05_A1[, which(! colnames(presabs_05_A1) %in% c("Scarus_sordidus"))]

presabs_05_A1$Acanthurus_nigricauda <- presabs_05_A1$Acanthurus_nigricauda +
  presabs_05_A1$Acanthurus_tristis
presabs_05_A1$Acanthurus_nigricauda[which(presabs_05_A1$Acanthurus_nigricauda > 1)] <- 1
presabs_05_A1 <- presabs_05_A1[, which(! colnames(presabs_05_A1) %in% c("Acanthurus_tristis"))]


# 05 - C2:

sort(colnames(presabs_05_C2))
presabs_05_C2 <- presabs_05_C2[, which(! colnames(presabs_05_C2) %in% c("Chelonia_mydas"))]
presabs_05_C2 <- presabs_05_C2[, which(! colnames(presabs_05_C2) %in% c("Chlorurus_spilurus"))]


presabs_05_C2$Ac_Cten_dark <- presabs_05_C2$Ac_Cten_dark +
                                   presabs_05_C2$Ctenochaetus_striatus
presabs_05_C2$Ac_Cten_dark[which(presabs_05_C2$Ac_Cten_dark > 1)] <- 1
presabs_05_C2 <- presabs_05_C2[, which(! colnames(presabs_05_C2) %in% c("Ctenochaetus_striatus"))]


# 06 - A1

sort(colnames(presabs_06_A1))
presabs_06_A1$Chlorurus_sordidus <- presabs_06_A1$Chlorurus_sordidus +
  presabs_06_A1$Scarus_sordidus
presabs_06_A1$Chlorurus_sordidus[which(presabs_06_A1$Chlorurus_sordidus > 1)] <- 1
presabs_06_A1 <- presabs_06_A1[, which(! colnames(presabs_06_A1) %in% c("Scarus_sordidus"))]

presabs_06_A1$Aulostomus_chinensis <- presabs_06_A1$Aulostomus_chinensis +
  presabs_06_A1$Aulostomus_maculatus
presabs_06_A1$Aulostomus_chinensis[which(presabs_06_A1$Aulostomus_chinensis > 1)] <- 1
presabs_06_A1 <- presabs_06_A1[, which(! colnames(presabs_06_A1) %in% c("Aulostomus_maculatus"))]

# 06 - C2

sort(colnames(presabs_06_C2))
# nothing to change


# 08 - A1

sort(colnames(presabs_08_A1))
presabs_08_A1 <- presabs_08_A1[, which(! colnames(presabs_08_A1) %in% c("Chelonia_mydas"))]
presabs_08_A1 <- presabs_08_A1[, which(! colnames(presabs_08_A1) %in% c("hortulanus_hortulanus"))]
presabs_08_A1 <- presabs_08_A1[, which(! colnames(presabs_08_A1) %in% c("Octopus_cyanea"))]



# 08 - C2
# Scarus caudofasciatus to keep if find a sister species but yet no so remove:
# ... present in 3 video of this day/cam but no other places
sort(colnames(presabs_08_C2))
presabs_08_C2 <- presabs_08_C2[, which(! colnames(presabs_08_C2) %in% c("Chelonia_mydas"))]
presabs_08_C2 <- presabs_08_C2[, which(! colnames(presabs_08_C2) %in% c("hortulanus_hortulanus"))]
presabs_08_C2 <- presabs_08_C2[, which(! colnames(presabs_08_C2) %in% c("Scarus_caudofasciatus"))]


presabs_08_C2 <- dplyr::rename(presabs_08_C2, Halichoeres_marginatus = Halichoeres_lamarii)


# 09 - A1
# A. tristis has been wrongly annotated, it is in fact Acanthurus tennentii ...
# ... so I rename the column:

sort(colnames(presabs_09_A1))
presabs_09_A1 <- presabs_09_A1[, which(! colnames(presabs_09_A1) %in% c("Chelonia_mydas"))]
presabs_09_A1 <- presabs_09_A1[, which(! colnames(presabs_09_A1) %in% c("Octopus_cyanea"))]
presabs_09_A1 <- presabs_09_A1[, which(! colnames(presabs_09_A1) %in% c("Neotrygon_kuhlii"))]


presabs_09_A1$Ac_Cten_dark <- presabs_09_A1$Ac_Cten_dark +
                                 presabs_09_A1$Acanthurus_nigrofuscus +
                                 presabs_09_A1$Ctenochaetus_striatus
presabs_09_A1$Ac_Cten_dark[which(presabs_09_A1$Ac_Cten_dark > 1)] <- 1
presabs_09_A1 <- presabs_09_A1[, which(! colnames(presabs_09_A1) %in% c("Acanthurus_nigrofuscus",
                                                                        "Ctenochaetus_striatus"))]

presabs_09_A1 <- dplyr::rename(presabs_09_A1, Acanthurus_tennentii = Acanthurus_tristis)


# 09 - C2

sort(colnames(presabs_09_C2))
presabs_09_C2 <- presabs_09_C2[, which(! colnames(presabs_09_C2) %in% c("Chelonia_mydas"))]
presabs_09_C2 <- presabs_09_C2[, which(! colnames(presabs_09_C2) %in% c("hortulanus_hortulanus"))]
presabs_09_C2 <- presabs_09_C2[, which(! colnames(presabs_09_C2) %in% c("Octopus_cyanea"))]
presabs_09_C2 <- presabs_09_C2[, which(! colnames(presabs_09_C2) %in% c("Neotrygon_kuhlii"))]

presabs_09_C2 <- dplyr::rename(presabs_09_C2, Halichoeres_marginatus = Halichoeres_lamarii)
presabs_09_C2 <- dplyr::rename(presabs_09_C2, "Coris_batuensis" = "Coris_variegata")

presabs_09_C2$Ac_Cten_dark <- presabs_09_C2$Ac_Cten_dark +
  presabs_09_C2$Acanthurus_nigrofuscus +
  presabs_09_C2$Ctenochaetus_binotatus +
  presabs_09_C2$Ctenochaetus_striatus
presabs_09_C2$Ac_Cten_dark[which(presabs_09_C2$Ac_Cten_dark > 1)] <- 1
presabs_09_C2 <- presabs_09_C2[, which(! colnames(presabs_09_C2) %in% c("Ctenochaetus_striatus",
                                                                        "Acanthurus_nigrofuscus",
                                                                        "Ctenochaetus_binotatus"))]


# save cleaned data:
saveRDS(presabs_03_A1, here::here("transformed_data", "pres_abs_vid_final_A1_03.rds"))
saveRDS(presabs_03_C2, here::here("transformed_data", "pres_abs_vid_final_C2_03.rds"))
saveRDS(presabs_04_A1, here::here("transformed_data", "pres_abs_vid_final_A1_04.rds"))
saveRDS(presabs_04_C2, here::here("transformed_data", "pres_abs_vid_final_C2_04.rds"))
saveRDS(presabs_05_A1, here::here("transformed_data", "pres_abs_vid_final_A1_05.rds"))
saveRDS(presabs_05_C2, here::here("transformed_data", "pres_abs_vid_final_C2_05.rds"))
saveRDS(presabs_06_A1, here::here("transformed_data", "pres_abs_vid_final_A1_06.rds"))
saveRDS(presabs_06_C2, here::here("transformed_data", "pres_abs_vid_final_C2_06.rds"))
saveRDS(presabs_08_A1, here::here("transformed_data", "pres_abs_vid_final_A1_08.rds"))
saveRDS(presabs_08_C2, here::here("transformed_data", "pres_abs_vid_final_C2_08.rds"))
saveRDS(presabs_09_A1, here::here("transformed_data", "pres_abs_vid_final_A1_09.rds"))
saveRDS(presabs_09_C2, here::here("transformed_data", "pres_abs_vid_final_C2_09.rds"))



# Step 5: Get real nb of species on each site and total ####

## Total:
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

# get species nb: 150
length(sp_nm_all)


## MPA - N'Gouja:
sp_nm_NG <- unique(c(colnames(presabs_03_A1), colnames(presabs_03_C2),
                     colnames(presabs_05_A1), colnames(presabs_05_C2),
                     colnames(presabs_08_A1), colnames(presabs_08_C2)))

# remove time and video name info:
sp_nm_NG <- sp_nm_NG[which(! sp_nm_NG %in% c("time", "video_nm"))]

sort(sp_nm_NG)

# get species number: 127 species in MPA (Octopus cyanea only seen in NG)
# (Chelonia mydas seen on both sites)
length(sp_nm_NG)


## Fished Area - Boueni: (raie pastenague vue le 09 a Boueni, pas comprise dans les 113 sp)
sp_nm_B <- unique(c(colnames(presabs_04_A1), colnames(presabs_04_C2),
                    colnames(presabs_06_A1), colnames(presabs_06_C2),
                    colnames(presabs_09_A1), colnames(presabs_09_C2)))

# remove time and video name info:
sp_nm_B <- sp_nm_B[which(! sp_nm_B %in% c("time", "video_nm"))]

sort(sp_nm_B)

# get species number: 111 species in Fished Area
length(sp_nm_B)


# B - For data at the hour scale ####


# Step 1: call the 12 tables showing species occurrences in hours ####
## two cameras, 6 days / site, 2 sites

presabs_hour_03_A1 <- readRDS(here::here("raw_data", "pres_abs_hour_A1_03.rds"))
presabs_hour_04_A1 <- readRDS(here::here("raw_data", "pres_abs_hour_A1_04.rds"))
presabs_hour_05_A1 <- readRDS(here::here("raw_data", "pres_abs_hour_A1_05.rds"))
presabs_hour_06_A1 <- readRDS(here::here("raw_data", "pres_abs_hour_A1_06.rds"))
presabs_hour_08_A1 <- readRDS(here::here("raw_data", "pres_abs_hour_A1_08.rds"))
presabs_hour_09_A1 <- readRDS(here::here("raw_data", "pres_abs_hour_A1_09.rds"))

presabs_hour_03_C2 <- readRDS(here::here("raw_data", "pres_abs_hour_C2_03.rds"))
presabs_hour_04_C2 <- readRDS(here::here("raw_data", "pres_abs_hour_C2_04.rds"))
presabs_hour_05_C2 <- readRDS(here::here("raw_data", "pres_abs_hour_C2_05.rds"))
presabs_hour_06_C2 <- readRDS(here::here("raw_data", "pres_abs_hour_C2_06.rds"))
presabs_hour_08_C2 <- readRDS(here::here("raw_data", "pres_abs_hour_C2_08.rds"))
presabs_hour_09_C2 <- readRDS(here::here("raw_data", "pres_abs_hour_C2_09.rds"))


###


# Step 2 - Remove or merge columns ####

# Remove Chromis_NA and Chelonia_... , Octopus_cyanea, hortulanus_hortulanus (?) columns and ...
# ... merge A. nigrofuscus + C. striatus + C. binotatus into Ac_Cten_dark ...
# ... because can not be distinguished on videos ...
# ... and add Scarus sordidus and Chlorurus sordidus - keep C. sord (same species) ...
# ... Chlorurus spilurus: delete column as Pacific species (annot error) ...
# ... Neotrygon kuhlii: delete column (raie pastenague)
# ... Rename Halichoeres lamarii into Halichoeres marginatus (new name) ...
# ... Rename Coris variegata into Coris batuensis because variegata is endemic from red sea
# ... Add Pomacentrus similis and P. caeruleus as P. similis does not belong in Mayotte and ...
# ... data checked on WAVE
# ... Aulostomus_maculatus -> Aulostomus chinenesis as A. maculatus only in North America



# 03 - A1:

sort(colnames(presabs_hour_03_A1))
presabs_hour_03_A1 <- presabs_hour_03_A1[, which(! colnames(presabs_hour_03_A1) %in% c("Chelonia_mydas"))]
presabs_hour_03_A1 <- presabs_hour_03_A1[, which(! colnames(presabs_hour_03_A1) %in% c("hortulanus_hortulanus"))]

presabs_hour_03_A1$Ac_Cten_dark <- presabs_hour_03_A1$Ac_Cten_dark + presabs_hour_03_A1$Ctenochaetus_striatus
presabs_hour_03_A1$Ac_Cten_dark[which(presabs_hour_03_A1$Ac_Cten_dark > 1)] <- 1
presabs_hour_03_A1 <- presabs_hour_03_A1[, which(! colnames(presabs_hour_03_A1) %in% c("Ctenochaetus_striatus"))]

presabs_hour_03_A1$Chlorurus_sordidus <- presabs_hour_03_A1$Chlorurus_sordidus +
  presabs_hour_03_A1$Scarus_sordidus
presabs_hour_03_A1$Chlorurus_sordidus[which(presabs_hour_03_A1$Chlorurus_sordidus > 1)] <- 1
presabs_hour_03_A1 <- presabs_hour_03_A1[, which(! colnames(presabs_hour_03_A1) %in% c("Scarus_sordidus"))]


# 03 - C2:

sort(colnames(presabs_hour_03_C2))
presabs_hour_03_C2 <- presabs_hour_03_C2[, which(! colnames(presabs_hour_03_C2) %in% c("Chelonia_mydas"))]
presabs_hour_03_C2 <- presabs_hour_03_C2[, which(! colnames(presabs_hour_03_C2) %in% c("hortulanus_hortulanus"))]


# 04 - A1:

sort(colnames(presabs_hour_04_A1))
presabs_hour_04_A1 <- presabs_hour_04_A1[, which(! colnames(presabs_hour_04_A1) %in% c("Chelonia_mydas"))]


# 04 - C2:

sort(colnames(presabs_hour_04_C2))
presabs_hour_04_C2 <- presabs_hour_04_C2[, which(! colnames(presabs_hour_04_C2) %in% c("Chelonia_mydas"))]
presabs_hour_04_C2 <- presabs_hour_04_C2[, which(! colnames(presabs_hour_04_C2) %in% c("Chromis_NA"))]
presabs_hour_04_C2 <- dplyr::rename(presabs_hour_04_C2, "Coris_batuensis" = "Coris_variegata")

presabs_hour_04_C2$Ac_Cten_dark <- presabs_hour_04_C2$Ac_Cten_dark +
  presabs_hour_04_C2$Ctenochaetus_striatus +
  presabs_hour_04_C2$Acanthurus_nigrofuscus +
  presabs_hour_04_C2$Ctenochaetus_binotatus
presabs_hour_04_C2$Ac_Cten_dark[which(presabs_hour_04_C2$Ac_Cten_dark > 1)] <- 1
presabs_hour_04_C2 <- presabs_hour_04_C2[, which(! colnames(presabs_hour_04_C2) %in% c("Ctenochaetus_striatus",
                                                                        "Acanthurus_nigrofuscus",
                                                                        "Ctenochaetus_binotatus"))]


# 05 - A1:
# Acanthurus tristis has been wrongly annontated (video BI fr 219): ...
# ... it's in fact A.nigricauda so I add the two columns and remove ...
# ... A. tristis one:
# ... and P. similis wrongly annotated -> P. caeruleus (similis not in Mayotte):

sort(colnames(presabs_hour_05_A1))
presabs_hour_05_A1 <- presabs_hour_05_A1[, which(! colnames(presabs_hour_05_A1) %in% c("Chelonia_mydas"))]
presabs_hour_05_A1 <- presabs_hour_05_A1[, which(! colnames(presabs_hour_05_A1) %in% c("hortulanus_hortulanus"))]

presabs_hour_05_A1 <- dplyr::rename(presabs_hour_05_A1, "Pomacentrus_caeruleus" = "Pomacentrus_similis")

presabs_hour_05_A1$Ac_Cten_dark <- presabs_hour_05_A1$Ac_Cten_dark +
  presabs_hour_05_A1$Acanthurus_nigrofuscus +
  presabs_hour_05_A1$Ctenochaetus_binotatus
presabs_hour_05_A1$Ac_Cten_dark[which(presabs_hour_05_A1$Ac_Cten_dark > 1)] <- 1
presabs_hour_05_A1 <- presabs_hour_05_A1[, which(! colnames(presabs_hour_05_A1) %in% c("Acanthurus_nigrofuscus",
                                                                        "Ctenochaetus_binotatus"))]
presabs_hour_05_A1$Chlorurus_sordidus <- presabs_hour_05_A1$Chlorurus_sordidus +
  presabs_hour_05_A1$Scarus_sordidus
presabs_hour_05_A1$Chlorurus_sordidus[which(presabs_hour_05_A1$Chlorurus_sordidus > 1)] <- 1
presabs_hour_05_A1 <- presabs_hour_05_A1[, which(! colnames(presabs_hour_05_A1) %in% c("Scarus_sordidus"))]

presabs_hour_05_A1$Acanthurus_nigricauda <- presabs_hour_05_A1$Acanthurus_nigricauda +
  presabs_hour_05_A1$Acanthurus_tristis
presabs_hour_05_A1$Acanthurus_nigricauda[which(presabs_hour_05_A1$Acanthurus_nigricauda > 1)] <- 1
presabs_hour_05_A1 <- presabs_hour_05_A1[, which(! colnames(presabs_hour_05_A1) %in% c("Acanthurus_tristis"))]


# 05 - C2:

sort(colnames(presabs_hour_05_C2))
presabs_hour_05_C2 <- presabs_hour_05_C2[, which(! colnames(presabs_hour_05_C2) %in% c("Chelonia_mydas"))]
presabs_hour_05_C2 <- presabs_hour_05_C2[, which(! colnames(presabs_hour_05_C2) %in% c("Chlorurus_spilurus"))]


presabs_hour_05_C2$Ac_Cten_dark <- presabs_hour_05_C2$Ac_Cten_dark +
  presabs_hour_05_C2$Ctenochaetus_striatus
presabs_hour_05_C2$Ac_Cten_dark[which(presabs_hour_05_C2$Ac_Cten_dark > 1)] <- 1
presabs_hour_05_C2 <- presabs_hour_05_C2[, which(! colnames(presabs_hour_05_C2) %in% c("Ctenochaetus_striatus"))]


# 06 - A1

sort(colnames(presabs_hour_06_A1))
presabs_hour_06_A1$Chlorurus_sordidus <- presabs_hour_06_A1$Chlorurus_sordidus +
  presabs_hour_06_A1$Scarus_sordidus
presabs_hour_06_A1$Chlorurus_sordidus[which(presabs_hour_06_A1$Chlorurus_sordidus > 1)] <- 1
presabs_hour_06_A1 <- presabs_hour_06_A1[, which(! colnames(presabs_hour_06_A1) %in% c("Scarus_sordidus"))]

presabs_hour_06_A1$Aulostomus_chinensis <- presabs_hour_06_A1$Aulostomus_chinensis +
  presabs_hour_06_A1$Aulostomus_maculatus
presabs_hour_06_A1$Aulostomus_chinensis[which(presabs_hour_06_A1$Aulostomus_chinensis > 1)] <- 1
presabs_hour_06_A1 <- presabs_hour_06_A1[, which(! colnames(presabs_hour_06_A1) %in% c("Aulostomus_maculatus"))]

# 06 - C2

sort(colnames(presabs_hour_06_C2))
# nothing to change


# 08 - A1

sort(colnames(presabs_hour_08_A1))
presabs_hour_08_A1 <- presabs_hour_08_A1[, which(! colnames(presabs_hour_08_A1) %in% c("Chelonia_mydas"))]
presabs_hour_08_A1 <- presabs_hour_08_A1[, which(! colnames(presabs_hour_08_A1) %in% c("hortulanus_hortulanus"))]
presabs_hour_08_A1 <- presabs_hour_08_A1[, which(! colnames(presabs_hour_08_A1) %in% c("Octopus_cyanea"))]



# 08 - C2
# Scarus caudofasciatus to keep if find a sister species but yet no so remove:
# ... present in 3 video of this day/cam but no other places
sort(colnames(presabs_hour_08_C2))
presabs_hour_08_C2 <- presabs_hour_08_C2[, which(! colnames(presabs_hour_08_C2) %in% c("Chelonia_mydas"))]
presabs_hour_08_C2 <- presabs_hour_08_C2[, which(! colnames(presabs_hour_08_C2) %in% c("hortulanus_hortulanus"))]
presabs_hour_08_C2 <- presabs_hour_08_C2[, which(! colnames(presabs_hour_08_C2) %in% c("Scarus_caudofasciatus"))]


presabs_hour_08_C2 <- dplyr::rename(presabs_hour_08_C2, Halichoeres_marginatus = Halichoeres_lamarii)


# 09 - A1
# A. tristis has been wrongly annotated, it is in fact Acanthurus tennentii ...
# ... so I rename the column:

sort(colnames(presabs_hour_09_A1))
presabs_hour_09_A1 <- presabs_hour_09_A1[, which(! colnames(presabs_hour_09_A1) %in% c("Chelonia_mydas"))]
presabs_hour_09_A1 <- presabs_hour_09_A1[, which(! colnames(presabs_hour_09_A1) %in% c("Octopus_cyanea"))]
presabs_hour_09_A1 <- presabs_hour_09_A1[, which(! colnames(presabs_hour_09_A1) %in% c("Neotrygon_kuhlii"))]


presabs_hour_09_A1$Ac_Cten_dark <- presabs_hour_09_A1$Ac_Cten_dark +
  presabs_hour_09_A1$Acanthurus_nigrofuscus +
  presabs_hour_09_A1$Ctenochaetus_striatus
presabs_hour_09_A1$Ac_Cten_dark[which(presabs_hour_09_A1$Ac_Cten_dark > 1)] <- 1
presabs_hour_09_A1 <- presabs_hour_09_A1[, which(! colnames(presabs_hour_09_A1) %in% c("Acanthurus_nigrofuscus",
                                                                        "Ctenochaetus_striatus"))]

presabs_hour_09_A1 <- dplyr::rename(presabs_hour_09_A1, Acanthurus_tennentii = Acanthurus_tristis)


# 09 - C2

sort(colnames(presabs_hour_09_C2))
presabs_hour_09_C2 <- presabs_hour_09_C2[, which(! colnames(presabs_hour_09_C2) %in% c("Chelonia_mydas"))]
presabs_hour_09_C2 <- presabs_hour_09_C2[, which(! colnames(presabs_hour_09_C2) %in% c("hortulanus_hortulanus"))]
presabs_hour_09_C2 <- presabs_hour_09_C2[, which(! colnames(presabs_hour_09_C2) %in% c("Octopus_cyanea"))]
presabs_hour_09_C2 <- presabs_hour_09_C2[, which(! colnames(presabs_hour_09_C2) %in% c("Neotrygon_kuhlii"))]

presabs_hour_09_C2 <- dplyr::rename(presabs_hour_09_C2, Halichoeres_marginatus = Halichoeres_lamarii)
presabs_hour_09_C2 <- dplyr::rename(presabs_hour_09_C2, "Coris_batuensis" = "Coris_variegata")

presabs_hour_09_C2$Ac_Cten_dark <- presabs_hour_09_C2$Ac_Cten_dark +
  presabs_hour_09_C2$Acanthurus_nigrofuscus +
  presabs_hour_09_C2$Ctenochaetus_binotatus +
  presabs_hour_09_C2$Ctenochaetus_striatus
presabs_hour_09_C2$Ac_Cten_dark[which(presabs_hour_09_C2$Ac_Cten_dark > 1)] <- 1
presabs_hour_09_C2 <- presabs_hour_09_C2[, which(! colnames(presabs_hour_09_C2) %in% c("Ctenochaetus_striatus",
                                                                        "Acanthurus_nigrofuscus",
                                                                        "Ctenochaetus_binotatus"))]


# save cleaned data:
saveRDS(presabs_hour_03_A1, here::here("transformed_data", "pres_abs_hour_final_A1_03.rds"))
saveRDS(presabs_hour_03_C2, here::here("transformed_data", "pres_abs_hour_final_C2_03.rds"))
saveRDS(presabs_hour_04_A1, here::here("transformed_data", "pres_abs_hour_final_A1_04.rds"))
saveRDS(presabs_hour_04_C2, here::here("transformed_data", "pres_abs_hour_final_C2_04.rds"))
saveRDS(presabs_hour_05_A1, here::here("transformed_data", "pres_abs_hour_final_A1_05.rds"))
saveRDS(presabs_hour_05_C2, here::here("transformed_data", "pres_abs_hour_final_C2_05.rds"))
saveRDS(presabs_hour_06_A1, here::here("transformed_data", "pres_abs_hour_final_A1_06.rds"))
saveRDS(presabs_hour_06_C2, here::here("transformed_data", "pres_abs_hour_final_C2_06.rds"))
saveRDS(presabs_hour_08_A1, here::here("transformed_data", "pres_abs_hour_final_A1_08.rds"))
saveRDS(presabs_hour_08_C2, here::here("transformed_data", "pres_abs_hour_final_C2_08.rds"))
saveRDS(presabs_hour_09_A1, here::here("transformed_data", "pres_abs_hour_final_A1_09.rds"))
saveRDS(presabs_hour_09_C2, here::here("transformed_data", "pres_abs_hour_final_C2_09.rds"))


