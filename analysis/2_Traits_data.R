###############################################################################
##
## Script to 1/ retrieve traits data, clean and save it using GASPAR data ...
## ... and 2/ explore and summarie traits data with mFD
##
## 2_Traits_data.R
##
## 02/08/2022
##
## Camille Magneville
##
###############################################################################



# Step 1: Load the GASPAR data and get the names of species seen in Mayotte ####
# Also load diet information from Parravicini et al. 2021 ProcB paper ...
# ... more precise so use these diet informations instead of GASPAR's


# GASPAR data is saved on two datasets: one gathering species taxonomic ...
# ... information and another one gathering traits linked with a species ID ...
# ... so we must link taxonomic name and traits


# Load taxonomic informations:
sp_taxo <- read.delim(here::here("raw_data", "sp_taxo_GASPAR.txt"), sep = "\t")
# Load traits data:
sp_tr <- read.delim(here::here("raw_data", "sp_traits_2014_MK_GASPAR.txt"), sep = "\t")

# Load diet information:
sp_diet <- read.csv(here::here("raw_data", "diet_Parravicini2.csv"))


# Load the presence-absence dfs (cleaned through ...
# ... 1_Clean_raw_data_and_sp_richn.R) so we can see if some species are ...
# ... missing in the GASPAR data and delete GASPAR species that we don't need:


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

# get species nb: 150
length(sp_nm_all)


# Step 2: Clean the GASPAR data (delete, add, merge) ####


# A/ MERGE:


# link the two tables based on row names of taxonomic table (sp identif):
## add species_id on columns:
sp_taxo <- tibble::rownames_to_column(sp_taxo, var = "species_id")
colnames(sp_tr)[1] <- "species_id"
## join:
sp_tr2 <- dplyr::right_join(sp_tr, sp_taxo, by = "species_id")

# remove unused columns:
sp_tr3 <- sp_tr2[, -c(1, 8, 9, 10, 11, 13)]

# remove duplicated rows:
sp_tr4 <- sp_tr3[! duplicated(sp_tr3), ] # one species was on two rows
rownames(sp_tr4) <- NULL


# B / COMPLETE AND REMOVE UNUSED SP:

# which species are seen in Mayotte and are not in the GASPAR database:
## (help: setdiff(x, y) : gives elements which are in x but not in y)
setdiff(sp_nm_all, sp_tr4$Latin_name)

# Miss: Ac_Cten_dark (ok) # "OM","S3","Sed", "Day", "SmallG", "Bottom", "Ac_Cten dark"
# Scolopsis frenata # SERF database: "IM", "S3", "Mob", "Day", "SmallG", "Bottom", "Scolopsis_frenata"
# Scarus ferrugineus # SERF db: "OM","S4","Mob", "Day", "Sol", "Bottom", "Scarus_ferrugineus"
# Scarus caudofasciatus # SERF db: "OM","S4","Mob", "Day", "LargeG", "Bottom", "Scarus_caudofasciatus"
# Cephalopholis nigripinnis # SERF db: "FC","S3","Sed", "Both", "Sol", "Bottom", "Cephalopholis_nigripinnis"

# Add missing species:

S_frenata <- c("IM", "S3", "Mob", "Day", "SmallG", "Bottom", "Scolopsis_frenata")
S_ferrugineus <- c("OM","S4","Mob", "Day", "Sol", "Bottom", "Scarus_ferrugineus")
S_caudofasciatus <- c("OM","S4","Mob", "Day", "LargeG", "Bottom", "Scarus_caudofasciatus")
C_nigripinnis <- c("FC","S3","Sed", "Both", "Sol", "Bottom", "Cephalopholis_nigripinnis")
Ac_Cten_dark <- c("OM","S3","Sed", "Day", "SmallG", "Bottom", "Ac_Cten_dark")

sp_tr4[nrow(sp_tr4) + 1, ] <- S_frenata
sp_tr4[nrow(sp_tr4) + 1, ] <- S_ferrugineus
sp_tr4[nrow(sp_tr4) + 1, ] <- S_caudofasciatus
sp_tr4[nrow(sp_tr4) + 1, ] <- C_nigripinnis
sp_tr4[nrow(sp_tr4) + 1, ] <- Ac_Cten_dark


# check that now every sp seen in Mayotte is in the traits db:
setdiff(sp_nm_all, sp_tr4$Latin_name) # ok !


# remove species which are not seen in Mayotte from the traits db:
remove_sp <- setdiff(sp_tr4$Latin_name, sp_nm_all)
sp_tr_final <- sp_tr4[which(sp_tr4$Latin_name %in% sp_nm_all), ]


# add trait data:

## rename the species column in the diet dataset so can use left_join:
sp_diet$Latin_name <- paste0(sp_diet$Genus, sep = "_", sp_diet$Species)

## join
sp_tr_final <- dplyr::left_join(sp_tr_final, sp_diet, by = "Latin_name")

## remove unused columns:
sp_tr_final <- sp_tr_final[, c(2:7, 19)]

## rename the diet column and others:
colnames(sp_tr_final)[ncol(sp_tr_final)] <- "Diets"
colnames(sp_tr_final)[3] <- "Activity"
colnames(sp_tr_final)[4] <- "Schooling"


## there are 8 species missing diet data: use the paper Parravicini et al 2020 ...
# ... and other sources (doris, fishbase) to complete:

# same as other Scolpsis sp.:
sp_tr_final[which(sp_tr_final$Latin_name == "Scolopsis_frenata"), "Diets"] <- "Microinvertivores"

# paper Parravicini et al 2020, Plos Biol:
sp_tr_final[which(sp_tr_final$Latin_name == "Scarus_ferrugineus"), "Diets"] <- "Herbivores Microvores Detritivores"
sp_tr_final[which(sp_tr_final$Latin_name == "Cephalopholis_nigripinnis"), "Diets"] <- "Piscivores"

# As C. stratus because main species seen:
sp_tr_final[which(sp_tr_final$Latin_name == "Ac_Cten_dark"), "Diets"] <- "Herbivores Microvores Detritivores"

# Fishbase:
sp_tr_final[which(sp_tr_final$Latin_name == "Tylosurus_crocodilus"), "Diets"] <- "Piscivores"
sp_tr_final[which(sp_tr_final$Latin_name == "Caesio_lunaris"), "Diets"] <- "Planktivores"

# several sources (fishbase, doris, Australian Museum) as several diet possible:
sp_tr_final[which(sp_tr_final$Latin_name == "Aulostomus_chinensis"), "Diets"] <- "Piscivores"
sp_tr_final[which(sp_tr_final$Latin_name == "Canthigaster_cyanospilota"), "Diets"] <- "sessile invertivores"


# order the traits:

## Diets:
class(sp_tr_final$Diets)
sp_tr_final$Diets[which(sp_tr_final$Diets == "Macroinvertivores")] <- "MacI"
sp_tr_final$Diets[which(sp_tr_final$Diets == "Planktivores")] <- "Pla"
sp_tr_final$Diets[which(sp_tr_final$Diets == "Herbivores Microvores Detritivores")] <- "HMD"
sp_tr_final$Diets[which(sp_tr_final$Diets == "Piscivores")] <- "Pis"
sp_tr_final$Diets[which(sp_tr_final$Diets == "Crustacivores")] <- "Cru"
sp_tr_final$Diets[which(sp_tr_final$Diets == "Microinvertivores")] <- "MicI"
sp_tr_final$Diets[which(sp_tr_final$Diets == "Corallivores")] <- "Cor"
sp_tr_final$Diets[which(sp_tr_final$Diets == "sessile invertivores")] <- "SI"
sp_tr_final$Diets <- as.factor(sp_tr_final$Diets)
class(sp_tr_final$Diets)

## Size Class:
class(sp_tr_final$Size_Class)
sp_tr_final$Size_Class <- ordered(sp_tr_final$Size_Class, levels = c("S2", "S3", "S4", "S5", "S6"))
class(sp_tr_final$Size_Class)
levels(sp_tr_final$Size_Class)

## Home range:
class(sp_tr_final$Home_Range)
sp_tr_final$Home_Range <- ordered(sp_tr_final$Home_Range, levels = c("Sed", "Mob", "VMob"))
class(sp_tr_final$Home_Range)
levels(sp_tr_final$Home_Range)

## Activity:
class(sp_tr_final$Activity)
sp_tr_final$Activity <- as.factor(sp_tr_final$Activity)
class(sp_tr_final$Activity)

## Schooling:
class(sp_tr_final$Schooling)
sp_tr_final$Schooling <- ordered(sp_tr_final$Schooling, levels = c("Sol", "Pair", "SmallG", "MedG", "LargeG"))
class(sp_tr_final$Schooling)
levels(sp_tr_final$Schooling)

## Level water:
class(sp_tr_final$Level_water)
sp_tr_final$Level_water <- ordered(sp_tr_final$Level_water, levels = c("Bottom", "Low", "High"))
class(sp_tr_final$Level_water)
levels(sp_tr_final$Level_water)

# put species in rows:
rownames(sp_tr_final) <- sp_tr_final$Latin_name
sp_tr_final <- sp_tr_final[, -6]

# save trait data for the 155 fish species seen in Mayotte:
saveRDS(sp_tr_final, here::here("transformed_data", "sp_tr_final.rds"))


# Build trait categories dataframe (needed in mFD):
trait_name <- c("Size_Class", "Home_Range", "Activity",
                "Schooling", "Level_water", "Diets")
trait_type <- c("O", "O", "N", "O", "O", "N")
trait_cat <- cbind(trait_name, trait_type)
trait_cat <- as.data.frame(trait_cat)

# save:
saveRDS(trait_cat, file = here::here("transformed_data", "tr_cat_df.rds"))


# Step 3: Traits summary with the mFD package ####


# Compute the summary:
traits_summ <- mFD::sp.tr.summary(
  tr_cat     = trait_cat,
  sp_tr      = sp_tr_final,
  stop_if_NA = TRUE)

traits_summ$tr_types
traits_summ$mod_list
traits_summ$tr_summary_list




