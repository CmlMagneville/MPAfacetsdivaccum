###############################################################################
##
## Script to compute and plot the accumulation of the three facets ...
## ... through a day
##
## 6_Intra_days_accumul.R
##
## 03/08/2022
##
## Camille Magneville
##
###############################################################################



# Step 1: Call the presabs dataframe at a day scale (cameras pooled) (analysis > 4_Exploratory_mFD) ####


NG_03 <- readRDS(here::here("transformed_data" , "presabs_NG_03.rds"))
NG_05 <- readRDS(here::here("transformed_data" , "presabs_NG_05.rds"))
NG_08 <- readRDS(here::here("transformed_data" , "presabs_NG_08.rds"))

B_04 <- readRDS(here::here("transformed_data" , "presabs_B_04.rds"))
B_06 <- readRDS(here::here("transformed_data" , "presabs_B_06.rds"))
B_09 <- readRDS(here::here("transformed_data" , "presabs_B_09.rds"))


# Step 2: Create a dataframe with all information which will be used for TD, FD and PD ####


# Create a dataframe with species columns, days, video names and unique ID:

# argument for the function:
dfs_list <- list(NG_03, B_04, NG_05, B_06, NG_08, B_09)

days_vect <- c("03-11-2019", "04-11-2019", "05-11-2019", "06-11-2019",
               "08-11-2019", "09-11-2019")

# use the coded function:
presabs_day_site_df <- create.complete.df(dfs_list, days_vect)

nrow(presabs_day_site_df)
## 33 videos * 6 days = 198 rows ok


# add site information (the first 33 rows are NG then Boueni etc):

## first put Boueni everywhere:
presabs_day_site_df$site <- rep("Boueni", nrow(presabs_day_site_df))

## and then add N'Gouja:
presabs_day_site_df[c(1:33), "site"] <- "N'Gouja"
presabs_day_site_df[c(67:99), "site"] <- "N'Gouja"
presabs_day_site_df[c(133:165), "site"] <- "N'Gouja"

# save this df:
saveRDS(presabs_day_site_df, here::here("transformed_data", "basic_accumul_df.rds"))



# Step 3: Compute TD accumulation ####


# load the basic df:
basic_accum_df <- readRDS(here::here("transformed_data", "basic_accumul_df.rds"))

# compute the TD accumul df (and save the sp richness day variation plot):
TD_accum <- compute.td.day.accum(basic_accum_df, rich_plot = TRUE)
TD_accum_df <- TD_accum[[2]]

# save the TD_accum_df:
saveRDS(TD_accum_df, here::here("transformed_data", "TD_intraday_accum.rds"))



# Step 4: Compute FD accumulation ####


basic_accum_df <- readRDS(here::here("transformed_data", "basic_accumul_df.rds"))
tr_cat <- readRDS(here::here("transformed_data", "tr_cat_df.rds"))
sp_tr <- readRDS(here::here("transformed_data", "sp_tr_final.rds"))

# compute the FD accumul df (and save the FRic day variation plot):
FD_accum <- compute.fd.day.accum(basic_fd_accum_df = basic_accum_df,
                                 sp_tr = sp_tr,
                                 tr_cat = tr_cat,
                                 fd_indices = c("fric"),
                                 rich_plot = TRUE)

FD_accum_df <- FD_accum[[2]]

# save the TD_accum_df:
saveRDS(FD_accum_df, here::here("transformed_data", "FD_intraday_accum.rds"))



# Step 5: Compute PD accumulation ####


# First call data needed:

basic_accum_df <- readRDS(here::here("transformed_data", "basic_accumul_df.rds"))

# compute the PD accumul df (and save the FRic day variation plot):
PD_accum <- compute.pd.day.accum(basic_accum_df = basic_accum_df,
                                 rich_plot = TRUE)

PD_accum_df <- PD_accum[[2]]

# save the TD_accum_df:
saveRDS(PD_accum_df, here::here("transformed_data", "PD_intraday_accum.rds"))



# Step 6: Plot intra-day accumulation of the three facets for each day, separing sites ####
# Represent the percentage of total bioidvresity of the studied site and its accumulation #



# Load data:
TD_accum_df <- readRDS(here::here("transformed_data", "TD_intraday_accum.rds"))
FD_accum_df <- readRDS(here::here("transformed_data", "FD_intraday_accum.rds"))
PD_accum_df <- readRDS(here::here("transformed_data", "PD_intraday_accum.rds"))

# Add graphic data (TD, FD, PD order):
facets_colors <- c("#fdae61", "#abdda4", "#2b83ba")
linewidth <- 0.9

# Compute a df with values obtained from analysis before: maximal values of ...
# ... each facets for each site:
hline_df <- as.data.frame(matrix(ncol = 3, nrow = 6))
colnames(hline_df) <- c("site", "metric", "hline_value")

# B - TD
hline_df[1, ] <- c("Boueni", "TD", 74)
hline_df[2, ] <- c("N'Gouja", "TD", 84.67)
hline_df[3, ] <- c("Boueni", "FD", 57.76)
hline_df[4, ] <- c("N'Gouja", "FD", 90.70)
hline_df[5, ] <- c("N'Gouja", "PD", 90.45)
hline_df[6, ] <- c("Boueni", "PD", 78.47)
hline_df$hline_value <- as.numeric(hline_df$hline_value)


# plot (save):
plot.intra.day.accum(TD_accum_df,
                     PD_accum_df,
                     FD_accum_df,
                     hline_df,
                     facets_colors,
                     linewidth)


# Step 7: Compute interesting papers ####


# Load data:
TD_accum_df <- readRDS(here::here("transformed_data", "TD_intraday_accum.rds"))
FD_accum_df <- readRDS(here::here("transformed_data", "FD_intraday_accum.rds"))
PD_accum_df <- readRDS(here::here("transformed_data", "PD_intraday_accum.rds"))


# Compute the mean increase of TD per day:
# (end - begining / end) * 100
# N'Gouja:
NG_03 <- ((TD_accum_df$perc_TD_acc_site[which(TD_accum_df$day == "03-11-2019" &
                                            TD_accum_df$video_nb == "vid_33")] -
          TD_accum_df$perc_TD_acc_site[which(TD_accum_df$day == "03-11-2019" &
                                            TD_accum_df$video_nb == "vid_1")]) /
          TD_accum_df$perc_TD_acc_site[which(TD_accum_df$day == "03-11-2019" &
                                            TD_accum_df$video_nb == "vid_33")])*100
NG_05 <- ((TD_accum_df$perc_TD_acc_site[which(TD_accum_df$day == "05-11-2019" &
                                                TD_accum_df$video_nb == "vid_33")] -
             TD_accum_df$perc_TD_acc_site[which(TD_accum_df$day == "05-11-2019" &
                                                  TD_accum_df$video_nb == "vid_1")]) /
            TD_accum_df$perc_TD_acc_site[which(TD_accum_df$day == "05-11-2019" &
                                                 TD_accum_df$video_nb == "vid_33")])*100
NG_08 <- ((TD_accum_df$perc_TD_acc_site[which(TD_accum_df$day == "08-11-2019" &
                                                TD_accum_df$video_nb == "vid_33")] -
             TD_accum_df$perc_TD_acc_site[which(TD_accum_df$day == "08-11-2019" &
                                                  TD_accum_df$video_nb == "vid_1")]) /
            TD_accum_df$perc_TD_acc_site[which(TD_accum_df$day == "08-11-2019" &
                                                 TD_accum_df$video_nb == "vid_33")])*100
# Boueni:
B_04 <- ((TD_accum_df$perc_TD_acc_site[which(TD_accum_df$day == "04-11-2019" &
                                                TD_accum_df$video_nb == "vid_33")] -
             TD_accum_df$perc_TD_acc_site[which(TD_accum_df$day == "04-11-2019" &
                                                  TD_accum_df$video_nb == "vid_1")]) /
            TD_accum_df$perc_TD_acc_site[which(TD_accum_df$day == "04-11-2019" &
                                                 TD_accum_df$video_nb == "vid_33")])*100
B_06 <- ((TD_accum_df$perc_TD_acc_site[which(TD_accum_df$day == "06-11-2019" &
                                                TD_accum_df$video_nb == "vid_33")] -
             TD_accum_df$perc_TD_acc_site[which(TD_accum_df$day == "06-11-2019" &
                                                  TD_accum_df$video_nb == "vid_1")]) /
            TD_accum_df$perc_TD_acc_site[which(TD_accum_df$day == "06-11-2019" &
                                                 TD_accum_df$video_nb == "vid_33")])*100
B_09 <- ((TD_accum_df$perc_TD_acc_site[which(TD_accum_df$day == "09-11-2019" &
                                                TD_accum_df$video_nb == "vid_33")] -
             TD_accum_df$perc_TD_acc_site[which(TD_accum_df$day == "09-11-2019" &
                                                  TD_accum_df$video_nb == "vid_1")]) /
            TD_accum_df$perc_TD_acc_site[which(TD_accum_df$day == "09-11-2019" &
                                                 TD_accum_df$video_nb == "vid_33")])*100
mean(c(NG_03, NG_05, NG_08, B_04, B_06, B_09))
sd(c(NG_03, NG_05, NG_08, B_04, B_06, B_09))

# Compute the mean increase of FD per day:
# (end - begining / end) * 100
# N'Gouja:
NG_03 <- ((FD_accum_df$fric[which(FD_accum_df$day == "03-11-2019" &
                                                FD_accum_df$video_nb == "vid_33")] -
             FD_accum_df$fric[which(FD_accum_df$day == "03-11-2019" &
                                                  FD_accum_df$video_nb == "vid_1")]) /
            FD_accum_df$fric[which(FD_accum_df$day == "03-11-2019" &
                                                 FD_accum_df$video_nb == "vid_33")])*100
NG_05 <- ((FD_accum_df$fric[which(FD_accum_df$day == "05-11-2019" &
                                                FD_accum_df$video_nb == "vid_33")] -
             FD_accum_df$fric[which(FD_accum_df$day == "05-11-2019" &
                                                  FD_accum_df$video_nb == "vid_1")]) /
            FD_accum_df$fric[which(FD_accum_df$day == "05-11-2019" &
                                                 FD_accum_df$video_nb == "vid_33")])*100
NG_08 <- ((FD_accum_df$fric[which(FD_accum_df$day == "08-11-2019" &
                                                FD_accum_df$video_nb == "vid_33")] -
             FD_accum_df$fric[which(FD_accum_df$day == "08-11-2019" &
                                                  FD_accum_df$video_nb == "vid_1")]) /
            FD_accum_df$fric[which(FD_accum_df$day == "08-11-2019" &
                                                 FD_accum_df$video_nb == "vid_33")])*100
# Boueni:
B_04 <- ((FD_accum_df$fric[which(FD_accum_df$day == "04-11-2019" &
                                               FD_accum_df$video_nb == "vid_33")] -
            FD_accum_df$fric[which(FD_accum_df$day == "04-11-2019" &
                                                 FD_accum_df$video_nb == "vid_1")]) /
           FD_accum_df$fric[which(FD_accum_df$day == "04-11-2019" &
                                                FD_accum_df$video_nb == "vid_33")])*100
B_06 <- ((FD_accum_df$fric[which(FD_accum_df$day == "06-11-2019" &
                                               FD_accum_df$video_nb == "vid_33")] -
            FD_accum_df$fric[which(FD_accum_df$day == "06-11-2019" &
                                                 FD_accum_df$video_nb == "vid_1")]) /
           FD_accum_df$fric[which(FD_accum_df$day == "06-11-2019" &
                                                FD_accum_df$video_nb == "vid_33")])*100
B_09 <- ((FD_accum_df$fric[which(FD_accum_df$day == "09-11-2019" &
                                               FD_accum_df$video_nb == "vid_33")] -
            FD_accum_df$fric[which(FD_accum_df$day == "09-11-2019" &
                                                 FD_accum_df$video_nb == "vid_1")]) /
           FD_accum_df$fric[which(FD_accum_df$day == "09-11-2019" &
                                                FD_accum_df$video_nb == "vid_33")])*100
mean(c(NG_03, NG_05, NG_08, B_04, B_06, B_09))
sd(c(NG_03, NG_05, NG_08, B_04, B_06, B_09))


# Compute the mean increase of PD per day:
# (end - begining / end) * 100
# N'Gouja:
NG_03 <- ((PD_accum_df$accum_PD[which(PD_accum_df$day == "03-11-2019" &
                                    PD_accum_df$video_nb == "vid_33")] -
             PD_accum_df$accum_PD[which(PD_accum_df$day == "03-11-2019" &
                                      PD_accum_df$video_nb == "vid_1")]) /
            PD_accum_df$accum_PD[which(PD_accum_df$day == "03-11-2019" &
                                     PD_accum_df$video_nb == "vid_33")])*100
NG_05 <- ((PD_accum_df$accum_PD[which(PD_accum_df$day == "05-11-2019" &
                                    PD_accum_df$video_nb == "vid_33")] -
             PD_accum_df$accum_PD[which(PD_accum_df$day == "05-11-2019" &
                                      PD_accum_df$video_nb == "vid_1")]) /
            PD_accum_df$accum_PD[which(PD_accum_df$day == "05-11-2019" &
                                     PD_accum_df$video_nb == "vid_33")])*100
NG_08 <- ((PD_accum_df$accum_PD[which(PD_accum_df$day == "08-11-2019" &
                                    PD_accum_df$video_nb == "vid_33")] -
             PD_accum_df$accum_PD[which(PD_accum_df$day == "08-11-2019" &
                                      PD_accum_df$video_nb == "vid_1")]) /
            PD_accum_df$accum_PD[which(PD_accum_df$day == "08-11-2019" &
                                     PD_accum_df$video_nb == "vid_33")])*100
# Boueni:
B_04 <- ((PD_accum_df$accum_PD[which(PD_accum_df$day == "04-11-2019" &
                                   PD_accum_df$video_nb == "vid_33")] -
            PD_accum_df$accum_PD[which(PD_accum_df$day == "04-11-2019" &
                                     PD_accum_df$video_nb == "vid_1")]) /
           PD_accum_df$accum_PD[which(PD_accum_df$day == "04-11-2019" &
                                    PD_accum_df$video_nb == "vid_33")])*100
B_06 <- ((PD_accum_df$accum_PD[which(PD_accum_df$day == "06-11-2019" &
                                   PD_accum_df$video_nb == "vid_33")] -
            PD_accum_df$accum_PD[which(PD_accum_df$day == "06-11-2019" &
                                     PD_accum_df$video_nb == "vid_1")]) /
           PD_accum_df$accum_PD[which(PD_accum_df$day == "06-11-2019" &
                                    PD_accum_df$video_nb == "vid_33")])*100
B_09 <- ((PD_accum_df$accum_PD[which(PD_accum_df$day == "09-11-2019" &
                                   PD_accum_df$video_nb == "vid_33")] -
            PD_accum_df$accum_PD[which(PD_accum_df$day == "09-11-2019" &
                                     PD_accum_df$video_nb == "vid_1")]) /
           PD_accum_df$accum_PD[which(PD_accum_df$day == "09-11-2019" &
                                    PD_accum_df$video_nb == "vid_33")])*100
mean(c(NG_03, NG_05, NG_08, B_04, B_06, B_09))
sd(c(NG_03, NG_05, NG_08, B_04, B_06, B_09))
