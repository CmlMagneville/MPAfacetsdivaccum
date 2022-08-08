###############################################################################
##
## Script to compute and plot the accumulation of the three facets ...
## ... through a day
##
## 3_TD_accul_fcts.R
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


# Step 4: Compute FD accumulation ####


# Step 5: Compute PD accumulation ####


# Step 6: Plot intra-day accumulation of the three facets for each day, separing sites ####
