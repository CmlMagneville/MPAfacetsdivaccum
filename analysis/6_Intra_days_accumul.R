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



# load the basic df:
basic_accum_df <- readRDS(here::here("transformed_data", "basic_accumul_df.rds"))

# compute the TD accumul df (and save the sp richness day variation plot):
TD_accum <- compute.td.day.accum(basic_accum_df, rich_plot = TRUE)
TD_accum_df <- TD_accum[[2]]

# save the TD_accum_df:
saveRDS(TD_accum_df, here::here("transformed_data", "TD_intraday_accum.rds"))



# Step 4: Compute FD accumulation ####



# First call data needed (will work with FEs so FEs data also):

sp_fe_list <- readRDS(here::here("transformed_data", "sp_to_fe_list.rds"))
fe_nm <- readRDS(here::here("transformed_data", "fe_nm.rds"))

tr_cat <- readRDS(here::here("transformed_data", "tr_cat_df.rds"))
fe_tr <- readRDS(here::here("transformed_data", "fe_tr.rds"))

basic_accum_df <- readRDS(here::here("transformed_data", "basic_accumul_df.rds"))



# Compute a dataframe with fe*asb where asb = video_site_day

## first must put basic_accum_df into a asb*sp format:
clean_accum_df <- basic_accum_df
rownames(clean_accum_df) <- clean_accum_df$vid_id
clean_accum_df <- clean_accum_df[, -c(ncol(clean_accum_df), ncol(clean_accum_df) - 1,
                                      ncol(clean_accum_df) - 2, ncol(clean_accum_df) - 3)]
# values -> numeric and not character:
clean_accum_df <- apply(clean_accum_df, 2, as.numeric)

## use the from_sp_tofe fct():
asb_fe_w <- from.spfe.to.feasb(fe_nm, sp_fe_list, asb_sp_w = clean_accum_df)
asb_fe_w <- asb_fe_w$asb_fe_occ

## add the site, video etc information:
basic_FD_accum_df <- cbind(asb_fe_w, basic_accum_df[, c(ncol(basic_accum_df), ncol(basic_accum_df) - 1,
                                                          ncol(basic_accum_df) - 2, ncol(basic_accum_df) - 3)])

# compute the FD accumul df (and save the FRic day variation plot):
FD_accum <- compute.fd.day.accum(basic_fd_accum_df = basic_FD_accum_df,
                                 sp_tr = fe_tr,
                                 tr_cat = tr_cat,
                                 fd_indices = c("fric"),
                                 rich_plot = TRUE)

FD_accum_df <- FD_accum[[2]]

# save the TD_accum_df:
saveRDS(FD_accum_df, here::here("transformed_data", "FD_intraday_accum.rds"))



# Step 5: Compute PD accumulation ####





# Step 6: Plot intra-day accumulation of the three facets for each day, separing sites ####
