###############################################################################
##
## Script to compute and plot the accumulation of the three facets ...
## ... through the three days of sampling and diff mean video's diversities
##
## 7_Inter_day_accumul.R
##
## 11/08/2022
##
## Camille Magneville
##
###############################################################################


# Step 1: Compute TD interday accumulation and test TD difference between sites ####

# load the basic_accum_df (created and saved in 6_Intra_days_accumul.R):
basic_accum_df <- readRDS(here::here("transformed_data", "basic_accumul_df.rds"))

# compute interday accum REDO THE FCT TO VIDEO FORMAT
TD_interday_accum <- compute.td.interday.accum(basic_accum_df, rich_plot = TRUE)
TD_interday_df <- TD_interday_accum[[2]]

# save it:
saveRDS(TD_interday_df, here::here("transformed_data", "TD_interday_accum.rds"))


# TEST:

# Get the dataframe with species richness on each hour (3*9 values per site):
TD_hour_df <- TD_interday_accum[[3]]

# Get interesting values on which the T-test is done:
NG_TD <- TD_hour_df$richn[which(TD_hour_df$site == "N'Gouja")]
length(NG_TD) # 27 ok :)
B_TD <- TD_hour_df$richn[which(TD_hour_df$site == "Boueni")]
length(B_TD) # 27 ok :)

test_df <- cbind(c(rep("N'Gouja", length(NG_TD)), rep("Boueni", length(B_TD))),
                 c(NG_TD, B_TD), c(1:2*length(NG_TD)))
colnames(test_df) <- c("site", "TD", "id")
test_df <- as.data.frame(test_df)
test_df$TD <- as.numeric(test_df$TD)
test_df$site <- as.factor(test_df$site)

# Test normality of data to know which T test to use:
test_df %>%
  dplyr::group_by(site) %>%
  rstatix::shapiro_test(TD)
# normality of data, can use student t test

# Test homogeneity of variance to know which T test to use:
rstatix::levene_test(data = test_df, TD ~ site)
# pvalue < 5% : significative difference in variance: use welch correction:

# Test: in fact use krusal test to have an identical test for 3 facets:
kruskal.test(NG_TD, B_TD)
# Alternative: N'Gouja has a lower TD mean on video than Boueni: significant


# Step 2: Compute FD interday accumulation #### REDO THE FCT TO VIDEO FORMAT


# load the fe basic_accumul_df and data needed for FD accumul fct:
tr_cat <- readRDS(here::here("transformed_data", "tr_cat_df.rds"))
sp_tr <- readRDS(here::here("transformed_data", "sp_tr_final.rds"))
basic_accum_df <- readRDS(here::here("transformed_data", "basic_accumul_hour_df.rds"))


# arguments:
fd_indices <- c("fric", "fspe", "fdis", "fdiv", "fide")


FD_interday_accum <- compute.fd.interday.accum(basic_fd_accum_df = basic_accum_df,
                                 sp_tr,
                                 tr_cat,
                                 fd_indices,
                                 rich_plot = TRUE)

FD_interday_df <- FD_interday_accum[[2]]

# save it:
saveRDS(FD_interday_df, here::here("transformed_data", "FD_interday_accum.rds"))


# TEST:

# Get the dataframe with species richness on each video (3*9 values per site):
FD_vid_df <- FD_interday_accum[[3]]

# Get interesting values on which the T-test is done:
NG_FD <- FD_vid_df$fric[which(FD_vid_df$site == "N'Gouja")]
length(NG_FD) # 27 ok :)
B_FD <- FD_vid_df$fric[which(FD_vid_df$site == "Boueni")]
length(B_FD) # 27 ok :)

test_df <- cbind(c(rep("N'Gouja", length(NG_FD)), rep("Boueni", length(B_FD))),
                 c(NG_FD, B_FD), c(1:2*length(NG_FD)))
colnames(test_df) <- c("site", "FD", "id")
test_df <- as.data.frame(test_df)
test_df$FD <- as.numeric(test_df$FD)
test_df$site <- as.factor(test_df$site)

# Test normality of data to know which T test to use:
test_df %>%
  dplyr::group_by(site) %>%
  rstatix::shapiro_test(FD)
# not normal: use wilcoxon test

# Test: in fact use wilcoxon test to have an identical test for 3 facets:
kruskal.test(NG_FD, B_FD)
# No significant difference of FD based on video scale btw N'Gouja and Boueni



# Step 3: Compute PD interday accumulation #### REDO THE FCT TO VIDEO FORMAT


# load the basic_accum_df (created and saved in 6_Intra_days_accumul.R):
basic_accum_df <- readRDS(here::here("transformed_data", "basic_accumul_hour_df.rds"))

# compute:
PD_interday_accum <- compute.pd.interday.accum(basic_accum_df,
                                               rich_plot = TRUE)
PD_interday_df <- PD_interday_accum[[2]]

# save it:
saveRDS(PD_interday_df, here::here("transformed_data", "PD_interday_accum.rds"))

# TEST:

# Get the dataframe with species richness on each hour (3*9 values per site):
PD_vid_df <- PD_interday_accum[[3]]

# Get interesting values on which the T-test is done:
NG_PD <- PD_vid_df$hour_PD[which(PD_vid_df$site == "N'Gouja")]
length(NG_PD) # 27 ok :)
B_PD <- PD_vid_df$hour_PD[which(PD_vid_df$site == "Boueni")]
length(B_PD) # 27 ok :)

test_df <- cbind(c(rep("N'Gouja", length(NG_PD)), rep("Boueni", length(B_PD))),
                 c(NG_PD, B_PD), c(1:2*length(NG_PD)))
colnames(test_df) <- c("site", "PD", "id")
test_df <- as.data.frame(test_df)
test_df$PD <- as.numeric(test_df$PD)
test_df$site <- as.factor(test_df$site)

# Test normality of data to know which T test to use:
test_df %>%
  dplyr::group_by(site) %>%
  rstatix::shapiro_test(PD)
# normal, could test homogeneity of variance but use wilcox so consistent for the 3 facets

# Test: in fact use Kruskal test to have an identical test for 3 facets:
kruskal.test(NG_PD, B_PD)
# No significant difference of FD based on video scale btw N'Gouja and Boueni



# Step 4: Plot 3 facets interday accumulation ####


# Call the three accumulation df:
TD_accum_df <- readRDS(here::here("transformed_data", "TD_interday_accum.rds"))
FD_accum_df <- readRDS(here::here("transformed_data", "FD_interday_accum.rds"))
PD_accum_df <- readRDS(here::here("transformed_data", "PD_interday_accum.rds"))


# argument:
sites_colors <- c("#bf812d", "#80cdc1")
linewidth <- 0.9

# then plot:
plot.inter.day.accum(TD_accum_df,
                     PD_accum_df,
                     FD_accum_df,
                     sites_colors,
                     linewidth)



# Step 5: Plot the delta between N'Gouja and Boueni diversities ####


TD_accum_df <- readRDS(here::here("transformed_data", "TD_interday_accum.rds"))
FD_accum_df <- readRDS(here::here("transformed_data", "FD_interday_accum.rds"))
PD_accum_df <- readRDS(here::here("transformed_data", "PD_interday_accum.rds"))

facets_colors <- c("#fdae61", "#abdda4", "#2b83ba")
linewidth <- 0.9

plot.delta.alpha.inter.day.accum(TD_accum_df,
                                             PD_accum_df,
                                             FD_accum_df,
                                             facets_colors,
                                             linewidth)
