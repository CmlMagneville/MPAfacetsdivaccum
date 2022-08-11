###############################################################################
##
## Script to compute and plot the accumulation of the three facets ...
## ... through the three days of sampling
##
## 7_Inter_day_accumul.R
##
## 11/08/2022
##
## Camille Magneville
##
###############################################################################


# Step 1: Compute TD interday accumulation ####

# load the basic_accum_df (created and saved in 6_Intra_days_accumul.R):
basic_accum_df <- readRDS(here::here("transformed_data", "basic_accumul_df.rds"))

# compute interday accum
TD_interday_accum <- compute.td.interday.accum(basic_accum_df, rich_plot = TRUE)
TD_interday_df <- TD_interday_accum[[2]]

# save it:
saveRDS(TD_interday_df, here::here("transformed_data", "TD_interday_accum.rds"))


# Step 2: Compute FD interday accumulation ####


# load the fe basic_accumul_df and data needed for FD accumul fct:
tr_cat <- readRDS(here::here("transformed_data", "tr_cat_df.rds"))
fe_tr <- readRDS(here::here("transformed_data", "fe_tr.rds"))
basic_fd_accum_df <- readRDS(here::here("transformed_data", "basic_FD_accum_df.rds"))


# arguments:
fd_indices <- c("fric", "fspe", "fdis", "fdiv", "fide")
sp_tr <- fe_tr

FD_interday_accum <- compute.fd.day.accum(basic_fd_accum_df,
                                 sp_tr,
                                 tr_cat,
                                 fd_indices,
                                 rich_plot = TRUE)

FD_interday_df <- FD_interday_accum[[2]]

# save it:
saveRDS(FD_interday_df, here::here("transformed_data", "FD_interday_accum.rds"))


# Step 3: Compute PD interday accumulation ####


# load the basic_accum_df (created and saved in 6_Intra_days_accumul.R):
basic_accum_df <- readRDS(here::here("transformed_data", "basic_accumul_df.rds"))

# compute:
PD_interday_accum <- compute.pd.interday.accum(basic_accum_df,
                                               rich_plot = TRUE)
PD_interday_df <- PD_interday_accum[[2]]

# save it:
saveRDS(PD_interday_df, here::here("transformed_data", "PD_interday_accum.rds"))


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





