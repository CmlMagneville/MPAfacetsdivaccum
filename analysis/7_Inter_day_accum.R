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


# Step 1: Call data needed ####




# load the fe basic_accumul_df and data needed for FD accumul fct:
tr_cat <- readRDS(here::here("transformed_data", "tr_cat_df.rds"))
fe_tr <- readRDS(here::here("transformed_data", "fe_tr.rds"))
basic_fd_accum_df <- readRDS(here::here("transformed_data", "basic_FD_accum_df.rds"))



# Step 2: Compute TD interday accumulation ####

# load the basic_accum_df (created and saved in 6_Intra_days_accumul.R):
basic_accum_df <- readRDS(here::here("transformed_data", "basic_accumul_df.rds"))

# compute interday accum
TD_interday_accum <- compute.td.interday.accum(basic_accum_df, rich_plot = TRUE)
TD_interday_df <- TD_interday_accum[[2]]

# save it:
saveRDS(TD_interday_df, here::here("transformed_data", "TD_interday_accum.rds"))


# Step 3: Compute FD interday accumulation ####



# Step 4: Compute PD interday accumulation ####



# Step 5: Plot 3 facets interday accumulation ####



