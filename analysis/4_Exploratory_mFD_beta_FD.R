###############################################################################
##
## Script to compute FD indices at the day scale
##
## 4_Exploratory_mFD.R
##
## 04/08/2022
##
## Camille Magneville
##
###############################################################################



# Step 1: Gather dataframes from the same days (two cameras dataframes) ####


# Load presabs dataframes:

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


# Compte dfs for each day and save them:
NG_03 <- merge.cam.vid.df(list_df = list(presabs_03_A1, presabs_03_C2),
                          scale = "video")
NG_05 <- merge.cam.vid.df(list_df = list(presabs_05_A1, presabs_05_C2),
                          scale = "video")
NG_08 <- merge.cam.vid.df(list_df = list(presabs_08_A1, presabs_08_C2),
                          scale = "video")
B_04 <- merge.cam.vid.df(list_df = list(presabs_04_A1, presabs_04_C2),
                         scale = "video")
B_06 <- merge.cam.vid.df(list_df = list(presabs_06_A1, presabs_06_C2),
                         scale = "video")
B_09 <- merge.cam.vid.df(list_df = list(presabs_09_A1, presabs_09_C2),
                         scale = "video")

saveRDS(NG_03, here::here("transformed_data", "presabs_NG_03.rds"))
saveRDS(NG_05, here::here("transformed_data", "presabs_NG_05.rds"))
saveRDS(NG_08, here::here("transformed_data", "presabs_NG_08.rds"))
saveRDS(B_04, here::here("transformed_data", "presabs_B_04.rds"))
saveRDS(B_06, here::here("transformed_data", "presabs_B_06.rds"))
saveRDS(B_09, here::here("transformed_data", "presabs_B_09.rds"))


# Create the sp*asb df with asb being site_day:

list1 <- list(NG_03, NG_05, NG_08)
names(list1) <- c("NG_03", "NG_05", "NG_08")
list2 <- list(B_04, B_06, B_09)
names(list2) <- c("B_04", "B_06", "B_09")

asb_sp_df <- create.asb.sp.site(list1, list2)
saveRDS(asb_sp_df, here::here("transformed_data", "asb_sp_site_day.rds"))


# Step 2: Call trait data ####


# Load sp*tr data:
sp_tr <- readRDS(here::here("transformed_data", "sp_tr_final.rds"))

# Load trait categories data:
tr_cat <- readRDS(here::here("transformed_data", "tr_cat_df.rds"))


# Step 3: Know your data ####


# Species*Traits summary:

traits_summ <- mFD::sp.tr.summary(
  tr_cat     = tr_cat,
  sp_tr      = sp_tr,
  stop_if_NA = TRUE)
traits_summ$tr_summary_list

# check traits are ok:
traits_summ$tr_types
traits_summ$mod_list

# Assemblage summary:

asb_sp_df <- as.matrix(asb_sp_df)
asb_sp_summ <- mFD::asb.sp.summary(asb_sp_w = asb_sp_df)

## number of days where the species is seen (from 1 -> 6):
asb_sp_summ$sp_tot_w
hist(asb_sp_summ$sp_tot_w, xlab = "Nombre de jours où les espèces sont vues", main = "", col = "grey90")

## nombre d'espèces vues par jour:
asb_sp_summ$asb_tot_w
barplot(asb_sp_summ$asb_tot_w, ylab = "Nombre d'espèces", col = "grey90")


# Step 4: Computing functional distances ####


sp_dist <- mFD::funct.dist(
  sp_tr         = sp_tr,
  tr_cat        = tr_cat,
  metric        = "gower",
  scale_euclid  = "scale_center",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = TRUE)


# some species pairs have a distance = 0: see how many to see if I gather ...
# ... species in FEs or not:

sp_dist_df <- mFD::dist.to.df(list(sp_dist = sp_dist))
sp_dist_df[which(sp_dist_df$sp_dist == 0), ]
nrow(sp_dist_df[which(sp_dist_df$sp_dist == 0), ])

# 102 species pairs have a fctional distance == 0: so I must group into FEs for the computation of the functional space
# and then transform FE coordinates in the fctional space into species coordinates in the same space


# Step 5: Gather into FEs and summarise FEs ####


# Gather into FEs and build asb*fes df:

## gather ito FEs:
sp_to_fe <- mFD::sp.to.fe(
  sp_tr       = sp_tr,
  tr_cat      = tr_cat,
  fe_nm_type  = "fe_rank",
  check_input = TRUE)

saveRDS(sp_to_fe, here::here("transformed_data", "sp_to_fe_all_info.rds"))

## get the nb of FEs computed from 153 species: 92
fe_nm <- sp_to_fe$fe_nm
length(sp_to_fe$fe_nm)
# save fe_nm because needed in FD accum fct:
saveRDS(fe_nm, here::here("transformed_data", "fe_nm.rds"))


## retrieve the fe*traits dataframe
fe_tr <- sp_to_fe$fe_tr
# save because needed in FD accum fct:
saveRDS(fe_tr, here::here("transformed_data", "fe_tr.rds"))

## get the number of species per FEs:
sp_to_fe$fe_nb_sp
barplot(sp_to_fe$fe_nb_sp, ylab = "Number of species per FE",col = "grey90")

## get which species is in each FE:
sp_fe_list <- sp_to_fe$sp_fe
# save because needed in FD accum fct:
saveRDS(sp_fe_list, here::here("transformed_data", "sp_to_fe_list.rds"))

## build the asb*fe df:
fes_dfs <- from.spfe.to.feasb(fe_nm, sp_fe_list, asb_sp_w = asb_sp_df)
## as I used occurrence data, weights in fes_dfs$asb_fe does not mean ...
## ... anything so I use occ data:
asb_fe_df <- fes_dfs$asb_fe_occ


# Summarise FEs:

asb_fe <- as.matrix(asb_fe_df)
asb_fe_summ <- mFD::asb.sp.summary(asb_sp_w = asb_fe)

## get FEs occurrence data
asb_fe_occ <- asb_fe_summ$asb_sp_occ

## number of FEs in each site
asb_fe_summ$asb_sp_richn
barplot(asb_fe_summ$asb_sp_richn, ylab = "Number of FEs", col = "grey90")


# Step 6: Computing fctional distances between FEs ####


fe_dist <- mFD::funct.dist(
  sp_tr         = fe_tr,
  tr_cat        = tr_cat,
  metric        = "gower",
  scale_euclid  = "scale_center",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = TRUE)

# to see easily fctional distances, convert dist object into a df:
fe_dist_df <- mFD::dist.to.df(list(fe_dist = fe_dist))


# Step 7: Compute fctional space and their quality ####


# compute functional spaces quality with spaces with up to 10 PCs:
fspaces_quality <- mFD::quality.fspaces(
  sp_dist             = fe_dist,
  maxdim_pcoa         = 10,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")


fspaces_quality$quality_fspaces
# The best space has 5 dimensions

# Plot the quality of functional spaces:
mFD::quality.fspaces.plot(
  fspaces_quality            = fspaces_quality,
  quality_metric             = "mad",
  fspaces_plot               = c("tree_average", "pcoa_2d", "pcoa_3d",
                                 "pcoa_4d", "pcoa_5d", "pcoa_6d"),
  name_file                  = NULL,
  range_dist                 = NULL,
  range_dev                  = NULL,
  range_qdev                 = NULL,
  gradient_deviation         = c(neg = "darkblue", nul = "grey80", pos = "darkred"),
  gradient_deviation_quality = c(low = "yellow", high = "red"),
  x_lab                      = "Trait-based distance")



# Step 8: Test correlations between traits and axes ####


# Get FEs coordinates along the 5 first PCS:
fe_faxes_coord <- fspaces_quality$"details_fspaces"$"sp_pc_coord"

# Get the correlations and plot it for FEs space:
fe_tr_faxes <- mFD::traits.faxes.cor(
  sp_tr          = fe_tr,
  sp_faxes_coord = fe_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4", "PC5")],
  plot           = TRUE)
correl_tr_faxes <- fe_tr_faxes$tr_faxes_plot

ggplot2::ggsave(filename = here::here("outputs/Correl_tr_faxes.pdf"),
                plot = correl_tr_faxes,
                device = "pdf",
                scale = 1,
                height = 5000,
                width = 11000,
                units = "px",
                dpi = 600)

# PC1: (-) Big size, Very Mobile, High level
# PC2: (-) LargeG


# Step 9: Plot 5D functional space on the first 4 axis ####


big_plot <- mFD::funct.space.plot(
  sp_faxes_coord  = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4", "PC5")],
  faxes           = c("PC1", "PC2", "PC3", "PC4"),
  name_file       = NULL,
  faxes_nm        = NULL,
  range_faxes     = c(NA, NA),
  color_bg        = "grey95",
  color_pool      = "darkgreen",
  fill_pool       = "white",
  shape_pool      = 21,
  size_pool       = 1,
  plot_ch         = TRUE,
  color_ch        = "black",
  fill_ch         = "white",
  alpha_ch        = 0.5,
  plot_vertices   = TRUE,
  color_vert      = "blueviolet",
  fill_vert       = "blueviolet",
  shape_vert      = 23,
  size_vert       = 1,
  plot_sp_nm      = NULL,
  nm_size         = 3,
  nm_color        = "black",
  nm_fontface     = "plain",
  check_input     = TRUE)


# Then convert FE coordinates into species coordinates:
sp_faxes_coord <- from.fecoord.to.spcoord(fe_faxes_coord, asb_sp_df,
                                    sp_to_fe)
saveRDS(sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4", "PC5")], here::here("transformed_data", "sp_faxes_coord.rds"))


# Step 10: Get FD indices for each day: FRic, Fspe, FDis and FIde ####


# Compute for each day with species:

asb_sp_df <- as.matrix(asb_sp_df)
sp_faxes_coord <- as.matrix(sp_faxes_coord)

alpha_fd_indices_day <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4", "PC5")],
  asb_sp_w         = asb_sp_df,
  ind_vect         = c("fdis", "fric",
                       "fdiv",
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

alpha_fd_indices_day$functional_diversity_indices

# Compute beta between days:
beta_fd_indices_day <- mFD::beta.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4", "PC5")],
  asb_sp_occ       = asb_sp_df,
  check_input      = TRUE,
  beta_family      = c("Jaccard"),
  details_returned = TRUE)


# Test differences in FRic between sites using day scale:
NG_FD <- alpha_fd_indices_day$functional_diversity_indices[c(1:3), 3]
B_FD <- alpha_fd_indices_day$functional_diversity_indices[c(4:6), 3]
kruskal.test(NG_FD, B_FD)
# p value not significative so do not reject H0: same FRic between sites

# Test differences in FDis between sites using day scale:
NG_FD <- alpha_fd_indices_day$functional_diversity_indices[c(1:3), 2]
B_FD <- alpha_fd_indices_day$functional_diversity_indices[c(4:6), 2]
kruskal.test(NG_FD, B_FD)
# p value not significative so do not reject H0: same FDis between sites

# Test differences in FSpe between sites using day scale:
NG_FD <- alpha_fd_indices_day$functional_diversity_indices[c(1:3), 5]
B_FD <- alpha_fd_indices_day$functional_diversity_indices[c(4:6), 5]
kruskal.test(NG_FD, B_FD)
# p value not significative so do not reject H0: same FD between sites

# Compute for each site with FEs:

## first I should gather the first three rows of asb_fe_df and idem three next rows:
site_asb_df <- asb_sp_df
site_asb_df[1, ] <- site_asb_df[1, ] + site_asb_df[2, ] + site_asb_df[3, ]
site_asb_df[4, ] <- site_asb_df[4, ] + site_asb_df[5, ] + site_asb_df[6, ]
site_asb_df <- site_asb_df[-c(2, 3, 5, 6), ]
site_asb_df [site_asb_df  > 1] <- 1
rownames(site_asb_df) <- c("N'Gouja", "Boueni")
saveRDS(site_asb_df, here::here("transformed_data", "site_asb_df.rds"))


alpha_fd_indices_site <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4", "PC5")],
  asb_sp_w         = site_asb_df,
  ind_vect         = c("fdis", "fric",
                       "fdiv",
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

alpha_fd_indices_site$functional_diversity_indices



# Step 11: Plot indices ####


# At the site level (easier with mFD package, day level comes after):

plots_alpha_site <- mFD::alpha.multidim.plot(
  output_alpha_fd_multidim = alpha_fd_indices_site,
  plot_asb_nm              = c("N'Gouja", "Boueni"),
  ind_nm                   = c("fdis", "fide", "fric",
                               "fspe"),
  faxes                    = NULL,
  faxes_nm                 = NULL,
  range_faxes              = c(NA, NA),
  color_bg                 = "grey95",
  shape_sp                 = c(pool = 3, asb1 = 21, asb2 = 21),
  size_sp                  = c(pool = 0.7, asb1 = 0.8, asb2 = 0.8),
  color_sp                 = c(pool = "grey50", asb1 = "#80cdc1", asb2 = "#bf812d"),
  color_vert               = c(pool = "grey50", asb1 = "#80cdc1", asb2 = "#bf812d"),
  fill_sp                  = c(pool = NA, asb1 = "#80cdc1", asb2 = "#bf812d"),
  fill_vert                = c(pool = NA, asb1 = "#80cdc1", asb2 = "#bf812d"),
  color_ch                 = c(pool = NA, asb1 = "#80cdc1", asb2 = "#bf812d"),
  fill_ch                  = c(pool = "white", asb1 = "#80cdc1", asb2 = "#bf812d"),
  alpha_ch                 = c(pool = 1, asb1 = 0.6, asb2 = 0.3),
  shape_centroid_fdis      = c(asb1 = 22,  asb2 = 24),
  shape_centroid_fdiv      = c(asb1 = 22,  asb2 = 24),
  shape_centroid_fspe      = 23,
  color_centroid_fspe      = "black",
  size_sp_nm               = 3,
  color_sp_nm              = "black",
  plot_sp_nm               = NULL,
  fontface_sp_nm           = "plain",
  save_file                = FALSE,
  check_input              = TRUE)

fric_site_plot_PC124 <- (plots_alpha_site$fric$PC1_PC2 + plots_alpha_site$fric$caption +
                  plots_alpha_site$fric$PC1_PC4) +
  patchwork::plot_layout(byrow = TRUE, heights = c(1, 1), widths = c(1, 1),
                         ncol = 2, nrow = 2, guides = "collect")

# now I have to build the PC3-PC5 so we can have a graph with all the dimensions:

# Compute the range of functional axes:
range_sp_coord  <- range(sp_faxes_coord)

# Based on the range of species coordinates values, compute a nice range ...
# ... for functional axes:
range_faxes <- range_sp_coord +
  c(-1, 1) * (range_sp_coord[2] - range_sp_coord[1]) * 0.05
range_faxes

# get species coordinates along the two studied axes:
sp_faxes_coord_xy <- sp_faxes_coord[, c("PC3", "PC5")]

# Plot background with grey backrgound:
plot_k <- mFD::background.plot(range_faxes = range_faxes,
                               faxes_nm = c("PC3", "PC5"),
                               color_bg = "grey95")

# Retrieve vertices coordinates along the two studied functional axes:
vert <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_xy,
                      order_2D = FALSE,
                      check_input = TRUE)

plot_k <- mFD::pool.plot(ggplot_bg = plot_k,
                               sp_coord2D = sp_faxes_coord_xy,
                               vertices_nD = vert,
                               plot_pool = FALSE,
                               color_pool = "grey50",
                               fill_pool = NA,
                               alpha_ch =  0.8,
                               color_ch = NA,
                               fill_ch = "white",
                               shape_pool = 3,
                               size_pool = 0.7,
                               shape_vert = 3,
                               size_vert = 0.7,
                               color_vert = "grey50",
                               fill_vert = "grey50")

# Boueni:
## filter species from B:
sp_filter_B <- mFD::sp.filter(asb_nm = c("Boueni"),
                                    sp_faxes_coord = sp_faxes_coord_xy,
                                    asb_sp_w = site_asb_df)
## get species coordinates (B):
sp_faxes_coord_B <- sp_filter_B$`species coordinates`


# NGouja:
## filter species from NG:
sp_filter_NG <- mFD::sp.filter(asb_nm = c("N'Gouja"),
                                    sp_faxes_coord = sp_faxes_coord_xy,
                                    asb_sp_w = site_asb_df)
## get species coordinates (basket_6):
sp_faxes_coord_NG <- sp_filter_NG$`species coordinates`

#B:
vert_nm_B <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_B,
                                 order_2D = TRUE,
                                 check_input = TRUE)

# NG:
vert_nm_NG <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_NG,
                                 order_2D = TRUE,
                                 check_input = TRUE)

plot_PC3_PC5 <- mFD::fric.plot(ggplot_bg = plot_k,
                           asb_sp_coord2D = list("N'Gouja" = sp_faxes_coord_NG,
                                                 "Boueni" = sp_faxes_coord_B),
                           asb_vertices_nD = list("N'Gouja" = vert_nm_NG,
                                                  "Boueni" = vert_nm_B),

                           plot_sp = TRUE,

                           color_ch = c("N'Gouja" = "#80cdc1",
                                        "Boueni" = "#bf812d"),
                           fill_ch = c("N'Gouja" = "#80cdc1",
                                       "Boueni" = "#bf812d"),
                           alpha_ch = c("N'Gouja" = 0.6,
                                        "Boueni" = 0.3),

                           shape_sp = c("N'Gouja" = 21,
                                        "Boueni" = 21),
                           size_sp = c("N'Gouja" = 0.8,
                                       "Boueni" = 0.8),
                           color_sp = c("N'Gouja" = "#80cdc1",
                                        "Boueni" = "#bf812d"),
                           fill_sp = c("N'Gouja" = "#80cdc1",
                                       "Boueni" = "#bf812d"),

                           shape_vert = c("N'Gouja" = 21,
                                          "Boueni" = 21),
                           size_vert = c("N'Gouja" = 0.8,
                                         "Boueni" = 0.8),
                           color_vert = c("N'Gouja" = "#80cdc1",
                                          "Boueni" = "#bf812d"),
                           fill_vert = c("N'Gouja" = "#80cdc1",
                                         "Boueni" = "#bf812d"))


fric_site_plot <- (plots_alpha_site$fric$PC1_PC2 + plots_alpha_site$fric$caption +
                           plots_alpha_site$fric$PC1_PC4 + plot_PC3_PC5) +
  patchwork::plot_layout(byrow = TRUE, heights = c(1, 1), widths = c(1, 1),
                         ncol = 2, nrow = 2, guides = "collect")

ggplot2::ggsave(filename = here::here("outputs/FRic_sites.pdf"),
                plot = fric_site_plot,
                device = "pdf",
                scale = 1,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

fdis_site_plot <- plots_alpha_site$fdis$patchwork # not lisible
fspe_site_plot <- plots_alpha_site$fspe$patchwork # not lisible
fide_site_plot <- plots_alpha_site$fide$patchwork # not lisible

# Get a species/ several species names associated with one FE:
search.sp.nm(sp_to_fe, "fe_73")


# Step 12: Compute functional beta indices ####


beta_fd_indices <- mFD::beta.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4", "PC5")],
  asb_sp_occ       = site_asb_df,
  check_input      = TRUE,
  beta_family      = c("Jaccard"),
  details_returned = TRUE)


# Step 13: Where are the species present only in each site? ####


# Retrieve the names of fe in each site:
fe_NG <- colnames(asb_fe_occ[, which(asb_fe_occ[1,] == 1 |
                                       asb_fe_occ[2,] == 1 |
                                       asb_fe_occ[3,] == 1)])
fe_B <- colnames(asb_fe_occ[, which(asb_fe_occ[4,] == 1 |
                                      asb_fe_occ[5,] == 1 |
                                      asb_fe_occ[6,] == 1)])

# Retrieve the names of fe being only present in each site:
unique_NG <- setdiff(fe_NG, fe_B)
unique_B <- setdiff(fe_B, fe_NG)

# Plot in the functional space (check individual plots):
unique_NG_plot <- mFD::funct.space.plot(
  sp_faxes_coord  = fe_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4", "PC5")],
  faxes           = c("PC1", "PC2", "PC3", "PC4"),
  name_file       = NULL,
  faxes_nm        = NULL,
  range_faxes     = c(NA, NA),
  color_bg        = "grey95",
  color_pool      = "darkgreen",
  fill_pool       = "white",
  shape_pool      = 21,
  size_pool       = 1,
  plot_ch         = TRUE,
  color_ch        = "black",
  fill_ch         = "white",
  alpha_ch        = 0.5,
  plot_vertices   = TRUE,
  color_vert      = "blueviolet",
  fill_vert       = "blueviolet",
  shape_vert      = 23,
  size_vert       = 1,
  plot_sp_nm      = unique_NG,
  nm_size         = 3,
  nm_color        = "black",
  nm_fontface     = "plain",
  check_input     = TRUE)

unique_B_plot <- mFD::funct.space.plot(
  sp_faxes_coord  = fe_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4", "PC5")],
  faxes           = c("PC1", "PC2", "PC3", "PC4"),
  name_file       = NULL,
  faxes_nm        = NULL,
  range_faxes     = c(NA, NA),
  color_bg        = "grey95",
  color_pool      = "darkgreen",
  fill_pool       = "white",
  shape_pool      = 21,
  size_pool       = 1,
  plot_ch         = TRUE,
  color_ch        = "black",
  fill_ch         = "white",
  alpha_ch        = 0.5,
  plot_vertices   = TRUE,
  color_vert      = "blueviolet",
  fill_vert       = "blueviolet",
  shape_vert      = 23,
  size_vert       = 1,
  plot_sp_nm      = unique_B,
  nm_size         = 3,
  nm_color        = "black",
  nm_fontface     = "plain",
  check_input     = TRUE)


# Retrieve the traits of unique species:
# ... + precisely: the porportion of unique species in each class to compare:
sp_NG <- colnames(site_asb_df[, which(site_asb_df[1,] == 1)])
sp_B <- colnames(site_asb_df[, which(site_asb_df[2,] == 1)])

unique_sp_NG <- setdiff(sp_NG, sp_B)
unique_sp_B <- setdiff(sp_B, sp_NG)

diet_unique_NG <- sp_tr_final[which(rownames(sp_tr_final) %in% unique_sp_NG), ] %>%
  dplyr::group_by(Diets) %>%
  dplyr::summarise(nb = n())
diet_unique_NG$nb <- (diet_unique_NG$nb/length(unique_sp_NG))*100

diet_unique_B <- sp_tr_final[which(rownames(sp_tr_final) %in% unique_sp_B), ] %>%
  dplyr::group_by(Diets) %>%
  dplyr::summarise(nb = n())
diet_unique_B$nb <- (diet_unique_B$nb/length(unique_sp_B))*100


homerange_unique_NG <- sp_tr_final[which(rownames(sp_tr_final) %in% unique_sp_NG), ] %>%
  dplyr::group_by(Home_Range) %>%
  dplyr::summarise(nb = n())
homerange_unique_NG$nb <- (homerange_unique_NG$nb/length(unique_sp_NG))*100

homerange_unique_B <- sp_tr_final[which(rownames(sp_tr_final) %in% unique_sp_B), ] %>%
  dplyr::group_by(Home_Range) %>%
  dplyr::summarise(nb = n())
homerange_unique_B$nb <- (homerange_unique_B$nb/length(unique_sp_B))*100


size_unique_NG <- sp_tr_final[which(rownames(sp_tr_final) %in% unique_sp_NG), ] %>%
  dplyr::group_by(Size_Class) %>%
  dplyr::summarise(nb = n())
size_unique_NG$nb <- (size_unique_NG$nb/length(unique_sp_NG))*100

size_unique_B <- sp_tr_final[which(rownames(sp_tr_final) %in% unique_sp_B), ] %>%
  dplyr::group_by(Size_Class) %>%
  dplyr::summarise(nb = n())
size_unique_B$nb <- (size_unique_B$nb/length(unique_sp_B))*100


act_unique_NG <- sp_tr_final[which(rownames(sp_tr_final) %in% unique_sp_NG), ] %>%
  dplyr::group_by(Activity) %>%
  dplyr::summarise(nb = n())
act_unique_NG$nb <- (act_unique_NG$nb/length(unique_sp_NG))*100

act_unique_B <- sp_tr_final[which(rownames(sp_tr_final) %in% unique_sp_B), ] %>%
  dplyr::group_by(Activity) %>%
  dplyr::summarise(nb = n())
act_unique_B$nb <- (act_unique_B$nb/length(unique_sp_B))*100


school_unique_NG <- sp_tr_final[which(rownames(sp_tr_final) %in% unique_sp_NG), ] %>%
  dplyr::group_by(Schooling) %>%
  dplyr::summarise(nb = n())
school_unique_NG$nb <- (school_unique_NG$nb/length(unique_sp_NG))*100

school_unique_B <- sp_tr_final[which(rownames(sp_tr_final) %in% unique_sp_B), ] %>%
  dplyr::group_by(Schooling) %>%
  dplyr::summarise(nb = n())
school_unique_B$nb <- (school_unique_B$nb/length(unique_sp_B))*100


levwater_unique_NG <- sp_tr_final[which(rownames(sp_tr_final) %in% unique_sp_NG), ] %>%
  dplyr::group_by(Level_water) %>%
  dplyr::summarise(nb = n())
levwater_unique_NG$nb <- (levwater_unique_NG$nb/length(unique_sp_NG))*100

levwater_unique_B <- sp_tr_final[which(rownames(sp_tr_final) %in% unique_sp_B), ] %>%
  dplyr::group_by(Level_water) %>%
  dplyr::summarise(nb = n())
levwater_unique_B$nb <- (levwater_unique_B$nb/length(unique_sp_B))*100


