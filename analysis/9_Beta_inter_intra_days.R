###############################################################################
##
## Script to compute and plot beta diversities inter and intra days
##
## 9_Beta_inter_intra_days.R
##
## 19/08/2022
##
## Camille Magneville
##
###############################################################################



# Step 1: Call data ####


# TD, FD and  PD:
basic_accum_df <- readRDS(here::here("transformed_data", "basic_accumul_df.rds"))

# FD:
sp_faxes_coord <- readRDS(here::here("transformed_data", "sp_faxes_coord.rds"))



# Step 2: Compute beta diversities avec save them ####


# TD:
basic_accum_df[, -c(151:154)] <- apply(basic_accum_df[, -c(151:154)], 2, as.numeric)
rownames(basic_accum_df) <- basic_accum_df$vid_id
basic_accum_df <- basic_accum_df[, -c(151:154)]

beta_TD <- betapart::beta.pair(basic_accum_df, index.family = "jaccard")
saveRDS(beta_TD, here::here("transformed_data", "beta_TD_videos_dist.rds"))

# PD:

## Change 11 species names which are not in the fishtree phylogeny in the ...
## ... basic_accum_df:

basic_accum_df <- as.data.frame(basic_accum_df)
basic_accum_df <- dplyr::rename(basic_accum_df, "Gomphosus_varius" = "Gomphosus_caeruleus")
basic_accum_df <- dplyr::rename(basic_accum_df, "Scolopsis_bilineata" = "Scolopsis_frenata")
basic_accum_df <- dplyr::rename(basic_accum_df, "Scolopsis_bimaculata" = "Scolopsis_ghanam")
basic_accum_df <- dplyr::rename(basic_accum_df, "Cetoscarus_bicolor" = "Cetoscarus_ocellatus")
basic_accum_df <- dplyr::rename(basic_accum_df, "Scarus_altipinnis" = "Scarus_falcipinnis")
basic_accum_df <- dplyr::rename(basic_accum_df, "Scarus_oviceps" = "Scarus_scaber")
basic_accum_df <- dplyr::rename(basic_accum_df, "Tylosurus_crocodilus_crocodilus" = "Tylosurus_crocodilus")
basic_accum_df <- dplyr::rename(basic_accum_df, "Chlorurus_microrhinos" = "Chlorurus_strongylocephalus")
basic_accum_df <- dplyr::rename(basic_accum_df, "Canthigaster_coronata" = "Canthigaster_cyanospilota")
basic_accum_df <- dplyr::rename(basic_accum_df, "Labropsis_australis" = "Labropsis_xanthonota")
basic_accum_df <- dplyr::rename(basic_accum_df, "Ctenochaetus_striatus" = "Ac_Cten_dark")


sp_nm_all <- colnames(basic_accum_df)

phylo <- fishtree::fishtree_phylogeny(species = sp_nm_all)

# Compute beta PD:
beta_PD <- betapart::phylo.beta.pair(basic_accum_df, phylo, index.family = "jaccard")
saveRDS(beta_PD, here::here("transformed_data", "beta_PD_videos_dist.rds"))


# FD:
basic_fd <- basic_accum_df
basic_fd[, -c(87:90)] <- apply(basic_fd[, -c(87:90)], 2, as.numeric)
rownames(basic_fd) <- basic_fd$vid_id
basic_fd <- basic_fd[, -c(87:90)]

beta_FD <- mFD::beta.fd.multidim(
   sp_faxes_coord   = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4", "PC5")],
   asb_sp_occ       = basic_fd,
   check_input      = TRUE,
   beta_family      = c("Jaccard"),
  details_returned = TRUE)
saveRDS(beta_FD, here::here("transformed_data", "beta_FD_videos_dist.rds"))



# Step 3: Compute 3 datatables which beta div values and pairs informations ####


# Here, we compute 3 dataframes: one for each diversity facets. Each contains ...
# ... sp1|sp2|value_beta|same_day|same_vid|same_site|site_nm columns (for FD sp = FE) ...
# ... if same_day = TRUE and same_video = FALSE -> intraday variation
# ... if same_day = FALSE and same_video = TRUE and site = same -> interday variation

# Call TD data:
beta_TD <- readRDS(here::here("transformed_data", "beta_TD_videos_dist.rds"))
beta_TD_tot <- beta_TD$beta.jac

# Compute df:
beta_TD_df <- beta.video.clean.df(beta_df = beta_TD_tot)
saveRDS(beta_TD_df, here::here("transformed_data", "beta_TD_df.rds"))


# Call PD data:
beta_PD <- readRDS(here::here("transformed_data", "beta_PD_videos_dist.rds"))
beta_PD_tot <- beta_PD$phylo.beta.jac

# Compute df
beta_PD_df <- beta.video.clean.df(beta_df = beta_PD_tot)
saveRDS(beta_PD_df, here::here("transformed_data", "beta_PD_df.rds"))


# Call FD data:
beta_FD <- readRDS(here::here("transformed_data", "beta_FD_videos_dist.rds"))
beta_FD_tot <- beta_FD$pairasb_fbd_indices$jac_diss

# Compute df:
beta_FD_df <- beta.video.clean.df(beta_FD_tot)
saveRDS(beta_FD_df, here::here("transformed_data", "beta_FD_df.rds"))




# Call data:
beta_TD_df <- readRDS(here::here("transformed_data", "beta_TD_df.rds"))
beta_PD_df <- readRDS(here::here("transformed_data", "beta_PD_df.rds"))
beta_FD_df <- readRDS(here::here("transformed_data", "beta_FD_df.rds"))


# Plot and save for TD:
plot_TD <- plot.boxplots.beta(beta_df = beta_TD_df, metric = "TD")

# Plot and save for PD:
plot_PD <- plot.boxplots.beta(beta_df = beta_PD_df, metric = "PD")

# Plot and save for FD:
plot_FD <- plot.boxplots.beta(beta_df = beta_PD_df, metric = "FD")




# Test

# change facets name to have different facets:
# H0: not stat different
beta_df <- beta_FD_df

# Is the variation in N'Gouja significantly superior to the variation in B?
wilcox.test(beta_df[which(beta_df[, "site_nm"] == "N'Gouja"), "beta"],
            beta_df[which(beta_df[, "site_nm"] == "Boueni"), "beta"],
            alternative = "greater")


# Is the interdays variation significantly different in NG than in B?
wilcox.test(beta_df[which(beta_df[, "same_video"] == TRUE & beta_df[, "same_site"] == TRUE
                          & beta_df[, "site_nm"] == "N'Gouja"), "beta"],

            beta_df[which(beta_df[, "same_video"] == TRUE & beta_df[, "same_site"] == TRUE
                          & beta_df[, "site_nm"] == "Boueni"), "beta"])
# pvalue 3.66*10e-9 -> reject H0 so stats different

# Is the interday beta superior in N'Gouja than in Boueni? yes
wilcox.test(beta_df[which(beta_df[, "same_video"] == TRUE & beta_df[, "same_site"] == TRUE
                          & beta_df[, "site_nm"] == "N'Gouja"), "beta"],

            beta_df[which(beta_df[, "same_video"] == TRUE & beta_df[, "same_site"] == TRUE
                          & beta_df[, "site_nm"] == "Boueni"), "beta"],
            alternative = "greater")

# Is the intraday variation significantly different in NG than in B?
# H0: not stat different
wilcox.test(beta_df[which(beta_df[, "same_day"] == TRUE
                          & beta_df[, "site_nm"] == "N'Gouja"), "beta"],

            beta_df[which(beta_df[, "same_day"] == TRUE
                          & beta_df[, "site_nm"] == "Boueni"), "beta"])
# pvalue < 2.2*10e-16 -> reject H0 so stats different

# Is the intraday beta superior in N'Gouja than in Boueni? yes
wilcox.test(beta_df[which(beta_df[, "same_day"] == TRUE
                          & beta_df[, "site_nm"] == "N'Gouja"), "beta"],

            beta_df[which(beta_df[, "same_day"] == TRUE
                          & beta_df[, "site_nm"] == "Boueni"), "beta"],
            alternative = "greater")



# Is the interday variation significantly superior to the intraday ?
# sites pooled
# H0: not stat different
wilcox.test(beta_df[which(beta_df[, "same_video"] == TRUE & beta_df[, "same_site"] == TRUE), "beta"],

            beta_df[which(beta_df[, "same_day"] == TRUE), "beta"],
            alternative = "less")

# yes sites inter > intra when sites pooled

# N'Gouja only
# H0: not stat different
wilcox.test(beta_df[which(beta_df[, "same_video"] == TRUE & beta_df[, "same_site"] == TRUE &
                            beta_df[, "site_nm"] == "N'Gouja"), "beta"],

            beta_df[which(beta_df[, "same_day"] == TRUE &
                            beta_df[, "site_nm"] == "N'Gouja"), "beta"],
            alternative = "greater")

# yes inter > intra in N'Gouja


# Boueni only
# H0: not stat different
wilcox.test(beta_df[which(beta_df[, "same_video"] == TRUE & beta_df[, "same_site"] == TRUE &
                            beta_df[, "site_nm"] == "Boueni"), "beta"],

            beta_df[which(beta_df[, "same_day"] == TRUE &
                            beta_df[, "site_nm"] == "Boueni"), "beta"],
            alternative = "greater")

# yes inter > intra in Boueni


# Step 5: PERMDISP on beta between videos ####


# Call data:
beta_TD_df <- readRDS(here::here("transformed_data", "beta_TD_df.rds"))
beta_PD_df <- readRDS(here::here("transformed_data", "beta_PD_df.rds"))
beta_FD_df <- readRDS(here::here("transformed_data", "beta_FD_df.rds"))



# only keep inter and intra comparisons:
beta_facet_df <- beta_TD_df

beta_facet_df <- beta_facet_df[which((beta_facet_df$same_site == TRUE & beta_facet_df$same_video == TRUE) |
                          (beta_facet_df$same_day == TRUE)), ]
rownames(beta_facet_df) <- paste0("pair", sep = "_", c(1:nrow(beta_facet_df)))
# ok it has the right number of rows: 3366 = 198 (interday) + 3168 (intraday) ...
# ... 198 = 33 videos * 3 days combinations * 2 sites
# ... 3168 = combination of two videos on 33 videos * 3 days = 1056 * 3

beta_facet_df$site_nm <- as.factor(beta_facet_df$site_nm)


# Compute the permdisp test for video beta TD:

## Get a distance matrix:
dist <- reshape2::acast(beta_facet_df[, c(1,2,3)], x1 ~ x2, value.var='beta', margins=FALSE)
dist <- as.dist(dist)

## Compute the dispersion:
dispersion <- vegan::betadisper(dist, group = beta_facet_df$site_nm)
pmod <- vegan::permutest(dispersion, pairwise = TRUE, permutations = 99)
plot(dispersion, hull=TRUE, ellipse=TRUE)

# permanova:
permanova_results <- vegan::adonis(beta_all_small_df[, c(3, 8, 9)] ~ beta_all_small_df$site_nm, method="bray",perm = 99)


# plot:

## add to the coordinates dataframe the columns to add colors on the plot:
coordinates_pairs <- cbind(coordinates_pairs, beta_all_small_df$same_day,
                           beta_all_small_df$same_video,
                           beta_all_small_df$same_site,
                           beta_all_small_df$site_nm)
colnames(coordinates_pairs) <- c("PC1", "PC2", "PC3", "same_day", "same_video",
                                 "same_site", "site_nm")

## add column if intraday:
coordinates_pairs$intraday <- rep("no", nrow(coordinates_pairs))
for (i in (1:nrow(coordinates_pairs))){

  if (coordinates_pairs$same_day[i] == TRUE) {
    coordinates_pairs[i, "intraday"] <- "yes"
  }

}

## add column if interday:
coordinates_pairs$interday <- rep("no", nrow(coordinates_pairs))
for (i in (1:nrow(coordinates_pairs))){

  if (coordinates_pairs$same_video[i] == TRUE & coordinates_pairs$same_site[i] == TRUE) {
    coordinates_pairs[i, "interday"] <- "yes"
  }

}


pcoa_plot <- ggplot2::ggplot() +

  # add interday dots in black:
  ggplot2::geom_point(data = coordinates_pairs[which(coordinates_pairs$interday == "yes"), ],
                      ggplot2::aes(x = PC1, y = PC2)) +

  # add N'Gouja intra day plots:
  ggplot2::geom_point(data = coordinates_pairs[which(coordinates_pairs$intraday == "yes" &
                                                     coordinates_pairs$site_nm == "N'Gouja"), ],
                      ggplot2::aes(x = PC1, y = PC2), color = "#80cdc1", alpha = 0.6) +

  # add Boueni intra day plots:
  ggplot2::geom_point(data = coordinates_pairs[which(coordinates_pairs$intraday == "yes" &
                                                       coordinates_pairs$site_nm == "Boueni"), ],
                      ggplot2::aes(x = PC1, y = PC2), color = "#bf812d", alpha = 0.6) +

  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                          colour = "white"),
                 panel.grid.major = ggplot2::element_line(colour = "grey90"))




# Step 6: Plot beta intra (all days separatly) and inter days (between all pairs of days) ####


# Call data:
beta_TD_df <- readRDS(here::here("transformed_data", "beta_TD_df.rds"))
beta_PD_df <- readRDS(here::here("transformed_data", "beta_PD_df.rds"))
beta_FD_df <- readRDS(here::here("transformed_data", "beta_FD_df.rds"))

# Add intraday and interday column:


