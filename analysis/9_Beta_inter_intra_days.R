###############################################################################
##
## Script to compute and plot beta diversities inter and intra days
##
## 9_Beta_inter_intra_days.R
##
## 19/08/2022
##
##
##
###############################################################################



# Step 1: Call data ####


# TD, FD and  PD:
basic_accum_df_clean <- readRDS(here::here("transformed_data", "basic_accumul_df.rds"))

# FD:
sp_faxes_coord <- readRDS(here::here("transformed_data", "sp_faxes_coord.rds"))



# Step 2: Compute beta diversities avec save them ####


# TD:
basic_accum_df <- basic_accum_df_clean
basic_accum_df[, -c(131:134)] <- apply(basic_accum_df[, -c(131:134)], 2, as.numeric)
rownames(basic_accum_df) <- basic_accum_df$video_id
basic_accum_df <- basic_accum_df[, -c(131:134)]

beta_TD <- betapart::beta.pair(basic_accum_df, index.family = "jaccard")
saveRDS(beta_TD, here::here("transformed_data", "beta_TD_videos_dist.rds"))

# PD:

## Change 11 species names which are not in the fishtree phylogeny in the ...
## ... basic_accum_df:

basic_accum_df <- as.data.frame(basic_accum_df)
basic_accum_df <- dplyr::rename(basic_accum_df, "Gomphosus_varius" = "Gomphosus_caeruleus")
# basic_accum_df <- dplyr::rename(basic_accum_df, "Scolopsis_bilineata" = "Scolopsis_frenata")
basic_accum_df <- dplyr::rename(basic_accum_df, "Scolopsis_bimaculata" = "Scolopsis_ghanam")
basic_accum_df <- dplyr::rename(basic_accum_df, "Cetoscarus_bicolor" = "Cetoscarus_ocellatus")
basic_accum_df <- dplyr::rename(basic_accum_df, "Scarus_altipinnis" = "Scarus_falcipinnis")
basic_accum_df <- dplyr::rename(basic_accum_df, "Scarus_oviceps" = "Scarus_scaber")
basic_accum_df <- dplyr::rename(basic_accum_df, "Tylosurus_crocodilus_crocodilus" = "Tylosurus_crocodilus")
# basic_accum_df <- dplyr::rename(basic_accum_df, "Chlorurus_microrhinos" = "Chlorurus_strongylocephalus")
# basic_accum_df <- dplyr::rename(basic_accum_df, "Canthigaster_coronata" = "Canthigaster_cyanospilota")
basic_accum_df <- dplyr::rename(basic_accum_df, "Labropsis_australis" = "Labropsis_xanthonota")
basic_accum_df <- dplyr::rename(basic_accum_df, "Ctenochaetus_striatus" = "Ac_Cten_dark")


sp_nm_all <- colnames(basic_accum_df)

phylo <- fishtree::fishtree_phylogeny(species = sp_nm_all)

# Compute beta PD:
beta_PD <- betapart::phylo.beta.pair(basic_accum_df, phylo, index.family = "jaccard")
saveRDS(beta_PD, here::here("transformed_data", "beta_PD_videos_dist.rds"))


# FD:
basic_accum_df <- basic_accum_df_clean
basic_accum_df[, -c(131:134)] <- apply(basic_accum_df[, -c(131:134)], 2, as.numeric)
rownames(basic_accum_df) <- basic_accum_df$video_id
basic_accum_df <- basic_accum_df[, -c(131:134)]

beta_FD <- mFD::beta.fd.multidim(
   sp_faxes_coord   = as.matrix(sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4", "PC5")]),
   asb_sp_occ       = basic_accum_df,
   check_input      = TRUE,
   beta_family      = c("Jaccard"),
  details_returned = TRUE)
saveRDS(beta_FD, here::here("transformed_data", "beta_FD_videos_dist.rds"))



# Step 3: Compute 3 datatables which beta div values and pairs informations ####


# Here, we compute 3 dataframes: one for each diversity facets. Each contains ...
# ... vid1|vid2|value_beta|same_day|same_vid|same_site|site_nm columns (for FD sp = FE) ...
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
plot_FD <- plot.boxplots.beta(beta_df = beta_FD_df, metric = "FD")




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
# pvalue e-7 -> reject H0 so stats different

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


# Step 5: PERMDISP and PERMANOVA on beta between videos ####


# Call data:
beta_TD_df <- readRDS(here::here("transformed_data", "beta_TD_df.rds"))
beta_PD_df <- readRDS(here::here("transformed_data", "beta_PD_df.rds"))
beta_FD_df <- readRDS(here::here("transformed_data", "beta_FD_df.rds"))

# For information:
# 3366 = 198 (interday) + 3168 (intraday) ...
# ... 198 = 33 videos * 3 days combinations * 2 sites
# ... 3168 = combination of two videos on 33 videos * 3 days = 1056 * 3


# TD :
TD_permdisp <- permdisp.test(beta_facet_df = beta_TD_df)

# PD :
PD_permdisp <- permdisp.test(beta_facet_df = beta_PD_df)

# FD :
FD_permdisp <- permdisp.test(beta_facet_df = beta_FD_df)


# dispersion significant for TD, FD, PD: no permanova. Yet (graphs)


# permanova TD:
# get the df build in the permdisp.test function:
beta_env_df <- TD_permdisp[[2]]
beta_env_df$site_nm <- as.factor(beta_env_df$site_nm)
beta_env_df$day <- as.factor(beta_env_df$day)
beta_env_df$site_day <- as.factor(beta_env_df$site_day)
dist <- TD_permdisp[[3]]

TD_permanova_results <- vegan::adonis(dist ~ site_nm + day,
                                      data = beta_env_df,
                                      perm = 999)
# Significant effect of site and day on PD distances between videos


# permanova FD:
# get the df build in the permdisp.test function:
beta_env_df <- FD_permdisp[[2]]
beta_env_df$site_nm <- as.factor(beta_env_df$site_nm)
beta_env_df$day <- as.factor(beta_env_df$day)
beta_env_df$site_day <- as.factor(beta_env_df$site_day)
dist <- FD_permdisp[[3]]

FD_permanova_results <- vegan::adonis(dist ~ site_nm + day,
                                      data = beta_env_df,
                                      perm = 999)
# Significant effect of site and day on FD distances between videos


# permanova PD:
# get the df build in the permdisp.test function:
beta_env_df <- PD_permdisp[[2]]
beta_env_df$site_nm <- as.factor(beta_env_df$site_nm)
beta_env_df$day <- as.factor(beta_env_df$day)
beta_env_df$site_day <- as.factor(beta_env_df$site_day)
dist <- PD_permdisp[[3]]

PD_permanova_results <- vegan::adonis(dist ~ site_nm + day,
                                      data = beta_env_df,
                                      perm = 999)
# Significant effect of site and day on PD distances between videos


# So: more variability of distances between sites than variab of distances among sites
# So: more variability of distances between days than variab of distances among days


# Step 6: Temporal decay beta on intra day data ####


# Call data:
beta_TD_df <- readRDS(here::here("transformed_data", "beta_TD_df.rds"))
beta_PD_df <- readRDS(here::here("transformed_data", "beta_PD_df.rds"))
beta_FD_df <- readRDS(here::here("transformed_data", "beta_FD_df.rds"))

# plot temporal decay and save it for TD:
temp_decay_TD <- temp.decay(beta_facet_df = beta_TD_df, metric = "TD")
temp_decay_TD <- temp_decay_TD + ggplot2::ggtitle("TD")

# plot temporal decay and save it for PD:
temp_decay_PD <- temp.decay(beta_facet_df = beta_PD_df, metric = "PD")
temp_decay_PD <- temp_decay_TD + ggplot2::ggtitle("PD")

# plot temporal decay and save it for FD:
temp_decay_FD <- temp.decay(beta_facet_df = beta_FD_df, metric = "FD")
temp_decay_FD <- temp_decay_FD + ggplot2::ggtitle("FD")


# patchwork:
temp_decay_all <- (temp_decay_TD + temp_decay_FD + temp_decay_PD + patchwork::plot_spacer()) +
  patchwork::plot_layout(byrow = TRUE, heights = c(1, 1), widths = c(1, 1),
                         ncol = 2, nrow = 2, guides = "collect")

ggplot2::ggsave(filename = here::here("outputs", "Temp_decay_all.pdf"),
                plot = temp_decay_all,
                device = "pdf",
                scale = 1,
                height = 9000,
                width = 14000,
                units = "px",
                dpi = 800)


# Step 7: Plot TD, FD and PD based on video dissimilarities ####


# Call data:
beta_TD_df <- readRDS(here::here("transformed_data", "beta_TD_df.rds"))
beta_PD_df <- readRDS(here::here("transformed_data", "beta_PD_df.rds"))
beta_FD_df <- readRDS(here::here("transformed_data", "beta_FD_df.rds"))


# TD - Get the coordinates of videos along PCoA axis:
TD_permdisp <- permdisp.test(beta_facet_df = beta_TD_df)
coord_TD <- as.data.frame(TD_permdisp[[4]]$sites)

# PD :
PD_permdisp <- permdisp.test(beta_facet_df = beta_PD_df)
coord_PD <- as.data.frame(PD_permdisp[[4]]$sites)

# FD :
FD_permdisp <- permdisp.test(beta_facet_df = beta_FD_df)
coord_FD <- as.data.frame(FD_permdisp[[4]]$sites)

# Add a column to tell whether videos belong to N'Gouja or Boueni for 3 coord dfs:
coord_TD$site <- rep("N'Gouja", nrow(coord_TD))
coord_FD$site <- rep("N'Gouja", nrow(coord_FD))
coord_PD$site <- rep("N'Gouja", nrow(coord_PD))

# Fill this new column:
# TD
for (i in (1:nrow(coord_TD))) {

  nm_vid <- rownames(coord_TD)[i]

  if (stringr::str_detect(nm_vid, "04-11-2019") |
      stringr::str_detect(nm_vid, "06-11-2019") |
      stringr::str_detect(nm_vid, "09-11-2019")) {

    coord_TD$site[i] <- "Boueni"
  }

}

# FD
for (i in (1:nrow(coord_FD))) {

  nm_vid <- rownames(coord_FD)[i]

  if (stringr::str_detect(nm_vid, "04-11-2019") |
      stringr::str_detect(nm_vid, "06-11-2019") |
      stringr::str_detect(nm_vid, "09-11-2019")) {

    coord_FD$site[i] <- "Boueni"
  }

}

# PD
for (i in (1:nrow(coord_PD))) {

  nm_vid <- rownames(coord_PD)[i]

  if (stringr::str_detect(nm_vid, "04-11-2019") |
      stringr::str_detect(nm_vid, "06-11-2019") |
      stringr::str_detect(nm_vid, "09-11-2019")) {

    coord_PD$site[i] <- "Boueni"
  }

}



# plot TD:
TD_pcoa_plot <- ggplot2::ggplot() +

  # add interday dots in black:
  ggplot2::geom_point(data = coord_TD,
                      ggplot2::aes(x = PCoA1, y = PCoA2, color = site)) +

  ggplot2::scale_colour_manual(values = c("#bf812d",
                                          "#80cdc1"),
                               name = "Site") +

  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                          colour = "white"),
                 panel.grid.major = ggplot2::element_line(colour = "grey90"))

# plot FD:
FD_pcoa_plot <- ggplot2::ggplot() +

  # add interday dots in black:
  ggplot2::geom_point(data = coord_FD,
                      ggplot2::aes(x = PCoA1, y = PCoA2, color = site)) +

  ggplot2::scale_colour_manual(values = c("#bf812d",
                                          "#80cdc1"),
                               name = "Site") +

  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                          colour = "white"),
                 panel.grid.major = ggplot2::element_line(colour = "grey90"))

# plot PD:
PD_pcoa_plot <- ggplot2::ggplot() +

  # add interday dots in black:
  ggplot2::geom_point(data = coord_PD,
                      ggplot2::aes(x = PCoA1, y = PCoA2, color = site)) +

  ggplot2::scale_colour_manual(values = c("#bf812d",
                                          "#80cdc1"),
                               name = "Site") +

  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                          colour = "white"),
                 panel.grid.major = ggplot2::element_line(colour = "grey90"))

# Add plots together:
TD_pcoa_plot <- TD_pcoa_plot +
  ggplot2::ggtitle("TD")

FD_pcoa_plot <- FD_pcoa_plot +
  ggplot2::ggtitle("FD")

PD_pcoa_plot <- PD_pcoa_plot +
  ggplot2::ggtitle("PD")

pcoa_all <- (TD_pcoa_plot + FD_pcoa_plot + PD_pcoa_plot) +
  patchwork::plot_layout(byrow = TRUE, heights = c(1, 1), widths = c(1, 1),
                         ncol = 3, nrow = 1, guides = "collect")

ggplot2::ggsave(filename = here::here("outputs", "pcoa_all.pdf"),
                plot = pcoa_all,
                device = "pdf",
                scale = 1,
                height = 5000,
                width = 11000,
                units = "px",
                dpi = 800)
