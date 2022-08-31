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
beta_df <- beta_TD_df

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

# For information:
# 3366 = 198 (interday) + 3168 (intraday) ...
# ... 198 = 33 videos * 3 days combinations * 2 sites
# ... 3168 = combination of two videos on 33 videos * 3 days = 1056 * 3


# TD - change beta_facet_df for other facets:
beta_facet_df <- beta_PD_df
beta_facet_df$site_nm <- as.factor(beta_facet_df$site_nm)
beta_facet_df$x1 <- as.character(beta_facet_df$x1)
beta_facet_df$x2 <- as.character(beta_facet_df$x2)


## Get a distance matrix:
dist <- as.data.frame(matrix(nrow = length(unique(c(beta_facet_df$x1,
                                                    beta_facet_df$x2))),
                             ncol =  length(unique(c(beta_facet_df$x1,
                                                     beta_facet_df$x2)))))
colnames(dist) <- sort(unique(c(beta_facet_df$x1,
                                beta_facet_df$x2)))

rownames(dist) <- sort(unique(c(beta_facet_df$x1,
                                beta_facet_df$x2)))

for (i in (rownames(dist))) {

  for (j in (colnames(dist))) {

    # if the case has not been filled yet:
    if (is.na(dist[which(rownames(dist) == i),
                   which(colnames(dist) == j)])) {

        # if it is the same video:
        if (i == j) {
          value <- 0
        }

        else {

          # if first video in x1 column:
          if (i %in% unique(beta_facet_df$x1)) {
            # if the second video is in the x2 column:
            if (j %in% unique(beta_facet_df[which(beta_facet_df$x1 == i), "x2"])) {
              value <- beta_facet_df[which(beta_facet_df$x1 == i & beta_facet_df$x2 == j),
                                     "beta"]
            }
          }

          # if first video in x2 column:
          if (i %in% unique(beta_facet_df$x2)) {
            # if the second video is in the second column:
            if (j %in% unique(beta_facet_df[which(beta_facet_df$x2 == i), "x1"])) {
              value <- beta_facet_df[which(beta_facet_df$x1 == j & beta_facet_df$x2 == i),
                                     "beta"]
            }
          }



        }

        dist[which(rownames(dist) == i),
             which(colnames(dist) == j)] <- value

        dist[which(rownames(dist) == j),
             which(colnames(dist) == i)] <- value
      }

    }
}

dist <- as.dist(dist)

## Create a new df with variable to test for each video:
beta_env_df <- as.data.frame(matrix(nrow = length(unique(c(beta_facet_df$x1,
                                                           beta_facet_df$x2))),
                                    ncol = 4))
colnames(beta_env_df) <- c("video_nm", "site", "day", "site_day")

### Fill the columns:
# video_nm:
beta_env_df$video_nm <- unique(c(beta_facet_df$x1, beta_facet_df$x2))
# day:
beta_env_df$day <- stringr::str_sub(beta_env_df$video_nm, 5, 14)
# site:
for (i in (1:nrow(beta_env_df))) {

  if (grepl("03-11-2019", beta_env_df$day[i]) | grepl("05-11-2019", beta_env_df$day[i]) |
                                               grepl("08-11-2019", beta_env_df$day[i])) {
    beta_env_df$site[i] <- "N'Gouja"
  }

  if (grepl("04-11-2019", beta_env_df$day[i]) | grepl("06-11-2019", beta_env_df$day[i]) |
      grepl("09-11-2019", beta_env_df$day[i])) {
    beta_env_df$site[i] <- "Boueni"
  }

}
# day_site:
beta_env_df$site_day <- paste0(beta_env_df$site, sep = "_",
                               beta_env_df$day)
beta_env_df$site_day <- as.factor(beta_env_df$site_day)


## Compute the dispersion:
dispersion <- vegan::betadisper(dist, group = beta_env_df$site_day)
pmod <- vegan::permutest(dispersion, pairwise = TRUE, permutations = 99)
plot(dispersion, hull=TRUE, ellipse=TRUE)

# dispersion significant for TD: no permanova


# permanova:
permanova_results <- vegan::adonis(beta_all_small_df[, c(3, 8, 9)] ~ beta_env_df$site_day, method="bray", perm = 99)


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




