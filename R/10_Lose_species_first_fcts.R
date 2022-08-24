###############################################################################
##
## Script to compute and plot the evolution of FD and PD when loosing ...
## ... rarest/most common/random species first
##
## 10_Lose_species_first_fcts.R
##
## 23/08/2022
##
## Camille Magneville
##
###############################################################################





#' Title
#'
#' @param rarcom_df
#'
#' @return
#' @export
#'
#' @examples


lose.species.div.plot <- function(rarcom_df_site,
                                  sp_faxes_coord,
                                  asb_sp_site) {


  # LOSE RAREST FIRST ####

  # order the rarcom_df_site based on the perc_vid_occ column: increasing
  rarest_rarcom_df <- rarcom_df_site[order(rarcom_df_site$perc_vid_occ), ]

  # create a dataframe: columns -> species & rows -> length species
  species <- rarest_rarcom_df$species_nm
  rarest_final_df <- as.data.frame(matrix(1,
                                          nrow = length(species) + 1,
                                          ncol = length(species)))
  colnames(rarest_final_df) <- species


  # loop on each species: from the rarest to the most common:
  # first row has all the species
  for (i in (1:nrow(rarest_rarcom_df))) {

    nm_sp <- rarest_rarcom_df[i, "species_nm"]

    # put a 0 on i row and colum = sp_nm:
    rarest_final_df[c((i+1):nrow(rarest_final_df)), nm_sp] <- 0

  }

  # rename rownames:
  rownames(rarest_final_df) <- paste0("lose", sep = "_", "sp", sep = "_",
                                      c(0:(nrow(rarest_final_df) - 1)))


  # compute relative phylogenetic diversity:

  phylo_rarest_final_df <- rarest_final_df

  phylo_rarest_final_df <- dplyr::rename(phylo_rarest_final_df, "Gomphosus_varius" = "Gomphosus_caeruleus")
  phylo_rarest_final_df <- dplyr::rename(phylo_rarest_final_df, "Scolopsis_bilineata" = "Scolopsis_frenata")
  phylo_rarest_final_df <- dplyr::rename(phylo_rarest_final_df, "Scolopsis_bimaculata" = "Scolopsis_ghanam")
  phylo_rarest_final_df <- dplyr::rename(phylo_rarest_final_df, "Cetoscarus_bicolor" = "Cetoscarus_ocellatus")
  phylo_rarest_final_df <- dplyr::rename(phylo_rarest_final_df, "Scarus_altipinnis" = "Scarus_falcipinnis")
  phylo_rarest_final_df <- dplyr::rename(phylo_rarest_final_df, "Scarus_oviceps" = "Scarus_scaber")
  phylo_rarest_final_df <- dplyr::rename(phylo_rarest_final_df, "Tylosurus_crocodilus_crocodilus" = "Tylosurus_crocodilus")
  phylo_rarest_final_df <- dplyr::rename(phylo_rarest_final_df, "Chlorurus_microrhinos" = "Chlorurus_strongylocephalus")
  phylo_rarest_final_df <- dplyr::rename(phylo_rarest_final_df, "Ctenochaetus_striatus" = "Ac_Cten_dark")


  asb_sp_site <- dplyr::rename(asb_sp_site, "Gomphosus_varius" = "Gomphosus_caeruleus")
  asb_sp_site <- dplyr::rename(asb_sp_site, "Scolopsis_bilineata" = "Scolopsis_frenata")
  asb_sp_site <- dplyr::rename(asb_sp_site, "Scolopsis_bimaculata" = "Scolopsis_ghanam")
  asb_sp_site <- dplyr::rename(asb_sp_site, "Cetoscarus_bicolor" = "Cetoscarus_ocellatus")
  asb_sp_site <- dplyr::rename(asb_sp_site, "Scarus_altipinnis" = "Scarus_falcipinnis")
  asb_sp_site <- dplyr::rename(asb_sp_site, "Scarus_oviceps" = "Scarus_scaber")
  asb_sp_site <- dplyr::rename(asb_sp_site, "Tylosurus_crocodilus_crocodilus" = "Tylosurus_crocodilus")
  asb_sp_site <- dplyr::rename(asb_sp_site, "Chlorurus_microrhinos" = "Chlorurus_strongylocephalus")
  asb_sp_site <- dplyr::rename(asb_sp_site, "Canthigaster_coronata" = "Canthigaster_cyanospilota")
  asb_sp_site <- dplyr::rename(asb_sp_site, "Labropsis_australis" = "Labropsis_xanthonota")
  asb_sp_site <- dplyr::rename(asb_sp_site, "Ctenochaetus_striatus" = "Ac_Cten_dark")

  sp_nm_all <- colnames(asb_sp_site)

  # fill 1 on the asb_sp_site first line so can compute the maximal PD of the global tree:
  asb_sp_site[1, ] <- rep(1, ncol(asb_sp_site))

  # compute global phylogeny:
  phylo <- fishtree::fishtree_phylogeny(species = sp_nm_all)

  # compute Faith's PD for each assemblage where loose rarest species first:
  PD_rarest <- picante::pd(phylo_rarest_final_df, phylo, include.root=FALSE)
  PD_max <- picante::pd(asb_sp_site[1, ], phylo, include.root=FALSE)

  PD_rarest$PD <- PD_rarest$PD / PD_max$PD

  PD_rares <- PD_rarest$PD
  PD_rares[c(127, 128)] <- c(0, 0)


  # compute FRic (only for assemblages with more than 5 species):
  alpha_fd_indices <- mFD::alpha.fd.multidim(
    sp_faxes_coord   = as.matrix(sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4", "PC5")]),
    asb_sp_w         = as.matrix(rarest_final_df[c(1:122), ]),
    ind_vect         = c("fdis", "fric",
                         "fspe"),
    scaling          = TRUE,
    check_input      = TRUE,
    details_returned = TRUE)

  # asb 121 has NA in FRic...? species in the same FE, not enough points:
  FRic_rares <- alpha_fd_indices$functional_diversity_indices$fric
  FRic_rares[122] <- 0
  FRic_rares <- c(FRic_rares, rep(0, 6))

  FDis_rares <- alpha_fd_indices$functional_diversity_indices$fdis
  FDis_rares <- c(FDis_rares, rep(0, 6))

  FSpe_rares <- alpha_fd_indices$functional_diversity_indices$fspe
  FSpe_rares <- c(FSpe_rares, rep(0, 6))


  # create df with values:
  plot_fd_pd_df <- cbind(rownames(rarest_final_df),
                           PD_rares,
                           FRic_rares,
                           FDis_rares,
                           FSpe_rares)
  colnames(plot_fd_pd_df)[1] <- "Species_loss"



  # LOSE MOST COMMON FIRST ####

  # order the rarcom_df_site based on the perc_vid_occ column: decreasing
  common_rarcom_df <- rarcom_df_site[order(rarcom_df_site$perc_vid_occ,
                                           decreasing = TRUE), ]

  # create a dataframe: columns -> species & rows -> length species
  species <- common_rarcom_df$species_nm
  common_final_df <- as.data.frame(matrix(1,
                                          nrow = length(species) + 1,
                                          ncol = length(species)))
  colnames(common_final_df) <- species


  # loop on each species: from the rarest to the most common:
  # first row has all the species
  for (i in (1:nrow(common_rarcom_df))) {

    nm_sp <- common_rarcom_df[i, "species_nm"]

    # put a 0 on i row and colum = sp_nm:
    common_final_df[c((i+1):nrow(common_final_df)), nm_sp] <- 0

  }

  # rename rownames:
  rownames(common_final_df) <- paste0("lose", sep = "_", "sp", sep = "_",
                                      c(0:(nrow(common_final_df) - 1)))


  # compute relative phylogenetic diversity:

  phylo_common_final_df <- common_final_df

  phylo_common_final_df <- dplyr::rename(phylo_common_final_df, "Gomphosus_varius" = "Gomphosus_caeruleus")
  phylo_common_final_df <- dplyr::rename(phylo_common_final_df, "Scolopsis_bilineata" = "Scolopsis_frenata")
  phylo_common_final_df <- dplyr::rename(phylo_common_final_df, "Scolopsis_bimaculata" = "Scolopsis_ghanam")
  phylo_common_final_df <- dplyr::rename(phylo_common_final_df, "Cetoscarus_bicolor" = "Cetoscarus_ocellatus")
  phylo_common_final_df <- dplyr::rename(phylo_common_final_df, "Scarus_altipinnis" = "Scarus_falcipinnis")
  phylo_common_final_df <- dplyr::rename(phylo_common_final_df, "Scarus_oviceps" = "Scarus_scaber")
  phylo_common_final_df <- dplyr::rename(phylo_common_final_df, "Tylosurus_crocodilus_crocodilus" = "Tylosurus_crocodilus")
  phylo_common_final_df <- dplyr::rename(phylo_common_final_df, "Chlorurus_microrhinos" = "Chlorurus_strongylocephalus")
  phylo_common_final_df <- dplyr::rename(phylo_common_final_df, "Ctenochaetus_striatus" = "Ac_Cten_dark")


  # compute Faith's PD for each assemblage where loose common species first:
  PD_common <- picante::pd(phylo_common_final_df, phylo, include.root=FALSE)
  PD_max <- picante::pd(asb_sp_site[1, ], phylo, include.root=FALSE)

  PD_common$PD <- PD_common$PD / PD_max$PD

  PD_commons <- PD_common$PD
  PD_commons[c(127, 128)] <- c(0, 0)


  # compute FRic (only for assemblages with more than 5 species):
  alpha_fd_indices <- mFD::alpha.fd.multidim(
    sp_faxes_coord   = as.matrix(sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4", "PC5")]),
    asb_sp_w         = as.matrix(common_final_df[c(1:122), ]),
    ind_vect         = c("fdis", "fric",
                         "fspe"),
    scaling          = TRUE,
    check_input      = TRUE,
    details_returned = TRUE)

  # asb 121 has NA in FRic...? species in the same FE, not enough points:
  FRic_commons <- alpha_fd_indices$functional_diversity_indices$fric
  FRic_commons[122] <- 0
  FRic_commons <- c(FRic_commons, rep(0, 6))

  FDis_commons <- alpha_fd_indices$functional_diversity_indices$fdis
  FDis_commons <- c(FDis_commons, rep(0, 6))

  FSpe_commons <- alpha_fd_indices$functional_diversity_indices$fspe
  FSpe_commons <- c(FSpe_commons, rep(0, 6))


  # create df with values:
  plot_fd_pd_df <- cbind(plot_fd_pd_df,
                         PD_commons,
                         FRic_commons,
                         FDis_commons,
                         FSpe_commons)


  # LOSE RANDOM FIRST : do 100 iteration of random: ####

  n <- 1

  # create a dataframe which will contains PD and FD values for the 100 iterations
  random_iteration_df <- as.data.frame(matrix(nrow = 1, ncol = 4))
  colnames(random_iteration_df) <- c("metric", "value", "Species_loss", "iter_nb")


  while(n <= 20) {


    # create a list of species name:
    sp_nm_list <- rarcom_df_site$species_nm

    # order the rarcom_df_site
    random_rarcom_df <- rarcom_df_site

    # create a dataframe: columns -> species & rows -> length species
    species <- random_rarcom_df$species_nm
    random_final_df <- as.data.frame(matrix(1,
                                            nrow = length(species) + 1,
                                            ncol = length(species)))
    colnames(random_final_df) <- species


    # loop on each species:
    # first row has all the species
    for (i in (1:nrow(random_rarcom_df))) {

      # random number between 1:nb of species:
      randnb <- floor(runif(1, 1, length(species)))

      nm_sp <- species[randnb]

      species <- species[which(species != nm_sp)]

      # put a 0 on i row and colum = sp_nm:
      random_final_df[c((i+1):nrow(random_final_df)), nm_sp] <- 0

    }

    # rename rownames:
    rownames(random_final_df) <- paste0("lose", sep = "_", "sp", sep = "_",
                                        c(0:(nrow(random_final_df) - 1)))


    # compute relative phylogenetic diversity:

    phylo_random_final_df <- random_final_df

    phylo_random_final_df <- dplyr::rename(phylo_random_final_df, "Gomphosus_varius" = "Gomphosus_caeruleus")
    phylo_random_final_df <- dplyr::rename(phylo_random_final_df, "Scolopsis_bilineata" = "Scolopsis_frenata")
    phylo_random_final_df <- dplyr::rename(phylo_random_final_df, "Scolopsis_bimaculata" = "Scolopsis_ghanam")
    phylo_random_final_df <- dplyr::rename(phylo_random_final_df, "Cetoscarus_bicolor" = "Cetoscarus_ocellatus")
    phylo_random_final_df <- dplyr::rename(phylo_random_final_df, "Scarus_altipinnis" = "Scarus_falcipinnis")
    phylo_random_final_df <- dplyr::rename(phylo_random_final_df, "Scarus_oviceps" = "Scarus_scaber")
    phylo_random_final_df <- dplyr::rename(phylo_random_final_df, "Tylosurus_crocodilus_crocodilus" = "Tylosurus_crocodilus")
    phylo_random_final_df <- dplyr::rename(phylo_random_final_df, "Chlorurus_microrhinos" = "Chlorurus_strongylocephalus")
    phylo_random_final_df <- dplyr::rename(phylo_random_final_df, "Ctenochaetus_striatus" = "Ac_Cten_dark")

    sp_nm_all <- colnames(asb_sp_site)

    # compute global phylogeny:
    phylo <- fishtree::fishtree_phylogeny(species = sp_nm_all)

    # compute Faith's PD for each assemblage where loose random species first:
    PD_random <- picante::pd(phylo_random_final_df, phylo, include.root=FALSE)
    PD_max <- picante::pd(asb_sp_site[1, ], phylo, include.root=FALSE)

    PD_random$PD <- PD_random$PD / PD_max$PD

    PD_randoms <- PD_random$PD
    PD_randoms[c(127, 128)] <- c(0, 0)


    # compute FRic (only for assemblages with more than 5 species):
    alpha_fd_indices <- mFD::alpha.fd.multidim(
      sp_faxes_coord   = as.matrix(sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4", "PC5")]),
      asb_sp_w         = as.matrix(random_final_df[c(1:122), ]),
      ind_vect         = c("fdis", "fric",
                           "fspe"),
      scaling          = TRUE,
      check_input      = TRUE,
      details_returned = TRUE)

    # asb 121 has NA in FRic...? species in the same FE, not enough points:
    FRic_randoms <- alpha_fd_indices$functional_diversity_indices$fric
    FRic_randoms[122] <- 0
    FRic_randoms <- c(FRic_randoms, rep(0, 6))

    FDis_randoms <- alpha_fd_indices$functional_diversity_indices$fdis
    FDis_randoms <- c(FDis_randoms, rep(0, 6))

    FSpe_randoms <- alpha_fd_indices$functional_diversity_indices$fspe
    FSpe_randoms <- c(FSpe_randoms, rep(0, 6))


    # add values of the nth iteration to the random df:
    # PD
    PD_df <- cbind(rep("Faith's PD", length(PD_randoms)),
                   PD_randoms,
                   rownames(common_final_df),
                   paste0(rep("iter_", length(PD_randoms)), sep = "", n))
    colnames(PD_df) <- c("metric", "value", "Species_loss", "iter_nb")

    # FRic
    FRic_df <- cbind(rep("FRic", length(FRic_randoms)),
                   FRic_randoms,
                   rownames(common_final_df),
                   paste0(rep("iter_", length(FRic_randoms)), sep = "", n))
    colnames(FRic_df) <- c("metric", "value", "Species_loss", "iter_nb")

    # FDis
    FDis_df <- cbind(rep("FDis", length(FDis_randoms)),
                   FDis_randoms,
                   rownames(common_final_df),
                   paste0(rep("iter_", length(FDis_randoms)), sep = "", n))
    colnames(FDis_df) <- c("metric", "value", "Species_loss", "iter_nb")

    # Fspe
    FSpe_df <- cbind(rep("FSpe", length(FSpe_randoms)),
                   FSpe_randoms,
                   rownames(common_final_df),
                   paste0(rep("iter_", length(FSpe_randoms)), sep = "", n))
    colnames(FSpe_df) <- c("metric", "value", "Species_loss", "iter_nb")


    random_iteration_df <- rbind(random_iteration_df, PD_df, FRic_df,
                                 FDis_df, FSpe_df)


    n <- n + 1

  }


  # PLOT ####

  # build dfs for each plot for rare and common species:
  plot_PD <- plot_fd_pd_df[, c(1, 2, 6)]
  plot_PD <- as.data.frame(plot_PD)
  plot_PD2 <- reshape2::melt(plot_PD,
                 id.vars = c("Species_loss"),
                 variable.name = 'metric', value.name = 'values')
  plot_PD2$metric <- as.character(plot_PD2$metric)
  plot_PD2[which(plot_PD2$metric == "PD_rares"), "metric"] <- "rares"
  plot_PD2[which(plot_PD2$metric == "PD_commons"), "metric"] <- "commons"
  plot_PD2$metric <- as.factor(plot_PD2$metric)
  plot_PD2$ind <- rep("Faith's PD", nrow(plot_PD2))

  plot_FRic <- plot_fd_pd_df[, c(1, 3, 7)]
  plot_FRic <- as.data.frame(plot_FRic)
  plot_FRic2 <- reshape2::melt(plot_FRic,
                             id.vars = c("Species_loss"),
                             variable.name = 'metric', value.name = 'values')
  plot_FRic2$metric <- as.character(plot_FRic2$metric)
  plot_FRic2[which(plot_FRic2$metric == "FRic_rares"), "metric"] <- "rares"
  plot_FRic2[which(plot_FRic2$metric == "FRic_commons"), "metric"] <- "commons"
  plot_FRic2$metric <- as.factor(plot_FRic2$metric)
  plot_FRic2$ind <- rep("FRic", nrow(plot_FRic2))

  plot_FDis <- plot_fd_pd_df[, c(1, 4, 8)]
  plot_FDis <- as.data.frame(plot_FDis)
  plot_FDis2 <- reshape2::melt(plot_FDis,
                               id.vars = c("Species_loss"),
                               variable.name = 'metric', value.name = 'values')
  plot_FDis2$metric <- as.character(plot_FDis2$metric)
  plot_FDis2[which(plot_FDis2$metric == "FDis_rares"), "metric"] <- "rares"
  plot_FDis2[which(plot_FDis2$metric == "FDis_commons"), "metric"] <- "commons"
  plot_FDis2$metric <- as.factor(plot_FDis2$metric)
  plot_FDis2$ind <- rep("FDis", nrow(plot_FDis2))

  plot_FSpe <- plot_fd_pd_df[, c(1, 5, 9)]
  plot_FSpe <- as.data.frame(plot_FSpe)
  plot_FSpe2 <- reshape2::melt(plot_FSpe,
                               id.vars = c("Species_loss"),
                               variable.name = 'metric', value.name = 'values')
  plot_FSpe2$metric <- as.character(plot_FSpe2$metric)
  plot_FSpe2[which(plot_FSpe2$metric == "FSpe_rares"), "metric"] <- "rares"
  plot_FSpe2[which(plot_FSpe2$metric == "FSpe_commons"), "metric"] <- "commons"
  plot_FSpe2$metric <- as.factor(plot_FSpe2$metric)
  plot_FSpe2$ind <- rep("FSpe", nrow(plot_FSpe2))


  # create df for the plot of rare and common species loss:
  plot_all_lose_df <- rbind(plot_PD2, plot_FRic2,
                            plot_FDis2, plot_FSpe2)

  # order species loss column in the rare/common df:
  plot_all_lose_df$Species_loss <- as.factor(plot_all_lose_df$Species_loss)
  plot_all_lose_df$Species_loss <- ordered(plot_all_lose_df$Species_loss,
                                           levels = paste0(rep("lose_sp_", length(rarcom_df_site$species_nm)),
                                       c(0:length(rarcom_df_site$species_nm))))

  # numeric value column in the rare/common df:
  plot_all_lose_df$values <- as.numeric(plot_all_lose_df$values)


  # compute the median value for each metric*sp_loss:
  plot_random_med_df <- as.data.frame(matrix(nrow = 1, ncol = 3))
  colnames(plot_random_med_df) <- c("Species_loss",
                                    "median_value",
                                    "ind")
  ## loop on species loss
  random_iteration_df <- random_iteration_df[-1, ]
  for (i in unique(random_iteration_df$Species_loss)) {


    # loop on indices for a given species_loss:
    for (j in unique(random_iteration_df$metric[which(random_iteration_df$Species_loss == i)])) {

      med_ij <- median(as.numeric(random_iteration_df[which(random_iteration_df$Species_loss == i &
                                                  random_iteration_df$metric == j), "value"]))

      plot_random_med_df[nrow(plot_random_med_df) + 1, ] <- c(i, med_ij, j)

    }

  }

  # order species loss column in the rare/common df:
  plot_random_med_df$Species_loss <- as.factor(plot_random_med_df$Species_loss)
  plot_random_med_df$Species_loss <- ordered(plot_random_med_df$Species_loss,
                                           levels = paste0(rep("lose_sp_", length(rarcom_df_site$species_nm)),
                                                           c(0:length(rarcom_df_site$species_nm))))

  # remove first NA row:
  plot_random_med_df <- plot_random_med_df[-1, ]

  # numeric value column in the rare/common df:
  plot_random_med_df$median_value <- as.numeric(plot_random_med_df$median_value)


  # add median values to the plot_all_lose_df:
  to_add <- cbind(as.character(plot_random_med_df$Species_loss),
                  rep("random", nrow(plot_random_med_df)),
                  plot_random_med_df$median_value,
                  plot_random_med_df$ind)
  colnames(to_add) <- colnames(plot_all_lose_df)

  plot_all_lose_df <- rbind(plot_all_lose_df,
                            to_add)

  plot_all_lose_df$values <- as.numeric(plot_all_lose_df$values)



  # plot rare and common (should add mediam and conf interval of randoms when pb solved):
  plot_lose_sp <- ggplot2::ggplot() +

    ggplot2::geom_line(data = plot_all_lose_df,
                       ggplot2::aes(y = values, x = Species_loss, group = metric,
                                             linetype = metric),
                                size = 0.9,
                                colour = site_color) +

    ggplot2::facet_grid(. ~ ind) +

    ggplot2::scale_linetype_manual(values = c("solid",
                                              "dashed",
                                              "dotted"),
                                   labels = c("Lose common species first",
                                              "Lose rares species first",
                                              "Lose random species first")) +

    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "white"),
                   panel.grid.major.x = ggplot2::element_line(colour = "white"),
                   panel.grid.major.y = ggplot2::element_line(colour = "grey90")) +

    ggplot2::ylab("Diversities values") +

    ggplot2::xlab("Species loss")







}

