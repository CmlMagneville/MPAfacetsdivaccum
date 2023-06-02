###############################################################################
##
## Script which gathers functions to find rare/medium/common species ...
## ... in both sites and where they  are in the functional space and ...
## .. the phylogenetic tree
##
## 8_Rarity_Commonness_fcts.R
##
## 12/08/2022
##
##
##
###############################################################################






#' Title
#'
#' @param basic_accum_df
#'
#' @return
#' @export
#'
#' @examples


rarcom.computation <- function(basic_accum_df) {


  # Create a new dataf=table which gathers site, species and occ in terms of ...
  # ... video in which each species occurs:
  rarcom_df <- as.data.frame(matrix(ncol = 3, nrow = 1))
  colnames(rarcom_df) <- c("site", "species_nm", "vid_occ_nb")


  # numeric format to all species:
  basic_accum_df[, which(! colnames(basic_accum_df) %in%
        c("video_nb", "day", "vid_id", "site"))] <- apply(basic_accum_df[,
                                          which(! colnames(basic_accum_df) %in%
                                      c("video_nb", "day", "vid_id", "site"))],
                                      2, as.numeric)


  # get the names of species in each site:
  NG_data <- basic_accum_df[which(basic_accum_df$site == "N'Gouja"),
                            which(! colnames(basic_accum_df) %in%
                                    c("video_nb", "day", "vid_id", "site"))]
  B_data <- basic_accum_df[which(basic_accum_df$site == "Boueni"),
                           which(! colnames(basic_accum_df) %in%
                                   c("video_nb", "day", "vid_id", "site"))]

  sp_NG <- names(NG_data[which(apply(NG_data, 2, sum) != 0)]) #127 species ok
  sp_B <- names(B_data[which(apply(B_data, 2, sum) != 0)]) #112 species ok


  # Fill it using the basic_accum_df:

  ## loop on each species:
  for (j in (1:ncol(basic_accum_df[, which(! colnames(basic_accum_df) %in%
                                               c("video_nb", "day", "vid_id", "site"))]))) {


    ## if the studied species is in NG:
    if (colnames(basic_accum_df)[j] %in% sp_NG) {

      sp_nm <- colnames(basic_accum_df)[j]
      site <- "N'Gouja"
      vid_occ_nb <- sum(NG_data[which(names(NG_data) == sp_nm)])

      rarcom_df[nrow(rarcom_df) + 1, ] <- c(site, sp_nm, vid_occ_nb)

    }

    ## if the studied species is in the Boueni:
    if (colnames(basic_accum_df)[j] %in% sp_B) {

      sp_nm <- colnames(basic_accum_df)[j]
      site <- "Boueni"
      vid_occ_nb <- sum(B_data[which(names(B_data) == sp_nm)])

      rarcom_df[nrow(rarcom_df) + 1, ] <- c(site, sp_nm, vid_occ_nb)

    }

  } # end loop on species

  # remove the first row withNA:
  rarcom_df <- rarcom_df[-1, ]

  # Add a column to the rarcom_df with indicate if the sp is only seen in one ...
  # ... or both sites:
  NG_sp_nm <- rarcom_df[which(rarcom_df$site == "N'Gouja"), "species_nm"]
  B_sp_nm <- rarcom_df[which(rarcom_df$site == "Boueni"), "species_nm"]

  rarcom_df$site_presence <- rep(0, nrow(rarcom_df))

  for (i in (1:nrow(rarcom_df))) {

    sp_nm <- rarcom_df[i, "species_nm"]

    if (sp_nm %in% NG_sp_nm & sp_nm %in% B_sp_nm) {
      rarcom_df$site_presence[i] <- "both"
    }

    else {
      if (sp_nm %in% NG_sp_nm) {
        rarcom_df$site_presence[i] <- "N'Gouja only"
      }

      if (sp_nm %in% B_sp_nm) {
        rarcom_df$site_presence[i] <- "Boueni only"
      }
    }


  }

  # add rarity information:
  # rare =< 25% of total videos, medium 25-50, common >= 50%
  rarcom_df$vid_occ_nb <- as.numeric(rarcom_df$vid_occ_nb)
  rarcom_df$perc_vid_occ <- (rarcom_df$vid_occ_nb / 99)*100

  rarcom_df$rarity <- rep(0, nrow(rarcom_df))

  for (i in (rownames(rarcom_df))) {

    if (rarcom_df[i, "perc_vid_occ"] <= 5) {
      rarcom_df[i, "rarity"] <- "super rare"
    }

    if (rarcom_df[i, "perc_vid_occ"] > 5 & rarcom_df[i, "perc_vid_occ"] < 25) {
      rarcom_df[i, "rarity"] <- "rare"
    }

    if (rarcom_df[i, "perc_vid_occ"] >= 25) {
      rarcom_df[i, "rarity"] <- "common"
    }


  }


  return(rarcom_df)

}


#####


#' Title
#'
#' @param rarcom_df
#' @param sites_colors
#'
#' @return
#' @export
#'
#' @example

plot.rarcom <- function(rarcom_df, sites_colors) {


  # right format:
  rarcom_df$site <- as.factor(rarcom_df$site)
  rarcom_df$site_presence <- as.factor(rarcom_df$site_presence)
  rarcom_df$vid_occ_nb <- as.numeric(rarcom_df$vid_occ_nb)

  # rename species:
  rarcom_df$species_nm <- gsub("_", " ", rarcom_df$species_nm)
  rarcom_df[c(2, 3), "species_nm"] <- "Ctenochaetus striatus"


  # plot barplot for both sites:

  ## NG:
  rarcom_plot_NG <- ggplot2::ggplot(data = rarcom_df[which(rarcom_df$site == "N'Gouja"), ]) +

    ggplot2::geom_bar(ggplot2::aes(x = reorder(species_nm, - vid_occ_nb), y = (vid_occ_nb/99)*100,
                                   fill = site_presence),
                      stat = "identity") +

    ggplot2:: geom_hline(yintercept = 5, linetype = "dashed", color = "black") +

    ggplot2:: geom_hline(yintercept = 25, linetype = "dashed", color = "black") +

    ggplot2::scale_fill_manual(values = c(sites_colors[1],
                                          sites_colors[3]),
                               name = "Site presence",
                               labels = c("Both", "Fully Protected")) +

    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 7,
                                                       face = "italic",
                                                       angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +

    ggplot2::ylab("Temporal occurrence (%)") +

    ggplot2::xlab("")

  # B:
  rarcom_plot_B <- ggplot2::ggplot(data = rarcom_df[which(rarcom_df$site == "Boueni"), ]) +

    ggplot2::geom_bar(ggplot2::aes(x = reorder(species_nm, - vid_occ_nb), y = (vid_occ_nb/99)*100,
                                   fill = site_presence),
                      stat = "identity") +

    ggplot2:: geom_hline(yintercept = 5, linetype = "dashed", color = "black") +

    ggplot2:: geom_hline(yintercept = 25, linetype = "dashed", color = "black") +


    ggplot2::scale_fill_manual(values = c(sites_colors[1],
                                          sites_colors[2]),
                               name = "Site presence",
                               labels = c("Both", "Poorly Protected")) +

    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 7,
                                                       face = "italic",
                                                       angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +

    ggplot2::ylab("Temporal occurrence (%)") +

    ggplot2::xlab("")


  plot_both <- (rarcom_plot_NG + rarcom_plot_B) +
    patchwork::plot_layout(byrow = TRUE, heights = c(1, 1), widths = c(1, 1),
                           ncol = 1, nrow = 2, guides = "collect") +
    patchwork::plot_annotation(tag_levels = "A")

  ggplot2::ggsave(filename = here::here("outputs", "rar_comm_vid.pdf"),
                  plot = plot_both,
                  device = "pdf",
                  scale = 0.6,
                  height = 13000,
                  width = 18000,
                  units = "px",
                  dpi = 800)


  ## plot biplot:

  # create a dataframe to plot the biplot:
  biplot_df <- as.data.frame(matrix(nrow = 1, ncol = 4))
  colnames(biplot_df) <- c("species_nm", "NGouja", "Boueni", "presence")

  # perc_vid occ en numeric:
  rarcom_df$perc_vid_occ <- as.numeric(rarcom_df$perc_vid_occ)

  # loop on species:
  for (i in unique(rarcom_df$species_nm)) {

    species_nm <- i

    print(species_nm)

    # if species seen in NG and B:
    if (unique(rarcom_df[which(rarcom_df$species_nm == i), "site_presence"]) == "both") {

      presence <- "both"
      NG <- rarcom_df[which(rarcom_df$species_nm == i & rarcom_df$site == "N'Gouja"), "perc_vid_occ"]
      B <- rarcom_df[which(rarcom_df$species_nm == i & rarcom_df$site == "Boueni"), "perc_vid_occ"]

      biplot_df[nrow(biplot_df) + 1, ] <- c(species_nm, NG, B, presence)

    }

    # if species only seen in NG:
    if (unique(rarcom_df[which(rarcom_df$species_nm == i), "site_presence"]) == "N'Gouja only") {

      presence <- "N'Gouja only"
      NG <- rarcom_df[which(rarcom_df$species_nm == i & rarcom_df$site == "N'Gouja"), "perc_vid_occ"]
      B <- 0

      biplot_df[nrow(biplot_df) + 1, ] <- c(species_nm, NG, B, presence)

    }

    # if species only seen in B:
    if (unique(rarcom_df[which(rarcom_df$species_nm == i), "site_presence"]) == "Boueni only") {

      presence <- "Boueni only"
      NG <- 0
      B <- rarcom_df[which(rarcom_df$species_nm == i & rarcom_df$site == "Boueni"), "perc_vid_occ"]

      biplot_df[nrow(biplot_df) + 1, ] <- c(species_nm, NG, B, presence)

    }
  }

  ## class columns:
  biplot_df <- biplot_df[-1, ]
  biplot_df$NGouja <- as.numeric(biplot_df$NGouja)
  biplot_df$Boueni <- as.numeric(biplot_df$Boueni)


  ## plot the biplot:
  biplot_rarcom <- ggplot2::ggplot(data = biplot_df) +

    ggplot2::geom_point(ggplot2::aes(x = Boueni, y = NGouja,
                                   color = presence)) +

    ggplot2::geom_abline(color = "grey30", linetype = "dashed") +

    ggplot2::scale_color_manual(values = c(sites_colors[1],
                                          sites_colors[2],
                                          sites_colors[3]),
                               name = "Site presence") +

    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +

    ggplot2::ylab("Percentage of occurrence in N'Gouja's videos") +

    ggplot2::xlab("Percentage of occurrence in Boueni's videos")


  ggplot2::ggsave(filename = here::here("outputs", "rar_comm_biplot.pdf"),
                  plot = biplot_rarcom,
                  device = "pdf",
                  scale = 0.6,
                  height = 8000,
                  width = 9000,
                  units = "px",
                  dpi = 800)


  return(list(plot_both, biplot_rarcom))


}


#####


#' Title
#'
#' @param basic_fd_accum_df
#' @param sp_tr
#' @param tr_cat
#' @param rarcom_df
#'
#' @return
#' @export
#'
#' @examples
#'

spot.rare.sp.fd <- function(basic_accum_df,
                            site_asb_fe_df,
                            sp_tr,
                            tr_cat,
                            rarcom_df,
                            rarity_shapes,
                            rarity_colors_B,
                            rarity_colors_NG,
                            sites_colors) {


  # 1
  # Compute the rarity/commoness of each FE and not species:
  rarcom_fd_df <- rarcom.computation(basic_accum_df)


  # 2
  # Get FE's coordinates in the functional space:
  # First compute functional distances between the FEs:
  fe_dist <- mFD::funct.dist(
    sp_tr         = sp_tr,
    tr_cat        = tr_cat,
    metric        = "gower",
    scale_euclid  = "scale_center",
    ordinal_var   = "classic",
    weight_type   = "equal",
    stop_if_NA    = TRUE)

  # Then, compute quality's functional space to get FE's coord:
  fspaces_quality <- mFD::quality.fspaces(
    sp_dist             = fe_dist,
    maxdim_pcoa         = 10,
    deviation_weighting = "absolute",
    fdist_scaling       = FALSE,
    fdendro             = "average")
  fe_faxes_coord <- fspaces_quality$"details_fspaces"$"sp_pc_coord"


  # 3
  # Complete the site_asb_fe_df with rows: NG_rare, NG_med, NG_common and ...
  # ... idem for Boueni:


  # Create empty rows:
  site_asb_fe_df <- as.data.frame(site_asb_fe_df)

  site_asb_fe_df[nrow(site_asb_fe_df) + 1, ] <- rep(0, ncol(site_asb_fe_df))
  rownames(site_asb_fe_df)[nrow(site_asb_fe_df)] <- "NG_super_rare"

  site_asb_fe_df[nrow(site_asb_fe_df) + 1, ] <- rep(0, ncol(site_asb_fe_df))
  rownames(site_asb_fe_df)[nrow(site_asb_fe_df)] <- "NG_rare"

  site_asb_fe_df[nrow(site_asb_fe_df) + 1, ] <- rep(0, ncol(site_asb_fe_df))
  rownames(site_asb_fe_df)[nrow(site_asb_fe_df)] <- "NG_common"

  site_asb_fe_df[nrow(site_asb_fe_df) + 1, ] <- rep(0, ncol(site_asb_fe_df))
  rownames(site_asb_fe_df)[nrow(site_asb_fe_df)] <- "B_super_rare"

  site_asb_fe_df[nrow(site_asb_fe_df) + 1, ] <- rep(0, ncol(site_asb_fe_df))
  rownames(site_asb_fe_df)[nrow(site_asb_fe_df)] <- "B_rare"

  site_asb_fe_df[nrow(site_asb_fe_df) + 1, ] <- rep(0, ncol(site_asb_fe_df))
  rownames(site_asb_fe_df)[nrow(site_asb_fe_df)] <- "B_common"


  # Fill the empty rows for N'Gouja first:
  for (i in (rarcom_fd_df$species_nm[which(rarcom_fd_df$site == "N'Gouja")])) {

    rarity <- rarcom_fd_df[which(rarcom_fd_df$species_nm == i &
                                rarcom_fd_df$site == "N'Gouja"), "rarity"]

    if (rarity == "super rare") {
      site_asb_fe_df[c(3:5), i] <- c(1, 0, 0)
    }

    if (rarity == "rare") {
      site_asb_fe_df[c(3:5), i] <- c(0, 1, 0)
    }

    if (rarity == "common") {
      site_asb_fe_df[c(3:5), i] <- c(0, 0, 1)
    }

  } # end loop N'Gouja's FE


  # Fill the empty rows for Boueni first:
  for (i in (rarcom_fd_df$species_nm[which(rarcom_fd_df$site == "Boueni")])) {

    rarity <- rarcom_fd_df[which(rarcom_fd_df$species_nm == i &
                                   rarcom_fd_df$site == "Boueni"), "rarity"]

    if (rarity == "super rare") {
      site_asb_fe_df[c(6:8), i] <- c(1, 0, 0)
    }

    if (rarity == "rare") {
      site_asb_fe_df[c(6:8), i] <- c(0, 1, 0)
    }

    if (rarity == "common") {
      site_asb_fe_df[c(6:8), i] <- c(0, 0, 1)
    }

  } # end loop Boueni's FE



  # restraint the coord to the first 5 axes: work in the 5d dimensional space:
  ## only work with the first 5 dimensions:
  fe_faxes_coord <- fe_faxes_coord[, c("PC1", "PC2", "PC3", "PC4", "PC5")]



  # 4
  # Then plot N'Gouja:

  ## get range faxes:
  range_sp_coord  <- range(fe_faxes_coord)
  range_faxes <- range_sp_coord +
    c(-1, 1) * (range_sp_coord[2] - range_sp_coord[1]) * 0.05


  # list that will contains plots for each combinatio of axis:
  plot_NG <- list()

  axes_plot <- utils::combn(c("PC1", "PC2", "PC3", "PC4"), 2)
  plot_nb   <- ncol(axes_plot)


  # for each combinason of two axis:
  for (k in (1:plot_nb)) {

    # get names of axes to plot:
    xy_k <- axes_plot[1:2, k]

    # get sp coord along these two axes:
    fe_faxes_coord_xy <- fe_faxes_coord[, xy_k]

    # Plot background:

    # plot backrgound:
    plot_k <- mFD::background.plot(range_faxes, faxes_nm = c(xy_k[1], xy_k[2]),
                                   color_bg = "grey95")


    ## Add global species pool:

    # Compute vertices:
    sp_coord_2d <- fe_faxes_coord_xy
    vert <- mFD::vertices(sp_coord_2d,  order_2D = TRUE, check_input = TRUE)

    plot_k <- mFD::pool.plot(ggplot_bg = plot_k,
                             sp_coord2D = sp_coord_2d,
                             vertices_nD = vert,
                             plot_pool = TRUE,
                             color_pool = NA,
                             fill_pool = NA,
                             alpha_ch =  NA,
                             color_ch = NA,
                             fill_ch = "white",
                             shape_pool = NA,
                             size_pool = 1,
                             shape_vert = NA,
                             size_vert = 1,
                             color_vert = NA,
                             fill_vert = NA)


    ## Add the N'Gouja's convexhull:
    # Retrieve species coordinates matrix for the assemblage NG_common:
    sp_filter <- mFD::sp.filter(asb_nm = c("N'Gouja"),
                                sp_faxes_coord = fe_faxes_coord_xy,
                                asb_sp_w       = site_asb_fe_df)
    sp_faxes_coord_NG <- sp_filter$`species coordinates`

    vert_nm_NG <- mFD::vertices(sp_faxes_coord_NG,
                                order_2D = TRUE, check_input = TRUE)


    plot_k <- mFD::fric.plot(ggplot_bg = plot_k,
                             asb_sp_coord2D = list("N'Gouja" =
                                                     sp_faxes_coord_NG),
                             asb_vertices_nD = list("N'Gouja" = vert_nm_NG),
                             plot_sp = FALSE,
                             color_ch = NA,
                             fill_ch = c("N'Gouja" = sites_colors[2]),
                             alpha_ch = c("N'Gouja" = 0.6),
                             shape_sp = c("N'Gouja" = NA),
                             size_sp = c("N'Gouja" = NA),
                             color_sp = c("N'Gouja" = NA),
                             fill_sp = c("N'Gouja" = NA),
                             shape_vert = NA,
                             size_vert = NA,
                             color_vert = NA,
                             fill_vert = NA)


    ## Add rare/medium/common species:

    # Retrieve species coordinates matrix for the assemblage NG_rare:
    sp_filter <- mFD::sp.filter(asb_nm = c("NG_super_rare"),
                                sp_faxes_coord = fe_faxes_coord_xy,
                                asb_sp_w       = site_asb_fe_df)
    sp_faxes_coord_rare <- sp_filter$`species coordinates`

    vert_nm_rare <- mFD::vertices(sp_faxes_coord_rare,
                                  order_2D = TRUE, check_input = TRUE)

    # Retrieve species coordinates matrix for the assemblage NG_medium:
    sp_filter <- mFD::sp.filter(asb_nm = c("NG_rare"),
                                sp_faxes_coord = fe_faxes_coord_xy,
                                asb_sp_w       = site_asb_fe_df)
    sp_faxes_coord_medium <- sp_filter$`species coordinates`

    vert_nm_medium <- mFD::vertices(sp_faxes_coord_medium,
                                    order_2D = TRUE, check_input = TRUE)

    # Retrieve species coordinates matrix for the assemblage NG_common:
    sp_filter <- mFD::sp.filter(asb_nm = c("NG_common"),
                                sp_faxes_coord = fe_faxes_coord_xy,
                                asb_sp_w       = site_asb_fe_df)
    sp_faxes_coord_common <- sp_filter$`species coordinates`

    vert_nm_common <- mFD::vertices(sp_faxes_coord_common,
                                    order_2D = TRUE, check_input = TRUE)

    plot_k <- mFD::fric.plot(ggplot_bg = plot_k,
                             asb_sp_coord2D = list("NG_super_rare" =
                                                     sp_faxes_coord_rare,
                                                   "NG_rare" =
                                                     sp_faxes_coord_medium,
                                                   "NG_common" =
                                                     sp_faxes_coord_common),
                             asb_vertices_nD = list("NG_super_rare" = vert_nm_rare,
                                                    "NG_rare" = vert_nm_medium,
                                                    "NG_common" = vert_nm_common),
                             plot_sp = TRUE,
                             color_ch = NA,
                             fill_ch = NA,
                             alpha_ch = NA,
                             shape_sp = list("NG_super_rare" = rarity_shapes[1],
                                             "NG_rare" = rarity_shapes[2],
                                             "NG_common" = rarity_shapes[3]),
                             size_sp = c("NG_super_rare" = 2, "NG_rare" = 3, "NG_common" = 3),
                             color_sp = c("NG_super_rare" = rarity_colors_NG[1],
                                          "NG_rare" = rarity_colors_NG[2],
                                          "NG_common" = rarity_colors_NG[3]),
                             fill_sp = c("NG_super_rare" = rarity_colors_NG[1],
                                         "NG_rare" = rarity_colors_NG[2],
                                         "NG_common" = rarity_colors_NG[3]),
                             shape_vert = list("NG_super_rare" = rarity_shapes[1],
                                               "NG_rare" = rarity_shapes[2],
                                               "NG_common" = rarity_shapes[3]),
                             size_vert = c("NG_super_rare" = 2, "NG_rare" = 2, "NG_common" = 2),
                             color_vert = c("NG_super_rare" = rarity_colors_NG[1],
                                            "NG_rare" = rarity_colors_NG[2],
                                            "NG_common" = rarity_colors_NG[3]),
                             fill_vert =  c("NG_super_rare" = rarity_colors_NG[1],
                                            "NG_rare" = rarity_colors_NG[2],
                                            "NG_common" = rarity_colors_NG[3]))

    plot_NG[[k]] <- plot_k

  } # end loop on plot



  # 4
  # Then plot Boueni:

  ## get range faxes:
  range_sp_coord  <- range(fe_faxes_coord)
  range_faxes <- range_sp_coord +
    c(-1, 1) * (range_sp_coord[2] - range_sp_coord[1]) * 0.05


  # list that will contains plots for each combinatio of axis:
  plot_B <- list()

  axes_plot <- utils::combn(c("PC1", "PC2", "PC3", "PC4"), 2)
  plot_nb   <- ncol(axes_plot)


  # for each combinason of two axis:
  for (k in (1:plot_nb)) {

    # get names of axes to plot:
    xy_k <- axes_plot[1:2, k]

    # get sp coord along these two axes:
    fe_faxes_coord_xy <- fe_faxes_coord[, xy_k]

    # Plot background:

    # plot backrgound:
    plot_k <- mFD::background.plot(range_faxes, faxes_nm = c(xy_k[1], xy_k[2]),
                                   color_bg = "grey95")


    ## Add global species pool:

    # Compute vertices:
    sp_coord_2d <- fe_faxes_coord_xy
    vert <- mFD::vertices(sp_coord_2d,  order_2D = TRUE, check_input = TRUE)

    plot_k <- mFD::pool.plot(ggplot_bg = plot_k,
                             sp_coord2D = sp_coord_2d,
                             vertices_nD = vert,
                             plot_pool = TRUE,
                             color_pool = NA,
                             fill_pool = NA,
                             alpha_ch =  NA,
                             color_ch = NA,
                             fill_ch = "white",
                             shape_pool = NA,
                             size_pool = 1,
                             shape_vert = NA,
                             size_vert = 1,
                             color_vert = NA,
                             fill_vert = NA)


    ## Add the Boueni convexhull:
    sp_filter <- mFD::sp.filter(asb_nm = c("Boueni"),
                                sp_faxes_coord = fe_faxes_coord_xy,
                                asb_sp_w       = site_asb_fe_df)
    sp_faxes_coord_B <- sp_filter$`species coordinates`

    vert_nm_B <- mFD::vertices(sp_faxes_coord_B,
                                order_2D = TRUE, check_input = TRUE)


    plot_k <- mFD::fric.plot(ggplot_bg = plot_k,
                             asb_sp_coord2D = list("Boueni" =
                                                     sp_faxes_coord_B),
                             asb_vertices_nD = list("Boueni" = vert_nm_B),
                             plot_sp = FALSE,
                             color_ch = NA,
                             fill_ch = c("Boueni" = sites_colors[1]),
                             alpha_ch = c("Boueni" = 0.6),
                             shape_sp = c("Boueni" = NA),
                             size_sp = c("Boueni" = NA),
                             color_sp = c("Boueni" = NA),
                             fill_sp = c("Boueni" = NA),
                             shape_vert = NA,
                             size_vert = NA,
                             color_vert = NA,
                             fill_vert = NA)


    ## Add rare/medium/common species:

    # Retrieve species coordinates matrix for the assemblage B_rare:
    sp_filter <- mFD::sp.filter(asb_nm = c("B_super_rare"),
                                sp_faxes_coord = fe_faxes_coord_xy,
                                asb_sp_w       = site_asb_fe_df)
    sp_faxes_coord_rare <- sp_filter$`species coordinates`

    vert_nm_rare <- mFD::vertices(sp_faxes_coord_rare,
                                  order_2D = TRUE, check_input = TRUE)

    # Retrieve species coordinates matrix for the assemblage B_medium:
    sp_filter <- mFD::sp.filter(asb_nm = c("B_rare"),
                                sp_faxes_coord = fe_faxes_coord_xy,
                                asb_sp_w       = site_asb_fe_df)
    sp_faxes_coord_medium <- sp_filter$`species coordinates`

    vert_nm_medium <- mFD::vertices(sp_faxes_coord_medium,
                                    order_2D = TRUE, check_input = TRUE)

    # Retrieve species coordinates matrix for the assemblage B_common:
    sp_filter <- mFD::sp.filter(asb_nm = c("B_common"),
                                sp_faxes_coord = fe_faxes_coord_xy,
                                asb_sp_w       = site_asb_fe_df)
    sp_faxes_coord_common <- sp_filter$`species coordinates`

    vert_nm_common <- mFD::vertices(sp_faxes_coord_common,
                                    order_2D = TRUE, check_input = TRUE)

    plot_k <- mFD::fric.plot(ggplot_bg = plot_k,
                             asb_sp_coord2D = list("B_super_rare" =
                                                     sp_faxes_coord_rare,
                                                   "B_rare" =
                                                     sp_faxes_coord_medium,
                                                   "B_common" =
                                                     sp_faxes_coord_common),
                             asb_vertices_nD = list("B_super_rare" = vert_nm_rare,
                                                    "B_rare" = vert_nm_medium,
                                                    "B_common" = vert_nm_common),
                             plot_sp = TRUE,
                             color_ch = NA,
                             fill_ch = NA,
                             alpha_ch = NA,
                             shape_sp = list("B_super_rare" = rarity_shapes[1],
                                             "B_rare" = rarity_shapes[2],
                                             "B_common" = rarity_shapes[3]),
                             size_sp = c("B_super_rare" = 2, "B_rare" = 3, "B_common" = 3),
                             color_sp = c("B_super_rare" = rarity_colors_B[1],
                                          "B_rare" = rarity_colors_B[2],
                                          "B_common" = rarity_colors_B[3]),
                             fill_sp = c("B_super_rare" = rarity_colors_B[1],
                                         "B_rare" = rarity_colors_B[2],
                                         "B_common" = rarity_colors_B[3]),
                             shape_vert = list("B_super_rare" = rarity_shapes[1],
                                               "B_rare" = rarity_shapes[2],
                                               "B_common" = rarity_shapes[3]),
                             size_vert = c("B_super_rare" = 2, "B_rare" = 2, "B_common" = 2),
                             color_vert = c("B_super_rare" = rarity_colors_B[1],
                                            "B_rare" = rarity_colors_B[2],
                                            "B_common" = rarity_colors_B[3]),
                             fill_vert =  c("B_super_rare" = rarity_colors_B[1],
                                            "B_rare" = rarity_colors_B[2],
                                            "B_common" = rarity_colors_B[3]))

    plot_B[[k]] <- plot_k

  } # end loop on plot


  # Then use patchwork to have two wonderful graphs:
  plot_rarity_space <- (plot_NG[[1]] + plot_NG[[6]] + plot_B[[1]] + plot_B[[6]])
    patchwork::plot_layout(byrow = TRUE, heights = rep(1, 3),
                           widths = rep(1, 3), ncol = 2, nrow = 2,
                           guides = "collect")

  # save:
  ggplot2::ggsave(filename = here::here("outputs", "plot_rarity_space.pdf"),
                  plot = plot_rarity_space,
                  device = "pdf",
                  scale = 1,
                  height = 10000,
                  width = 15000,
                  units = "px",
                  dpi = 800)

  return(plot_rarity_space)


}



####


plot.rarity.fspe <- function(sp_dist_gravcenter,
                             basic_accum_df,
                             rarcom_df) {


  # Add FSpe informations:
  fspe_rarcom_df <- rarcom_df
  fspe_rarcom_df$relat_dist_to_grav <- rep(0, nrow(fspe_rarcom_df))

  for (i in (1:nrow(fspe_rarcom_df))) {

    sp_nm <- fspe_rarcom_df[i, "species_nm"]
    fspe_value <- sp_dist_gravcenter[which(names(sp_dist_gravcenter) == sp_nm)][[1]]

    fspe_rarcom_df[i, "relat_dist_to_grav"] <- fspe_value

  }


  # plot N'Gouja's data:
  NG_rar_fspe_plot <- ggplot2::ggplot(data = fspe_rarcom_df[which(fspe_rarcom_df$site == "N'Gouja"), ],
                                      ggplot2::aes(x = rarity, y = relat_dist_to_grav)) +

    ggplot2::geom_boxplot(color = "#80cdc1", fill = "#80cdc1", alpha = 0.3,
                          outlier.shape = NA) +

    ggplot2::geom_jitter(ggplot2::aes(shape = site_presence), color = "#80cdc1", size = 2) +

    ggplot2::scale_shape_manual(values = c(20, 21),
                                name = "Site presence",
                                labels = c("Species shared between sites",
                                           "Species only present in the studied site")) +

    ggplot2::scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1, 1.1)) +

    ggplot2::xlab("") +

    ggplot2::ylab("Relative distance to the gravity center of the global pool") +

    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90"))

  # plot Boueni's data:
  B_rar_fspe_plot <- ggplot2::ggplot(data = fspe_rarcom_df[which(fspe_rarcom_df$site == "Boueni"), ],
                                      ggplot2::aes(x = rarity, y = relat_dist_to_grav)) +

    ggplot2::geom_boxplot(color = "#bf812d", fill = "#bf812d", alpha = 0.3) +

    ggplot2::geom_jitter(ggplot2::aes(shape = site_presence), color = "#bf812d", size = 2) +

    ggplot2::scale_shape_manual(values = c(20, 21),
                                name = "Site presence",
                                labels = c("Species shared between sites",
                                           "Species only present in the studied site")) +

    ggplot2::scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1, 1.1)) +

    ggplot2::xlab("") +

    ggplot2::ylab("Relative distance to the gravity center of the global pool") +

    ggplot2::guides(colour = "none", shape = "none")+

    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90"))


  # both_plot:
  both_plot <- (NG_rar_fspe_plot + B_rar_fspe_plot) +
  patchwork::plot_layout(byrow = TRUE, heights = rep(1, 3),
                         widths = rep(1, 3), ncol = 2, nrow = 1,
                         guides = "collect") +
  patchwork::plot_annotation(tag_levels = "A")


  # save:
  ggplot2::ggsave(filename = here::here("outputs", "plot_fspe_rarity.pdf"),
                  plot = both_plot,
                  device = "pdf",
                  scale = 0.8,
                  height = 8000,
                  width = 15000,
                  units = "px",
                  dpi = 800)

  return(list(fspe_rarcom_df, both_plot))


}
