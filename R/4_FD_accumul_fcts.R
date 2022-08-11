###############################################################################
##
## Script to code functions linked with FD accumulation
##
## 4_FD_accul_fcts.R
##
## 10/08/2022
##
## Camille Magneville
##
###############################################################################



#' Title
#'
#' @param basic_fd_accum_df
#' @param sp_tr
#' @param tr_cat
#' @param fd_indices
#' @param rich_plot
#'
#' @return
#' @export
#'
#' @examples



compute.fd.day.accum <- function(basic_fd_accum_df,
                                 sp_tr,
                                 tr_cat,
                                 fd_indices,
                                 rich_plot = TRUE) {

  # 1
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



  # Lastly compute FD indices for each assemblage:

  rownames(basic_fd_accum_df) <- basic_fd_accum_df$vid_id

  basic_fd_accum_df[, -c(ncol(basic_fd_accum_df), ncol(basic_fd_accum_df) - 1,
                    ncol(basic_fd_accum_df) - 2, ncol(basic_fd_accum_df) - 3)] <- apply(
                      basic_fd_accum_df[, -c(ncol(basic_fd_accum_df), ncol(basic_fd_accum_df) - 1,
                                             ncol(basic_fd_accum_df) - 2, ncol(basic_fd_accum_df) - 3)],
                      2, as.numeric)

  # the best space has 5dim but one asb as 5 species -> 4 dim which is also good:
  alpha_fd_indices_day <- mFD::alpha.fd.multidim(
    sp_faxes_coord   = fe_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4")],
    asb_sp_w         = as.matrix(basic_fd_accum_df[, -c(ncol(basic_fd_accum_df), ncol(basic_fd_accum_df) - 1,
                                                        ncol(basic_fd_accum_df) - 2, ncol(basic_fd_accum_df) - 3)]),
    ind_vect         = fd_indices,
    scaling          = TRUE,
    check_input      = TRUE,
    details_returned = TRUE)


  # add the Fric values to the basic_fd_accum_df:
  basic_df <- cbind(basic_fd_accum_df, alpha_fd_indices_day$functional_diversity_indices)


  # 2
  # Plot Fric evolution if TRUE:
  ## rename video_nb from 1, 2 -> vid_1, vid_2 and give right levels:
  basic_df$video_nb <- paste0(rep("vid_", nrow(basic_df)), sep = "", basic_df$video_nb)
  basic_df$video_nb  <- as.factor(basic_df$video_nb)
  basic_df$video_nb <- ordered(basic_df$video_nb, levels = paste0(rep("vid_", 33),
                                                                  c(1:33)))

  ## site is a factor:
  basic_df$site <- as.factor(basic_df$site)



  ##plot the species richness accumulation if asked:
  if (rich_plot == TRUE) {

    richn_var <- ggplot2::ggplot(data = basic_df) +

      ggplot2::geom_boxplot(ggplot2::aes(y = fric, x = video_nb),
                            color = "grey70", fill = "grey80", alpha = 0.5) +

      ggplot2::geom_jitter(ggplot2::aes(y = fric, x = video_nb,
                                        color = site, fill = site),
                           width = 0.1) +

      ggplot2::scale_fill_manual(values = c("#bf812d",
                                            "#80cdc1"),
                                 name = "site") +

      ggplot2::scale_colour_manual(values = c("#bf812d",
                                              "#80cdc1"),
                                   name = "site") +

      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                     panel.background = ggplot2::element_rect(fill = "white",
                                                              colour = "grey"),
                     panel.grid.major = ggplot2::element_line(colour = "grey")) +


      ggplot2::scale_x_discrete(labels= c("7:30", "", "8:00", "", "8:40", "",
                                          "9:15", "", "9:45", "", "10:20", "",
                                          "10:55", "", "11:40", "", "12:20", "",
                                          "12:55", "", "13:40", "", "14:25", "",
                                          "15:00", "", "15:45", "", "16:30", "",
                                          "17:00", "", "17:30")) +

      ggplot2::ylab("FRic") +

      ggplot2::xlab("") +

      ggplot2::guides(fill = "none")

  }


  # 3
  # now compute FD accum: 0 until first occurrences and then 1 always:

  ## remove the richn column:
  basic_df <- basic_df[, -c(ncol(basic_df) - 1)]

  ## add a new column: site + day:
  basic_df$site_day <- paste0(basic_df$site, sep = "_", basic_df$day)

  ## for each site_day:
  for (i in (unique(basic_df$site_day))) {


    ## loop on species:
    for (j in (1:(ncol(basic_df) - 6))) {


      # set the rownames to basic:
      rownames(basic_df) <- NULL


      ## loop on the rows:
      for (k in rownames(basic_df[which(basic_df$site_day == i), ])) {

        # if == 1, then for all the videos of the studied site_day -> 0
        if (basic_df[k, j] == 1) {
          basic_df[which(as.numeric(rownames(basic_df)) > as.numeric(k) & basic_df$site_day == i), j] <- 1
          break()
        }

      } # loop on the rows

    } # end loop on species

  } # end loop on site_day


  accum_FD_df <- basic_df

  # compute the sp richness accumulation:

  rownames(accum_FD_df) <- accum_FD_df$vid_id

  alpha_fd_indices_accum <- mFD::alpha.fd.multidim(
    sp_faxes_coord   = fe_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4")],
    asb_sp_w         = as.matrix(accum_FD_df[, -c(ncol(accum_FD_df), ncol(accum_FD_df) - 1,
                                                        ncol(accum_FD_df) - 2, ncol(accum_FD_df) - 3,
                                                        ncol(accum_FD_df) - 4, ncol(accum_FD_df) - 5)]),
    ind_vect         = fd_indices,
    scaling          = TRUE,
    check_input      = TRUE,
    details_returned = TRUE)

  # remove indices computed at the video scale for the plot (now we compute FD ...
  # ... accumulation, otherwise there will be columns named identically ...
  # ... by FD indices names: ex "fric" for FRic at the video scale and ...
  # ... "fric" showing the FRic gathered until the studied video (accum)):
  accum_FD_df <- accum_FD_df[, which(! colnames(accum_FD_df) %in% c(fd_indices, "sp_richn"))]

  # add functional indices:
  accum_FD_df <- cbind(accum_FD_df, alpha_fd_indices_accum$functional_diversity_indices)


  ## create a new column that will contain the percentage of new species ...
  # ... seen on each video -> FD accumul with 100% at the end of the day ...
  # ... all the species richness seen during the day_site is seen at the ...
  # ... end of the day_site obviously ;) :
  accum_FD_df$perc_FD_acc_day <- rep(0, nrow(accum_FD_df))

  ## then fill this new column:
  ## for each site_day:
  for (i in (unique(accum_FD_df$site_day))) {

    sum_tot_richn <- accum_FD_df[which(accum_FD_df$site_day == i & accum_FD_df$video_nb == "vid_33"), "fric"]

    ## loop on the rows of each site_day:
    for (j in rownames(accum_FD_df[which(accum_FD_df$site_day == i), ])) {

        accum_FD_df[j, "perc_FD_acc_day"] <- (accum_FD_df[j, "fric"]/sum_tot_richn)*100

    }
  }


  if (rich_plot == TRUE) {

    return_list <- list(richn_var, accum_FD_df)

    # and save:
    ggplot2::ggsave(filename = here::here("outputs", "FD_richn_day_var.pdf"),
                    plot = richn_var,
                    device = "pdf",
                    scale = 1,
                    height = 5000,
                    width = 8000,
                    units = "px",
                    dpi = 600)

  }

  if (rich_plot == FALSE) {

    return_list <- list(accum_FD_df)

  }

  return(return_list)


}



#####


#' Title
#'
#' @param basic_fd_accum_df
#' @param sp_tr
#' @param tr_cat
#' @param fd_indices
#' @param rich_plot
#'
#' @return
#' @export
#'
#' @examples



compute.fd.day.accum <- function(basic_fd_accum_df,
                                 sp_tr,
                                 tr_cat,
                                 fd_indices,
                                 rich_plot = TRUE) {

  # 1
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



  # Lastly compute FD indices for each assemblage:

  rownames(basic_fd_accum_df) <- basic_fd_accum_df$vid_id

  basic_fd_accum_df[, -c(ncol(basic_fd_accum_df), ncol(basic_fd_accum_df) - 1,
                         ncol(basic_fd_accum_df) - 2, ncol(basic_fd_accum_df) - 3)] <- apply(
                           basic_fd_accum_df[, -c(ncol(basic_fd_accum_df), ncol(basic_fd_accum_df) - 1,
                                                  ncol(basic_fd_accum_df) - 2, ncol(basic_fd_accum_df) - 3)],
                           2, as.numeric)

  # the best space has 5dim but one asb as 5 species -> 4 dim which is also good:
  alpha_fd_indices_day <- mFD::alpha.fd.multidim(
    sp_faxes_coord   = fe_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4")],
    asb_sp_w         = as.matrix(basic_fd_accum_df[, -c(ncol(basic_fd_accum_df), ncol(basic_fd_accum_df) - 1,
                                                        ncol(basic_fd_accum_df) - 2, ncol(basic_fd_accum_df) - 3)]),
    ind_vect         = fd_indices,
    scaling          = TRUE,
    check_input      = TRUE,
    details_returned = TRUE)


  # add the Fric values to the basic_fd_accum_df:
  basic_df <- cbind(basic_fd_accum_df, alpha_fd_indices_day$functional_diversity_indices)


  # 2
  # Plot Fric evolution if TRUE:
  ## rename video_nb from 1, 2 -> vid_1, vid_2 and give right levels:
  basic_df$video_nb <- paste0(rep("vid_", nrow(basic_df)), sep = "", basic_df$video_nb)
  basic_df$video_nb  <- as.factor(basic_df$video_nb)
  basic_df$video_nb <- ordered(basic_df$video_nb, levels = paste0(rep("vid_", 33),
                                                                  c(1:33)))

  ## site is a factor:
  basic_df$site <- as.factor(basic_df$site)

  ## Add a column which contains day1, day2, day3 for NG and B:
  basic_df[which(basic_df$day %in% c("03-11-2019", "04-11-2019")), "day_nb"] <- "day_1"
  basic_df[which(basic_df$day %in% c("05-11-2019", "06-11-2019")), "day_nb"] <- "day_2"
  basic_df[which(basic_df$day %in% c("08-11-2019", "09-11-2019")), "day_nb"] <- "day_3"
  basic_df$day_nb <- as.factor(basic_df$day_nb)
  basic_df$day_nb <- ordered(basic_df$day_nb, levels = paste0(rep("day_", 3),
                                                              c(1:3)))



  ##plot the species richness accumulation if asked:
  if (rich_plot == TRUE) {

    richn_var <- ggplot2::ggplot(data = basic_df) +

      ggplot2::geom_line(ggplot2::aes(y = fric, x = video_nb,group = day_nb,
                                      color = site, fill = site),
                         size = 0.9) +

      ggplot2::facet_grid(site ~ day_nb) +

      ggplot2::scale_fill_manual(values = c("#bf812d",
                                            "#80cdc1"),
                                 name = "Site") +

      ggplot2::scale_colour_manual(values = c("#bf812d",
                                              "#80cdc1"),
                                   name = "Site") +

      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                     panel.background = ggplot2::element_rect(fill = "white",
                                                              colour = "grey90"),
                     panel.grid.major = ggplot2::element_line(colour = "grey90")) +


      ggplot2::scale_x_discrete(labels= c("7:30", "", "8:00", "", "8:40", "",
                                          "9:15", "", "9:45", "", "10:20", "",
                                          "10:55", "", "11:40", "", "12:20", "",
                                          "12:55", "", "13:40", "", "14:25", "",
                                          "15:00", "", "15:45", "", "16:30", "",
                                          "17:00", "", "17:30")) +

      ggplot2::ylab("Proportion of total functional richness") +

      ggplot2::xlab("") +

      ggplot2::guides(fill = "none")

  }


  # 3
  # now compute FD accum: 0 until first occurrences and then 1 always:

  ## for each site_day:
  for (i in (unique(basic_df$site))) {


    ## loop on species:
    for (j in (1:(ncol(basic_df) - 13))) {


      # set the rownames to basic:
      rownames(basic_df) <- NULL


      ## loop on the rows:
      for (k in rownames(basic_df[which(basic_df$site == i), ])) {

        # if == 1, then for all the videos of the studied site_day -> 0
        if (basic_df[k, j] == 1) {
          basic_df[which(as.numeric(rownames(basic_df)) > as.numeric(k) & basic_df$site == i), j] <- 1
          break()
        }

      } # loop on the rows

    } # end loop on species

  } # end loop on site


  accum_FD_df <- basic_df

  # compute the sp richness accumulation:

  rownames(accum_FD_df) <- accum_FD_df$vid_id

  alpha_fd_indices_accum <- mFD::alpha.fd.multidim(
    sp_faxes_coord   = fe_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4")],
    asb_sp_w         = as.matrix(accum_FD_df[, c(1:(ncol(accum_FD_df) - 14))]),
    ind_vect         = fd_indices,
    scaling          = TRUE,
    check_input      = TRUE,
    details_returned = TRUE)

  # remove indices computed at the video scale for the plot (now we compute FD ...
  # ... accumulation, otherwise there will be columns named identically ...
  # ... by FD indices names: ex "fric" for FRic at the video scale and ...
  # ... "fric" showing the FRic gathered until the studied video (accum)):
  accum_FD_df <- accum_FD_df[, which(! colnames(accum_FD_df) %in% c(fd_indices, "sp_richn"))]

  if ("fide" %in% fd_indices) {
    accum_FD_df <- accum_FD_df[, which(! colnames(accum_FD_df) %in% c("fide_PC1",
                                                                      "fide_PC2",
                                                                      "fide_PC3",
                                                                      "fide_PC4"))]
  }

  # add functional indices:
  accum_FD_df <- cbind(accum_FD_df, alpha_fd_indices_accum$functional_diversity_indices)


  if (rich_plot == TRUE) {

    return_list <- list(richn_var, accum_FD_df)

    # and save:
    ggplot2::ggsave(filename = here::here("outputs", "FD_richn_interday_var.pdf"),
                    plot = richn_var,
                    device = "pdf",
                    scale = 1,
                    height = 5000,
                    width = 8000,
                    units = "px",
                    dpi = 600)

  }

  if (rich_plot == FALSE) {

    return_list <- list(accum_FD_df)

  }

  return(return_list)


}





