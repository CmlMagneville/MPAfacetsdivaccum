###############################################################################
##
## Script to code functions linked with PD accumulation
##
## 5_PD_accul_fcts.R
##
## 11/08/2022
##
## Camille Magneville
##
###############################################################################



#' Title
#'
#' @param basic_fd_accum_df
#' @param rich_plot
#'
#' @return
#' @export
#'
#' @examples


compute.pd.day.accum <- function(basic_accum_df,
                                 rich_plot = TRUE) {



  # 1
  # First change species names with associated species which are present ...
  # ... in the fishtree phylogeny:
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



  # 2
  # Compute PD for each video:

  # first must compute a sp*asb df:
  sp_asb_df <- basic_accum_df
  rownames(sp_asb_df) <- sp_asb_df$vid_id


  # compute phylo:
  sp_nm_all <- colnames(sp_asb_df[, -c(ncol(sp_asb_df),
                                       ncol(sp_asb_df) - 1,
                                       ncol(sp_asb_df) - 2,
                                       ncol(sp_asb_df) - 3)],)
  phylo <- fishtree::fishtree_phylogeny(species = sp_nm_all)

  # compute Faith's PD:
  PD_values <- picante::pd(sp_asb_df[, -c(ncol(sp_asb_df),
                                          ncol(sp_asb_df) - 1,
                                          ncol(sp_asb_df) - 2,
                                          ncol(sp_asb_df) - 3)], phylo, include.root=FALSE)


  # add Faith's PD information to the basic_accum_df:
  basic_accum_df <- cbind(basic_accum_df, PD_values$PD)
  colnames(basic_accum_df)[ncol(basic_accum_df)] <- "hour_PD"


  # 3
  # Represent PD's variation:


  # first must compute the total PD's of the whole phylogeny so ...
  # ... Faith's PD from each video is expressed as a proportion of the total ...
  # ... phylogeny (as the FRic index):
  sp_all_df <- sp_asb_df[nrow(sp_asb_df), ]
  sp_all_df[1, ] <- 1
  rownames(sp_all_df) <- "PD_tot"
  tot_PD_value <- picante::pd(sp_all_df[, -c(ncol(sp_all_df),
                                          ncol(sp_all_df) - 1,
                                          ncol(sp_all_df) - 2,
                                          ncol(sp_all_df) - 3)], phylo, include.root=FALSE)
  tot_PD_value <- tot_PD_value$PD

  ## Express video's PD as a proportion of the total PD:
  basic_accum_df$hour_PD <- basic_accum_df$hour_PD / tot_PD_value


  ## Plot PD's variation:
  basic_df <- basic_accum_df

  ## rename hour_nb from 1, 2 -> hour_1, hour_2 and give right levels:
  basic_df$hour_nb <- paste0(rep("hour_", nrow(basic_df)), sep = "", basic_df$hour_nb)
  basic_df$hour_nb  <- as.factor(basic_df$hour_nb)
  basic_df$hour_nb <- ordered(basic_df$hour_nb, levels = paste0(rep("hour_", 9),
                                                                  c(1:9)))

  ## site is a factor:
  basic_df$site <- as.factor(basic_df$site)



  ##plot the species richness accumulation if asked:
  if (rich_plot == TRUE) {

    richn_var <- ggplot2::ggplot(data = basic_df) +

      ggplot2::geom_boxplot(ggplot2::aes(y = hour_PD, x = hour_nb),
                            color = "grey70", fill = "grey80", alpha = 0.5) +

      ggplot2::geom_jitter(ggplot2::aes(y = hour_PD, x = hour_nb,
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


      ggplot2::scale_x_discrete(labels= c("8:00-8:59", "9:00-9:59",
                                          "10:00-10:59", "11:00-11:59",
                                          "12:00-12:59", "13:00-13:59",
                                          "14:00-14:59", "15:00-15:59",
                                          "16:00-17:59")) +

      ggplot2::ylab("Proportion of total Faith's PD") +

      ggplot2::xlab("") +

      ggplot2::guides(fill = "none")

  }


  # 4
  # Now compute PD accumulation df to compute the accumulation of PD through ...
  # ... a day: as for FRic after the first occ, always 1 so the tree grows:

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



  # 5
  # Compute PD's accumulation:

  accum_PD_df <- basic_df

  # compute the sp richness accumulation:

  rownames(accum_PD_df) <- accum_PD_df$hour_id

  PD_values <- picante::pd(accum_PD_df[, -c(ncol(accum_PD_df),
                                          ncol(accum_PD_df) - 1,
                                          ncol(accum_PD_df) - 2,
                                          ncol(accum_PD_df) - 3,
                                          ncol(accum_PD_df) - 4,
                                          ncol(accum_PD_df) - 5)], phylo, include.root=FALSE)


  # add Faith's PD information to the basic_accum_df:
  accum_PD_df <- cbind(accum_PD_df, PD_values$PD)
  colnames(accum_PD_df)[ncol(accum_PD_df)] <- "accum_PD"

  ## Express accumul PD as a proportion of the total PD (equivalent to FRic):
  accum_PD_df$accum_PD <- accum_PD_df$accum_PD / tot_PD_value


  ## create a new column that will contain the percentage of new species ...
  # ... seen on each video -> PD accumul with 100% at the end of the day ...
  # ... all the species richness seen during the day_site is seen at the ...
  # ... end of the day_site obviously ;) :
  accum_PD_df$perc_PD_acc_day <- rep(0, nrow(accum_PD_df))

  ## then fill this new column:
  ## for each site_day:
  for (i in (unique(accum_PD_df$site_day))) {

    sum_tot_richn <- accum_PD_df[which(accum_PD_df$site_day == i & accum_PD_df$hour_nb == "hour_9"), "accum_PD"]

    ## loop on the rows of each site_day:
    for (j in rownames(accum_PD_df[which(accum_PD_df$site_day == i), ])) {

      accum_PD_df[j, "perc_PD_acc_day"] <- (accum_PD_df[j, "accum_PD"]/sum_tot_richn)*100

    }
  }

  # PD as percentage:
  accum_PD_df$accum_PD <- accum_PD_df$accum_PD*100

  if (rich_plot == TRUE) {

    return_list <- list(richn_var, accum_PD_df)

    # and save:
    ggplot2::ggsave(filename = here::here("outputs", "PD_richn_day_var.pdf"),
                    plot = richn_var,
                    device = "pdf",
                    scale = 1,
                    height = 5000,
                    width = 8000,
                    units = "px",
                    dpi = 600)

  }

  if (rich_plot == FALSE) {

    return_list <- list(accum_PD_df)

  }

  return(return_list)


}




######




compute.pd.interday.accum <- function(basic_accum_df,
                                 rich_plot = TRUE) {



  # 1
  # First change species names with associated species which are present ...
  # ... in the fishtree phylogeny:
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



  # 2
  # Compute PD for each video:

  # first must compute a sp*asb df:
  sp_asb_df <- basic_accum_df
  rownames(sp_asb_df) <- sp_asb_df$hour_id


  # compute phylo:
  sp_nm_all <- colnames(sp_asb_df[, -c(ncol(sp_asb_df),
                                       ncol(sp_asb_df) - 1,
                                       ncol(sp_asb_df) - 2,
                                       ncol(sp_asb_df) - 3)],)
  phylo <- fishtree::fishtree_phylogeny(species = sp_nm_all)

  # compute Faith's PD:
  PD_values <- picante::pd(sp_asb_df[, -c(ncol(sp_asb_df),
                                          ncol(sp_asb_df) - 1,
                                          ncol(sp_asb_df) - 2,
                                          ncol(sp_asb_df) - 3)], phylo, include.root=FALSE)


  # add Faith's PD information to the basic_accum_df:
  basic_accum_df <- cbind(basic_accum_df, PD_values$PD)
  colnames(basic_accum_df)[ncol(basic_accum_df)] <- "hour_PD"


  # 3
  # Represent PD's variation:


  # first must compute the total PD's of the whole phylogeny so ...
  # ... Faith's PD from each video is expressed as a proportion of the total ...
  # ... phylogeny (as the FRic index):
  sp_all_df <- sp_asb_df[nrow(sp_asb_df), ]
  sp_all_df[1, ] <- 1
  rownames(sp_all_df) <- "PD_tot"
  tot_PD_value <- picante::pd(sp_all_df[, -c(ncol(sp_all_df),
                                             ncol(sp_all_df) - 1,
                                             ncol(sp_all_df) - 2,
                                             ncol(sp_all_df) - 3)], phylo, include.root=FALSE)
  tot_PD_value <- tot_PD_value$PD

  ## Express hour's PD as a proportion of the total PD:
  basic_accum_df$hour_PD <- basic_accum_df$hour_PD / tot_PD_value


  ## Plot PD's variation:
  basic_df <- basic_accum_df

  ## rename hour_nb from 1, 2 -> hour_1, hour_2 and give right levels:
  basic_df$hour_nb <- paste0(rep("hour_", nrow(basic_df)), sep = "", basic_df$hour_nb)
  basic_df$hour_nb  <- as.factor(basic_df$hour_nb)
  basic_df$hour_nb <- ordered(basic_df$hour_nb, levels = paste0(rep("hour_", 9),
                                                                  c(1:9)))

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

      ggplot2::geom_line(ggplot2::aes(y = hour_PD, x = hour_nb, group = day_nb,
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


      ggplot2::scale_x_discrete(labels= c("8:00-8:59", "9:00-9:59",
                                          "10:00-10:59", "11:00-11:59",
                                          "12:00-12:59", "13:00-13:59",
                                          "14:00-14:59", "15:00-15:59",
                                          "16:00-17:59")) +

      ggplot2::ylab("Proportion of total phylogenetic richness") +

      ggplot2::xlab("") +

      ggplot2::guides(fill = "none")

  }


  # 4
  # Now compute PD accumulation df to compute the accumulation of PD through ...
  # ... a day: as for FRic after the first occ, always 1 so the tree grows:


  ## for each site_day:
  for (i in (unique(basic_df$site))) {


    ## loop on species:
    for (j in (1:(ncol(basic_df) - 6))) {


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



  # 5
  # Compute PD's accumulation:

  accum_PD_df <- basic_df

  # compute the sp richness accumulation:

  rownames(accum_PD_df) <- accum_PD_df$hour_id

  PD_values <- picante::pd(accum_PD_df[, -c(ncol(accum_PD_df),
                                            ncol(accum_PD_df) - 1,
                                            ncol(accum_PD_df) - 2,
                                            ncol(accum_PD_df) - 3,
                                            ncol(accum_PD_df) - 4,
                                            ncol(accum_PD_df) - 5)], phylo, include.root=FALSE)


  # add Faith's PD information to the basic_accum_df:
  accum_PD_df <- cbind(accum_PD_df, PD_values$PD)
  colnames(accum_PD_df)[ncol(accum_PD_df)] <- "accum_PD"

  ## Express accumul PD as a proportion of the total PD (equivalent to FRic):
  accum_PD_df$accum_PD <- accum_PD_df$accum_PD / tot_PD_value

  if (rich_plot == TRUE) {

    return_list <- list(richn_var, accum_PD_df, basic_df)

    # and save:
    ggplot2::ggsave(filename = here::here("outputs", "PD_richn_interday_var.pdf"),
                    plot = richn_var,
                    device = "pdf",
                    scale = 1,
                    height = 5000,
                    width = 8000,
                    units = "px",
                    dpi = 600)

  }

  if (rich_plot == FALSE) {

    return_list <- list(accum_PD_df, basic_df)

  }

  return(return_list)


}

