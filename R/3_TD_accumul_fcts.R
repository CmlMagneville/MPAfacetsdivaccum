###############################################################################
##
## Script to code functions linked with TD accumulation
##
## 3_TD_accul_fcts.R
##
## 09/08/2022
##
## Camille Magneville
##
###############################################################################



#' Title
#'
#' @param dfs_list it is important to give the dataframe in the same order than
#' in the \code{day_vect}
#' @param day_vect
#'
#' @return
#' @export
#'
#' @examples


create.complete.df <- function(dfs_list, days_vect) {


  # get the total number of species:
  nm_sp_all <- c()
  for (i in 1:length(dfs_list)) {
    nm_sp_all <- append(nm_sp_all, colnames(dfs_list[[i]]))
  }
  nm_sp_all <- unique(nm_sp_all)
  # remove time and video nm:
  nb_sp <- length(nm_sp_all) - 2


  # create the dataframe which will contain all data:
  complete_df <- as.data.frame(matrix(nrow = 1, ncol = nb_sp + 3))
  colnames(complete_df) <- c(sort(nm_sp_all[which(! nm_sp_all %in% c("time", "video_nm"))]),
                             "video_nb", "day", "vid_id")


  # make a loop on each site_day :
  for (i in (1:length(dfs_list))) {

    # get each video information:
    for (j in (1:nrow(dfs_list[[i]]))) {

      vid_nb <- j
      day <- days_vect[i]
      vid_id <- paste0("vid", sep = "_", day, sep = "_", vid_nb)

     # fill the new df with video and day informations:
     complete_df[nrow(complete_df) + 1,] <- c(rep(0, ncol(complete_df) - 3),
                                              vid_nb, day, vid_id)

     # add 1 where a given species is seen:
     studied_df <- dfs_list[[i]]

     ## get the names of the species which are seen on the studied video:
     sp_seen <- colnames(studied_df[, which(studied_df[j, ] != 0)])
     sp_seen <- sp_seen[which(! sp_seen %in% c("time", "video_nm"))]

     ## add 1:
     complete_df[nrow(complete_df), which(colnames(complete_df) %in% sp_seen)] <- rep(1, length(sp_seen))

    }

  }


  # remove the first row which is empty:
  complete_df <- complete_df[-1, ]

  return(complete_df)


}



#######



#' Title
#'
#' @param basic_accum_df
#' @param rich_plot
#'
#' @return
#' @export
#'
#' @examples
#'


compute.td.day.accum <- function(basic_accum_df, rich_plot = TRUE) {


  # first I compute species richness for each video and plot it if asked...
  # ... to see species variation through a day:

  # add species richness column:
  basic_df <- basic_accum_df

  ## make every column numeric:
  for (i in (1:(ncol(basic_df) - 4))) {
    basic_df[, i] <- as.numeric(basic_df[, i])
  }

  ## add sp richness column
  basic_df$richn <- apply(basic_df[, which(! colnames(basic_df) %in%
                                             c("video_nb", "day", "vid_id", "site"))],
                          1, sum)

  # express species richness as a proportion of total species richness ...
  # ... so its expressed as FRic (and PD):
  tot_sp_richn <- ncol(basic_df[, which(! colnames(basic_df) %in%
                                          c("video_nb", "day", "vid_id", "site"))])
  basic_df$richn <- basic_df$richn / tot_sp_richn

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

      ggplot2::geom_boxplot(ggplot2::aes(y = richn, x = video_nb),
                            color = "grey70", fill = "grey80", alpha = 0.5) +

      ggplot2::geom_jitter(ggplot2::aes(y = richn, x = video_nb,
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

      ggplot2::ylab("Proportion of total species richness") +

      ggplot2::xlab("") +

      ggplot2::guides(fill = "none")

  }



  # now compute TD accum: first occ 1 and 0 after each first occ of the species:

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
          basic_df[which(as.numeric(rownames(basic_df)) > as.numeric(k) & basic_df$site_day == i), j] <- 0
        }

      } # loop on the rows

    } # end loop on species

  } # end loop on site_day


  accum_TD_df <- basic_df

  # compute the sp richness accumulation:

  ## first compute the sum of new species seen on each video:
  accum_TD_df$sum_accum <- apply(accum_TD_df[,
                        which(! colnames(accum_TD_df) %in% c("video_nb",
                                                             "day", "vid_id",
                                                             "site", "richn",
                                                             "site_day"))],
                                1,
                                sum)




  ## create a new column that will contain the percentage of new species ...
  # ... seen on each video -> TD accumul with 100% at the end of the day ...
  # ... all the species richness seen during the day_site is seen at the ...
  # ... end of the day_site obviously ;) :
  accum_TD_df$perc_TD_acc_day <- rep(0, nrow(accum_TD_df))

  ## then fill this new column:
  ## for each site_day:
  for (i in (unique(accum_TD_df$site_day))) {

    sum_tot_richn <- sum(accum_TD_df[which(accum_TD_df$site_day == i), "sum_accum"])
    # express it as percentage of total sp richness:
    sum_tot_richn <- sum_tot_richn/tot_sp_richn

    # create a counter that will count the rows (as j in a name):
    k <- 0

    ## loop on the rows of each site_day:
    for (j in rownames(accum_TD_df[which(accum_TD_df$site_day == i), ])) {

      k <- k + 1

      # if first video of the site_day, then no value to add:
      if (k == 1) {
        accum_TD_df[j, "perc_TD_acc_day"] <- accum_TD_df[j, "sum_accum"] / tot_sp_richn
        accum_TD_df[j, "perc_TD_acc_day"] <- (accum_TD_df[j, "perc_TD_acc_day"]/sum_tot_richn)*100
      }

      # if it is not the first video, then must add accumulated sp_richn of the videos before:
      else {

        # get the rowname of the video before: and of the first video of the site_day:
        rown_vid_before <- rownames(accum_TD_df[which(accum_TD_df$site_day == i &
                                                      accum_TD_df$video_nb == paste0("vid", sep = "_", k-1)), ])
        rown_first_vid <- rownames(accum_TD_df[which(accum_TD_df$site_day == i &
                                                       accum_TD_df$video_nb == paste0("vid", sep = "_", 1)), ])

        # compute the accumulated species richness up to the studied video:
        accum_TD_df[j, "perc_TD_acc_day"] <- accum_TD_df[j, "sum_accum"] +
                             sum(accum_TD_df[c(rown_first_vid:(rown_vid_before)), "sum_accum"])

        # express as percentage of total sp richn (as FRic):
        accum_TD_df[j, "perc_TD_acc_day"] <- accum_TD_df[j, "perc_TD_acc_day"]/tot_sp_richn

        # compute the percentage of species richness accumulated up to the studied video:
        accum_TD_df[j, "perc_TD_acc_day"] <- (accum_TD_df[j, "perc_TD_acc_day"]/sum_tot_richn)*100
      }

    }
  }

  if (rich_plot == TRUE) {

    return_list <- list(richn_var, accum_TD_df)

    # and save:
    ggplot2::ggsave(filename = here::here("outputs", "TD_richn_day_var.pdf"),
                    plot = richn_var,
                    device = "pdf",
                    scale = 1,
                    height = 5000,
                    width = 8000,
                    units = "px",
                    dpi = 600)

  }

  if (rich_plot == FALSE) {

    return_list <- list(accum_TD_df)

  }

  return(return_list)

}



######



#' Title
#'
#' @param basic_accum_df
#' @param rich_plot
#'
#' @return
#' @export
#'
#' @examples


compute.td.interday.accum <- function(basic_accum_df, rich_plot = TRUE) {


  # first I compute species richness for each video and plot it if asked...
  # ... to see species variation through a day:

  # add species richness column:
  basic_df <- basic_accum_df

  ## make every column numeric:
  for (i in (1:(ncol(basic_df) - 4))) {
    basic_df[, i] <- as.numeric(basic_df[, i])
  }

  ## add sp richness column
  basic_df$richn <- apply(basic_df[, which(! colnames(basic_df) %in%
                                             c("video_nb", "day", "vid_id", "site"))],
                          1, sum)

  # express species richness as a proportion of total species richness ...
  # ... so its expressed as FRic (and PD):
  tot_sp_richn <- ncol(basic_df[, which(! colnames(basic_df) %in%
                                          c("video_nb", "day", "vid_id", "site"))])
  basic_df$richn <- basic_df$richn / tot_sp_richn

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

      ggplot2::geom_line(ggplot2::aes(y = richn, x = video_nb,group = day_nb,
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

      ggplot2::ylab("Proportion of total species richness") +

      ggplot2::xlab("") +

      ggplot2::guides(fill = "none")

  }



  # now compute TD accum: first occ 1 and 1 after each first occ of the species:

  ## for each site:
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



  accum_TD_df <- basic_df

  # compute the sp richness accumulation:

  accum_TD_df$accum_TD <- apply(accum_TD_df[,
                                             which(! colnames(accum_TD_df) %in% c("video_nb",
                                                                                  "day", "vid_id",
                                                                                  "site", "richn",
                                                                                  "day_nb"))],
                                 1,
                                 sum)


  ## Express accumul TD as a proportion of the total sp richness (equivalent to FRic):
  tot_TD_value <- ncol(accum_TD_df[,
                                   which(! colnames(accum_TD_df) %in% c("video_nb",
                                                                        "day", "vid_id",
                                                                        "site", "richn",
                                                                        "day_nb", "accum_TD"))])
  accum_TD_df$accum_TD <- accum_TD_df$accum_TD / tot_TD_value


  if (rich_plot == TRUE) {

    return_list <- list(richn_var, accum_TD_df,basic_df)

    # and save:
    ggplot2::ggsave(filename = here::here("outputs", "TD_richn_interday_var.pdf"),
                    plot = richn_var,
                    device = "pdf",
                    scale = 1,
                    height = 5000,
                    width = 8000,
                    units = "px",
                    dpi = 600)

  }

  if (rich_plot == FALSE) {

    return_list <- list(accum_TD_df, basic_df)

  }

  return(return_list)

}



