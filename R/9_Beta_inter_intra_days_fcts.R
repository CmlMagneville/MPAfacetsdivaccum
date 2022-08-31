###############################################################################
##
## Functions to compute beta diversities between videos inter and intra days ...
## ... and plot them
##
## 9_Beta_inter_intra_days_fcts.R
##
## 19/08/2022
##
## Camille Magneville
##
###############################################################################




#' Title
#'
#' @param beta_df
#'
#' @return
#' @export
#'
#' @examples


beta.video.clean.df <- function(beta_df) {


  # use the mFD::dist_to_df function to transfrom the distance object:
  beta_df2 <- mFD::dist.to.df(list(beta = beta_df))

  # add three columns:
  beta_df2$same_day <- rep(FALSE, nrow(beta_df2))
  beta_df2$same_video <- rep(FALSE, nrow(beta_df2))
  beta_df2$same_site <- rep(FALSE, nrow(beta_df2))
  beta_df2$site_nm <- rep("different_sites", nrow(beta_df2))


  # loop on each pair of species or fe:
  for (i in (1:nrow(beta_df2))) {

    # retrieve days informations:
    day1 <- substr(beta_df2[i, 1], 5, 14)
    day2 <- substr(beta_df2[i, 2], 5, 14)

    # retrieve video information:
    nm1 <- substr(beta_df2[i, 1], nchar(as.character(beta_df2[i, 1])) - 7 + 1,
                  nchar(as.character(beta_df2[i, 1])))
    vid1 <- paste0("vid", sep = "", gsub(".*2019","", nm1))
    nm2 <- substr(beta_df2[i, 2], nchar(as.character(beta_df2[i, 2])) - 7 + 1,
                  nchar(as.character(beta_df2[i, 2])))
    vid2 <- paste0("vid", sep = "", gsub(".*2019","", nm2))

    # if same day and different video:
    if(day1 == day2 & vid1 != vid2) {
      beta_df2[i, "same_day"] <- TRUE
    }

    # if different day and same video:
    if(day1 != day2 & vid1 == vid2) {
      beta_df2[i, "same_video"] <- TRUE
    }

    # add site information:

    # if both days are in N'Gouja:
    if (day1 %in% c("03-11-2019", "05-11-2019", "08-11-2019") &
        day2 %in% c("03-11-2019", "05-11-2019", "08-11-2019")) {
      beta_df2[i, "same_site"] <- TRUE
      beta_df2[i, "site_nm"] <- "N'Gouja"

    }

    # if both days are in Boueni:
    if (day1 %in% c("04-11-2019", "06-11-2019", "09-11-2019") &
        day2 %in% c("04-11-2019", "06-11-2019", "09-11-2019")) {
      beta_df2[i, "same_site"] <- TRUE
      beta_df2[i, "site_nm"] <- "Boueni"

    }

  }

  return(beta_df2)

}



####


#' Title
#'
#' @param beta_TD_df
#'
#' @return
#' @export
#'
#' @examples


plot.boxplots.beta <- function(beta_df, metric) {


  # add a column saying if intra day or inter day compaison (or nothing):
  beta_df$compare <- rep("none", nrow(beta_df))

  # loop for each pair:
  for (i in (1:nrow(beta_df))) {

    if (beta_df$same_day[i] == TRUE) {
      beta_df$compare[i] <- "intraday"
    }

    if (beta_df$same_video[i] == TRUE & beta_df$same_site[i] == TRUE) {
      beta_df$compare[i] <- "interday"
    }

  } # end loop on each pair


  # for plot number 2, add day information for intra/interdays pairs:

  beta_df$pair_day <- rep("none", nrow(beta_df))

  for(i in (1:nrow(beta_df))) {

    if (beta_df$compare[i] == "intraday") {

      beta_df$pair_day[i] <- paste0(beta_df$compare[i], sep = "_",
                                    stringr::str_sub(beta_df$x1[i], 5, 14))

    }

    if (beta_df$compare[i] == "interday") {

      beta_df$pair_day[i] <- paste0(beta_df$compare[i], sep = "_",
                                    stringr::str_sub(beta_df$x1[i], 5, 14), sep = "_",
                                    stringr::str_sub(beta_df$x2[i], 5, 14))

    }

  }



  # plot 1:

  compare_labs <- c("Inter-days", "Intra-day")
  names(compare_labs) <- c("interday", "intraday")

  plot_beta1 <- ggplot2::ggplot(data = beta_df[which(beta_df$compare %in% c("interday", "intraday")), ],

                               ggplot2::aes(x = site_nm, y = beta, fill = site_nm)) +


              ggplot2::geom_jitter(alpha = 0.6, ggplot2::aes(color = site_nm)) +

              ggplot2::geom_boxplot(alpha = 0.5, outlier.shape = NA) +

              ggplot2::facet_grid(.~ compare,
                        labeller = ggplot2::labeller(compare = compare_labs)) +

    ggplot2::scale_fill_manual(values = c("#bf812d",
                                          "#80cdc1"),
                               name = "Site") +

    ggplot2::scale_colour_manual(values = c("#bf812d",
                                            "#80cdc1"),
                                 name = "Site") +

              ggplot2::xlab("") +

              ggplot2::ylab(paste0("Videos Beta", sep = " ", metric)) +

    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90"))


  ggplot2::ggsave(filename = here::here("outputs", paste0("beta_interintra_sites", sep = "_", metric, sep = "", ".pdf")),
                  plot = plot_beta1,
                  device = "pdf",
                  scale = 0.9,
                  height = 6000,
                  width = 10000,
                  units = "px",
                  dpi = 800)


  # plot 2:

  compare_labs <- c("Inter-days", "Intra-day")
  names(compare_labs) <- c("interday", "intraday")

  plot_beta2_NG <- ggplot2::ggplot(data = beta_df[which(beta_df$compare %in% c("interday", "intraday") &
                                                        beta_df$site_nm == "N'Gouja"), ],

                                ggplot2::aes(x = pair_day, y = beta),
                                fill = "#80cdc1") +


    ggplot2::geom_jitter(alpha = 0.6, color = "#80cdc1") +

    ggplot2::geom_boxplot(alpha = 0.5, outlier.shape = NA) +

    ggplot2::xlab("") +

    ggplot2::ylab(paste0("Videos Beta", sep = " ", metric)) +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +


    ggplot2::scale_x_discrete(labels= c("Inter ; Day 1 - Day 2", "Inter ; Day 1 - Day 3",
                                        "Inter ; Day 2 - Day 3",
                                        "Intra ; Day 1" , "Intra ; Day 2",
                                        "Intra ; Day 3"))

  plot_beta2_B <- ggplot2::ggplot(data = beta_df[which(beta_df$compare %in% c("interday", "intraday") &
                                                          beta_df$site_nm == "Boueni"), ],

                                   ggplot2::aes(x = pair_day, y = beta),
                                   fill = "#bf812d") +


    ggplot2::geom_jitter(alpha = 0.6, color = "#bf812d") +

    ggplot2::geom_boxplot(alpha = 0.5, outlier.shape = NA) +

    ggplot2::xlab("") +

    ggplot2::ylab(paste0("Videos Beta", sep = " ", metric)) +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +


    ggplot2::scale_x_discrete(labels= c("Inter ; Day 1 - Day 2", "Inter ; Day 1 - Day 3",
                                        "Inter ; Day 2 - Day 3",
                                        "Intra ; Day 1" , "Intra ; Day 2",
                                        "Intra ; Day 3"))

  plot_NG <- plot_beta2_NG +
    ggplot2::ggtitle("N'Gouja")

  plot_B <- plot_beta2_B +
    ggplot2::ggtitle("Boueni")

  plot_beta2_both <- (plot_NG + plot_B) +
    patchwork::plot_layout(byrow = TRUE, heights = c(1, 1), widths = c(1, 1),
                           ncol = 2, nrow = 1, guides = "collect")



  ggplot2::ggsave(filename = here::here("outputs", paste0("beta_divide_days", sep = "_", metric, sep = "", ".pdf")),
                  plot = plot_beta2_both,
                  device = "pdf",
                  scale = 0.9,
                  height = 6000,
                  width = 10000,
                  units = "px",
                  dpi = 800)


  return(list(plot_beta1, plot_beta2_both))


}


#####



#' Title
#'
#' @param beta_facet_df
#'
#' @return
#' @export
#'
#' @examples
#'


temp.decay <- function(beta_facet_df, metric) {


  # only keep the intra day data:
  beta_intra_df <- beta_facet_df[which(beta_facet_df$same_day == TRUE), ]
  # ... 3168 = combination of two videos on 33 videos * 3 days = 1056 * 3

  # columns x1 and x2 are characters:
  beta_intra_df$x1 <- as.character(beta_intra_df$x1)
  beta_intra_df$x2 <- as.character(beta_intra_df$x2)

  # add one column to the beta_intra_df which contains the temporal diff ...
  # ... between two videos ie the number of videos between them:
  beta_intra_df$diff_vid_nb <- rep(0, nrow(beta_intra_df))

  # span across rows to compute the video difference:
  for(i in (1:nrow(beta_intra_df))) {

    vid1 <- as.numeric(stringr::str_sub(beta_intra_df$x1[i], 16, nchar(beta_intra_df$x1[i])))
    vid2 <- as.numeric(stringr::str_sub(beta_intra_df$x2[i], 16, nchar(beta_intra_df$x2[i])))
    beta_intra_df$diff_vid_nb[i] <- abs(vid1 - vid2)

  }

  # columns in the right format:
  beta_intra_df$diff_vid_nb <- as.numeric(beta_intra_df$diff_vid_nb)
  beta_intra_df$beta <- as.numeric(beta_intra_df$beta)


  # plot:
  plot_temp_decay <- ggplot2::ggplot(data = beta_intra_df) +

    ggplot2::geom_point(ggplot2::aes(x = diff_vid_nb,
                                     y = beta,
                                     color = site_nm),
                        alpha = 0.2) +

    ggplot2::geom_smooth(ggplot2::aes(x = diff_vid_nb,
                                      y = beta,
                                      color = site_nm),
                         method = "loess") +

    ggplot2::scale_colour_manual(values = c("#bf812d",
                                            "#80cdc1"),
                                 name = "Site") +

    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +

    ggplot2::xlab("Temporal decay")


  ggplot2::ggsave(filename = here::here("outputs", paste0("Temp_decay", sep = "_", metric,
                                                          sep = "_", ".pdf")),
                  plot = plot_temp_decay,
                  device = "pdf",
                  scale = 1,
                  height = 6000,
                  width = 8000,
                  units = "px",
                  dpi = 800)


  return(plot_temp_decay)

}



####


permdisp.test <- function(beta_facet_df) {

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


  ## Compute the dispersion with site_day variable:
  dispersion <- vegan::betadisper(dist, group = beta_env_df$site_day)
  pmod <- vegan::permutest(dispersion, pairwise = TRUE, permutations = 99)
  plot <- plot(dispersion, hull=TRUE, ellipse=TRUE)


  return(list(pmod, plot))

}
