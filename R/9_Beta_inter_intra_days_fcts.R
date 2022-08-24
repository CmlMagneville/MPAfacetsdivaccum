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


  # plot:

  compare_labs <- c("Inter-days", "Intra-day")
  names(compare_labs) <- c("interday", "intraday")

  plot_beta <- ggplot2::ggplot(data = beta_df[which(beta_df$compare %in% c("interday", "intraday")), ],

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
                  plot = plot_beta,
                  device = "pdf",
                  scale = 0.9,
                  height = 6000,
                  width = 10000,
                  units = "px",
                  dpi = 800)


  return(plot_beta)


}

