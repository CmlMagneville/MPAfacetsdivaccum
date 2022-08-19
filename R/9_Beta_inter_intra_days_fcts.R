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
  beta_df2 <- mFD::dist_to_df(list("beta_df" = beta_df))

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
    nm1 <- substr(beta_df2[i, 1], nchar(beta_df2[i, 1]) - 5 + 1,
                  nchar(beta_df2[i, 1]))
    vid1 <- paste0("vid", sep = "_", gsub("_*.","", nm1))
    nm2 <- substr(beta_df2[i, 2], nchar(beta_df2[i, 2]) - 5 + 1,
                  nchar(beta_df2[i, 2]))
    vid2 <- paste0("vid", sep = "_", gsub("_*.","", nm2))

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


plot.boxplots.beta <- function(beta_TD_df) {







}

