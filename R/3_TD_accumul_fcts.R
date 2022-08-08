###############################################################################
##
## Script to code functions linked with TD accumulation
##
## 3_TD_accul_fcts.R
##
## 03/08/2022
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
     complete_df[j + 1, which(colnames(complete_df) %in% sp_seen)] <- rep(1, length(sp_seen))

    }

  }


  # remove the first row which is empty:
  complete_df <- complete_df[-1, ]

  return(complete_df)


}
