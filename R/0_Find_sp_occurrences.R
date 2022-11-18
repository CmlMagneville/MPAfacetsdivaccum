################################################################################
##
## Function to find the names of the videos where a lits of species occur
##
## 0_Find_sp_occurrences.R
##
## 18/11/2022
##
## Camille Magneville
##
################################################################################



find.occurrences <- function(dfs_list, sp_vect) {


  # create the new dataframe:
  final_occ_df <- as.data.frame(matrix(data = NA, ncol = 2, nrow = 1))
  colnames(final_occ_df) <- c("Species", "Video_occ")


  # loop on species names:
  for (i in c(1:length(sp_vect))) {

    sp_nm <- sp_vect[i]

    # loop on presabs dataframes:
    for (j in c(1:length(dfs_list))) {

      studied_df <- dfs_list[[j]]


      # if the species is in the dfs:
      if (sp_nm %in% colnames(studied_df)){

        # then get the name of the video on which it occurs:
        sp_occ_data <- dplyr::filter(studied_df, get(sp_nm) == 1)
        vid_nm <- sp_occ_data$video_nm

        for (k in c(1:length(vid_nm))) {

          final_occ_df <- dplyr::add_row(final_occ_df,
                                          Species = sp_nm,
                                          Video_occ = vid_nm[k])

        }

      } # end if species in the df


    } # end loop on the presabs dataframe

  } # end loop on species

  # remove first NA row:
  final_occ_df <- final_occ_df[-1, ]

  return(final_occ_df)

}
