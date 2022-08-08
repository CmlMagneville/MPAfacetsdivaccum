###############################################################################
##
## Script to code a function to merge n occ dataframes of different ...
## ... cameras into a single one for a given day. Scale: videos
##
## 1_Merge_cam_df.R
##
## 03/08/2022 
##
## Camille Magneville
##
###############################################################################



# CREATE DOCUMENTATION



merge.cam.vid.df <- function(list_df) {
  
  
  # create a df which will contain data from the two dataframes ...
  # ... but first is the first pres abs df and will be completed:
  merged_df <- list_df[[1]]
  time_vid_df <- merged_df[, c(ncol(merged_df) - 1, ncol(merged_df))]
  merged_df <- merged_df[, -c(ncol(merged_df) - 1, ncol(merged_df))]
  
  # loop on each other dataframe that should be merged (in Mayotte1 paper ...
  # ... only another one df to merge):
  
  for (i in (1:length(list_df))) {
    
    # loop on each column of this df: if the column already exists in the ...
    # ... merged df than add values (same nb of rows reflecting videos) ...
    # ... if the column doesn't exist in the merged_df then add it:
    
    for (j in (1:(ncol(list_df[[i]]) - 2))) {
      
      sp_nm <- colnames(list_df[[i]])[j]
      
      
      # if species already in the merged_df:
      if (sp_nm %in% colnames(merged_df)) {
        merged_df[, sp_nm] <- merged_df[, sp_nm] + list_df[[i]][, j]
      }
      
      # if species not in the merged_df:
      if (! sp_nm %in% colnames(merged_df)) {
        merged_df <- tibble::add_column(merged_df, new_sp = list_df[[i]][, j])
        colnames(merged_df)[ncol(merged_df)] <- sp_nm
      }

   } # end loop on columns (species)
    
  } # end loop on dfs
  
  
  # to all values > 1 -> 1 because we are studying occurrence of species ...
  # ... at a certain time:
  merged_occurrence_df <- merged_df
  merged_occurrence_df[merged_occurrence_df > 1] <- 1
  
  # re-add time and vido information and rename video info:
  merged_occurrence_df <- cbind(merged_occurrence_df, time_vid_df)
  merged_occurrence_df$video_nm <- rep(paste0("vid", sep = "_", 
                                              c(1:nrow(merged_occurrence_df))))
  
  # return the dataframe containing the data from the n dfs (n cam) ...
  # ... of the studied day with only 0 or 1: at a given given video, does ...
  # ... the species shows on the two cameras? 
  
  return(merged_occurrence_df)

}



##############################################################################



# CREATE DOCUMENTATION list1 concerne NG and list2 B 


NG_03 <- merge.cam.vid.df(list_df = list(presabs_03_A1, presabs_03_C2))
NG_05 <- merge.cam.vid.df(list_df = list(presabs_05_A1, presabs_05_C2))
NG_08 <- merge.cam.vid.df(list_df = list(presabs_08_A1, presabs_08_C2))
B_04 <- merge.cam.vid.df(list_df = list(presabs_04_A1, presabs_04_C2))
B_06 <- merge.cam.vid.df(list_df = list(presabs_06_A1, presabs_06_C2))
B_09 <- merge.cam.vid.df(list_df = list(presabs_09_A1, presabs_09_C2))

list1 <- list(NG_03, NG_05, NG_08)
names(list1) <- c("NG_03", "NG_05", "NG_08")
list2 <- list(B_04, B_06, B_09)
names(list2) <- c("B_04", "B_06", "B_09")

asb_sp_df <- create.asb.sp.site(list1, list2)

create.asb.sp.site <- function(list1, list2) {
  
  
  # Get the list of all species seen:
  
  ## for list 1 first:
  sp_list1 <- c()
  
  for (i in (1:length(list1))) {
    sp_list1 <- append(sp_list1, colnames(list1[[i]]))
  }
  
  ## for list 2 then:
  sp_list2 <- c()
  
  for (i in (1:length(list2))) {
    sp_list2 <- append(sp_list2, colnames(list2[[i]]))
  }
  
  ## then gather the two lists and clean:
  sp_all <- c(sp_list1, sp_list2)
  sp_all <- unique(sp_all)
  sp_all <- sp_all[which(! sp_all %in% c("time", "video_nm"))]
  
  
  # Create a dfs that will contains 0, 1 for all species (columns) ...
  # ... according to whether they are present in a given asb (row) or not:
  
  sp_asb_df <- as.data.frame(matrix(NA, ncol = length(sp_all),
                                        nrow = length(list1) + length(list2)))
  colnames(sp_asb_df) <- sort(sp_all)
  rownames(sp_asb_df) <- c(names(list1), names(list2))
  
  
  # fill the df:
  
  ## for each df in the list1 get its species and add 1 in the sp_asb_df if present:
  for (i in (1:length(list1))) {
    
    nm_day <- names(list1)[i]
    sp_list <- colnames(list1[[i]])
    
    
    for (j in (1:ncol(sp_asb_df))) {
      
      # get the species name of the column j:
      sp_studied <- colnames(sp_asb_df)[j]
      
      # if present in the ith dfs of the list1 then add 1:
      if (sp_studied %in% sp_list) {
        sp_asb_df[nm_day, j] <- 1
      }
      
    } # end loop on columns of sp_asb_df
    
  } # end loop on dfs from list1
  
  
  ## idem for list2
  for (i in (1:length(list2))) {
    
    nm_day <- names(list2)[i]
    sp_list <- colnames(list2[[i]])
    
    
    for (j in (1:ncol(sp_asb_df))) {
      
      # get the species name of the column j:
      sp_studied <- colnames(sp_asb_df)[j]
      
      # if present in the ith dfs of the list2 then add 1:
      if (sp_studied %in% sp_list) {
        sp_asb_df[nm_day, j] <- 1
      }
      
    } # end loop on columns of sp_asb_df
    
  } # end loop on dfs from list2
  
  
  # replace NA by 0:
  sp_asb_df[is.na(sp_asb_df)] <- 0
  
  
  return(sp_asb_df)
  
}
