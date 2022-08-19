###############################################################################
##
## Script to code functions linked with mFD exploratory analysis:
## ... 1/ fct to go from FE list and sp*asb df to FE*asb df
##
## 1_Merge_cam_df.R
##
## 03/08/2022
##
## Camille Magneville
##
###############################################################################



# CREATE DOCUMENTATION
# sp_fe_list from mFD sp.to.fe() output $sp_fe


from.spfe.to.feasb <- function(fe_nm, sp_fe, asb_sp_w) {


  # create a dataframe that will contain fe*asb data:
  asb_fe <- as.data.frame(matrix(0,
                                 ncol = length(fe_nm),
                                 nrow = nrow(asb_sp_w)))
  colnames(asb_fe) <- fe_nm
  rownames(asb_fe) <- rownames(asb_sp_w)


  # fill the asb_fe df by looping on species, linking sp -> FE:
  for (i in names(sp_fe)) {

    # get the FE associated with the i species:
    assoc_FE <- sp_fe[which(names(sp_fe) == i)][[1]]

    # get the presabs values of the i species:
    values_sp <- asb_sp_w[, which(colnames(asb_sp_w) == i)]

    # fill the asb_fe df:
    asb_fe[, assoc_FE] <- asb_fe[, assoc_FE] + values_sp

  }



  # build the asb_fe df that only contains occurrence of the FE in each ...
  # ... assemblage:
  asb_fe_occ <- asb_fe
  asb_fe_occ[asb_fe_occ > 1] <- 1

  return(list("asb_fe_occ" = asb_fe_occ,
              "asb_fe_w" = asb_fe))

}



####


fe.sp.df.computation <- function(sp_to_fe) {


  # create a dataframe that contain fe_nm and sp_nm:
  fe_sp_df <- as.data.frame(matrix(ncol = 2, nrow = 1))
  colnames(fe_sp_df) <- c("fe_nm", "species_nm")

  # loop on fes:
  for (i in (1:length(sp_to_fe$fe_nm))) {

    sp_nm <- search.sp.nm(sp_to_fe, paste0("fe", sep = "_", i))

    # loop on species:
    for (j in (1:length(sp_nm))) {

      fe_sp_df[nrow(fe_sp_df) + 1, "fe_nm"] <- paste0("fe", sep = "_", i)
      fe_sp_df[nrow(fe_sp_df), "species_nm"] <- sp_nm[j]

    } # end loop on species of the studied fe

  } # end loop on FEs

  return(fe_sp_df[-1, ])

}


#####


## DOC TO DO

# fct to search a species name given a fe name:


search.sp.nm <- function(sp_to_fe, nm_fe) {

  sp_vect <- c()

  for (i in names(sp_to_fe$sp_fe)) {

    if (sp_to_fe$sp_fe[[i]] == nm_fe) {

      sp_vect <- append(sp_vect, i)

    }

  }

  return(sp_vect)

}
