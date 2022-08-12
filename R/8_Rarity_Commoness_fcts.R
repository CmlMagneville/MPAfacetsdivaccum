###############################################################################
##
## Script which gathers functions to find rare/medium/common species ...
## ... in both sites and where they  are in the functional space and ...
## .. the phylogenetic tree
##
## 8_Rarity_Commonness_fcts.R
##
## 12/08/2022
##
## Camille Magneville
##
###############################################################################






#' Title
#'
#' @param basic_accum_df
#'
#' @return
#' @export
#'
#' @examples


rarcom.computation <- function(basic_accum_df) {


  # Create a new dataf=table which gathers site, species and occ in terms of ...
  # ... video in which each species occurs:
  rarcom_df <- as.data.frame(matrix(ncol = 3, nrow = 1))
  colnames(rarcom_df) <- c("site", "species_nm", "vid_occ_nb")


  # numeric format to all species:
  basic_accum_df[, which(! colnames(basic_accum_df) %in%
        c("video_nb", "day", "vid_id", "site"))] <- apply(basic_accum_df[,
                                          which(! colnames(basic_accum_df) %in%
                                      c("video_nb", "day", "vid_id", "site"))],
                                      2, as.numeric)


  # get the names of species in each site:
  NG_data <- basic_accum_df[which(basic_accum_df$site == "N'Gouja"),
                            which(! colnames(basic_accum_df) %in%
                                    c("video_nb", "day", "vid_id", "site"))]
  B_data <- basic_accum_df[which(basic_accum_df$site == "Boueni"),
                           which(! colnames(basic_accum_df) %in%
                                   c("video_nb", "day", "vid_id", "site"))]

  sp_NG <- names(NG_data[which(apply(NG_data, 2, sum) != 0)]) #127 species ok
  sp_B <- names(B_data[which(apply(B_data, 2, sum) != 0)]) #112 species ok


  # Fill it using the basic_accum_df:

  ## loop on each species:
  for (j in (1:ncol(basic_accum_df[, which(! colnames(basic_accum_df) %in%
                                               c("video_nb", "day", "vid_id", "site"))]))) {


    ## if the studied species is in NG:
    if (colnames(basic_accum_df)[j] %in% sp_NG) {

      sp_nm <- colnames(basic_accum_df)[j]
      site <- "N'Gouja"
      vid_occ_nb <- sum(NG_data[which(names(NG_data) == sp_nm)])

      rarcom_df[nrow(rarcom_df) + 1, ] <- c(site, sp_nm, vid_occ_nb)

    }

    ## if the studied species is in the Boueni:
    if (colnames(basic_accum_df)[j] %in% sp_B) {

      sp_nm <- colnames(basic_accum_df)[j]
      site <- "Boueni"
      vid_occ_nb <- sum(B_data[which(names(B_data) == sp_nm)])

      rarcom_df[nrow(rarcom_df) + 1, ] <- c(site, sp_nm, vid_occ_nb)

    }

  } # end loop on species

  # remove the first row withNA:
  rarcom_df <- rarcom_df[-1, ]

  # Add a column to the rarcom_df with indicate if the sp is only seen in one ...
  # ... or both sites:
  NG_sp_nm <- rarcom_df[which(rarcom_df$site == "N'Gouja"), "species_nm"]
  B_sp_nm <- rarcom_df[which(rarcom_df$site == "Boueni"), "species_nm"]

  rarcom_df$site_presence <- rep(0, nrow(rarcom_df))

  for (i in (1:nrow(rarcom_df))) {

    sp_nm <- rarcom_df[i, "species_nm"]

    if (sp_nm %in% NG_sp_nm & sp_nm %in% B_sp_nm) {
      rarcom_df$site_presence[i] <- "both"
    }

    else {
      if (sp_nm %in% NG_sp_nm) {
        rarcom_df$site_presence[i] <- "N'Gouja only"
      }

      if (sp_nm %in% B_sp_nm) {
        rarcom_df$site_presence[i] <- "Boueni only"
      }
    }


  }

  # add rarity information:
  # rare =< 25% of total videos, medium 25-50, common >= 50%
  rarcom_df$vid_occ_nb <- as.numeric(rarcom_df$vid_occ_nb)
  rarcom_df$perc_vid_occ <- (rarcom_df$vid_occ_nb / 99)*100

  rarcom_df$rarity <- rep(0, nrow(rarcom_df))

  for (i in (rownames(rarcom_df))) {

    if (rarcom_df[i, "perc_vid_occ"] <= 25) {
      rarcom_df[i, "rarity"] <- "rare"
    }

    if (rarcom_df[i, "perc_vid_occ"] > 25 & rarcom_df[i, "perc_vid_occ"] < 50) {
      rarcom_df[i, "rarity"] <- "medium"
    }

    if (rarcom_df[i, "perc_vid_occ"] >= 50) {
      rarcom_df[i, "rarity"] <- "common"
    }


  }


  return(rarcom_df)

}


#####


#' Title
#'
#' @param rarcom_df
#' @param sites_colors
#'
#' @return
#' @export
#'
#' @example

plot.rarcom <- function(rarcom_df, sites_colors) {


  # right format:
  rarcom_df$site <- as.factor(rarcom_df$site)
  rarcom_df$site_presence <- as.factor(rarcom_df$site_presence)
  rarcom_df$vid_occ_nb <- as.numeric(rarcom_df$vid_occ_nb)

  # rename species:
  rarcom_df$species_nm <- gsub("_", " ", rarcom_df$species_nm)
  rarcom_df[c(2, 3), "species_nm"] <- "Ctenochaetus striatus"

  rarcom_plot_NG <- ggplot2::ggplot(data = rarcom_df[which(rarcom_df$site == "N'Gouja"), ]) +

    ggplot2::geom_bar(ggplot2::aes(x = reorder(species_nm, - vid_occ_nb), y = (vid_occ_nb/99)*100,
                                   fill = site_presence),
                      stat = "identity") +

    ggplot2::scale_fill_manual(values = c(sites_colors[1],
                                          sites_colors[3]),
                               name = "Site presence") +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 8),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +

    ggplot2::ylab("Percentage of occurrence in the videos") +

    ggplot2::xlab("")


  rarcom_plot_B <- ggplot2::ggplot(data = rarcom_df[which(rarcom_df$site == "Boueni"), ]) +

    ggplot2::geom_bar(ggplot2::aes(x = reorder(species_nm, - vid_occ_nb), y = (vid_occ_nb/99)*100,
                                   fill = site_presence),
                      stat = "identity") +

    ggplot2::scale_fill_manual(values = c(sites_colors[1],
                                          sites_colors[2]),
                               name = "Site presence") +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 8),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +

    ggplot2::ylab("Percentage of occurrence in the videos") +

    ggplot2::xlab("")


  plot_both <- (rarcom_plot_NG + rarcom_plot_B) +
    patchwork::plot_layout(byrow = TRUE, heights = c(1, 1), widths = c(1, 1),
                           ncol = 1, nrow = 2, guides = "collect")

  ggplot2::ggsave(filename = here::here("outputs", "rar_comm_vid.pdf"),
                  plot = plot_both,
                  device = "pdf",
                  scale = 0.6,
                  height = 13000,
                  width = 18000,
                  units = "px",
                  dpi = 800)


  return(plot_both)


}


#####


#' Title
#'
#' @param basic_fd_accum_df
#' @param sp_tr
#' @param tr_cat
#' @param rarcom_df
#'
#' @return
#' @export
#'
#' @examples
#'

spot.rare.sp.fd <- function(basic_fd_accum_df,
                            sp_tr,
                            tr_cat,
                            rarcom_df) {



}



