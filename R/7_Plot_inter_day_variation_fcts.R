###############################################################################
##
## Script to plot inter day accumulation for the three facets and 3 other FD ind
##
## 7_Plot_inter_day_variation.R
##
## 11/08/2022
##
## Camille Magneville
##
###############################################################################



plot.inter.day.accum <- function(TD_accum_df,
                                 PD_accum_df,
                                 FD_accum_df,
                                 sites_colors,
                                 linewidth) {


  # create a dataframe with info: site, day, video_nb, fric, td, pd:
  complete_plot_df <- TD_accum_df[, c(ncol(TD_accum_df) - 1,
                                      ncol(TD_accum_df) - 3,
                                      ncol(TD_accum_df) - 5,
                                      ncol(TD_accum_df) - 6,
                                      ncol(TD_accum_df))]
  colnames(complete_plot_df)[ncol(complete_plot_df)] <- "TD"

  # add FD and PD information:
  complete_plot_df$FRic <- FD_accum_df$fric
  complete_plot_df$FSpe <- FD_accum_df$fspe
  complete_plot_df$FDis <- FD_accum_df$fdis
  complete_plot_df$FDiv <- FD_accum_df$fdiv
  complete_plot_df$FIde_PC1 <- FD_accum_df$fide_PC1
  complete_plot_df$FIde_PC2 <- FD_accum_df$fide_PC2
  complete_plot_df$FIde_PC3 <- FD_accum_df$fide_PC3
  complete_plot_df$FIde_PC4 <- FD_accum_df$fide_PC4

  complete_plot_df$PD <- PD_accum_df$accum_PD


  # now merge TD, FD and PD columns -> metric:
  final_plot_df <- reshape2::melt(complete_plot_df,
                                  id.vars = c("day_nb", "site", "day", "video_nb"),
                                  variable.name = 'metric', value.name = 'values')

  # right class:
  final_plot_df$site <- as.factor(final_plot_df$site)
  final_plot_df$day <- as.factor(final_plot_df$day)
  final_plot_df$day <- as.factor(final_plot_df$day_nb)
  final_plot_df$video_nb <- ordered(final_plot_df$video_nb, levels = paste0(rep("video_", 33),
                                                                            c(1:33)))
  final_plot_df$metric <- as.factor(final_plot_df$metric)


  # Create new names from facets day_1, day_2 and day_3:
  day_labs <- c("Sampling day 1", "Sampling day 2", "Sampling day 3")
  names(day_labs) <- c("day_1", "day_2", "day_3")

  # plot 1 for TD:
  plot_TD <- ggplot2::ggplot(data = final_plot_df[which(final_plot_df$metric == "TD"), ]) +

    ggplot2::geom_line(ggplot2::aes(y = values, x = video_nb, group = site,
                                    color = site, fill = site),
                       size = 0.9) +

    ggplot2::facet_grid(. ~ day_nb,
                        labeller = ggplot2::labeller(day_nb = day_labs)) +


    ggplot2::scale_fill_manual(values = c("#bf812d",
                                          "#80cdc1"),
                               name = "Site") +

    ggplot2::scale_colour_manual(values = c("#bf812d",
                                            "#80cdc1"),
                                 name = "Site",
                                 labels = c("Poorly Protected",
                                            "Fully Protected")) +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +


    ggplot2::scale_x_discrete(labels= c("7:30", "", "", "", "8:40", "",
                                        "", "", "9:45", "", "", "",
                                        "10:55", "", "", "", "12:20", "",
                                        "", "", "13:40", "", "", "",
                                        "15:00", "", "", "", "16:30", "",
                                        "", "", "17:30")) +

    ggplot2::expand_limits(y = 0) +

    ggplot2::ylab("Species richness") +

    ggplot2::xlab("") +

    ggplot2::guides(fill = "none")


  # plot 1 for FRic:
  plot_FRic <- ggplot2::ggplot(data = final_plot_df[which(final_plot_df$metric == "FRic"), ]) +

    ggplot2::geom_line(ggplot2::aes(y = values, x = video_nb, group = site,
                                    color = site, fill = site),
                       size = 0.9) +

    ggplot2::facet_grid(. ~ day_nb,
                        labeller = ggplot2::labeller(day_nb = day_labs)) +


    ggplot2::scale_fill_manual(values = c("#bf812d",
                                          "#80cdc1"),
                               name = "Site") +

    ggplot2::scale_colour_manual(values = c("#bf812d",
                                            "#80cdc1"),
                                 name = "Site",
                                 labels = c("Poorly Protected",
                                            "Fully Protected")) +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +


    ggplot2::scale_x_discrete(labels= c("7:30", "", "", "", "8:40", "",
                                        "", "", "9:45", "", "", "",
                                        "10:55", "", "", "", "12:20", "",
                                        "", "", "13:40", "", "", "",
                                        "15:00", "", "", "", "16:30", "",
                                        "", "", "17:30")) +

    ggplot2::expand_limits(y = 0) +

    ggplot2::ylab("FRic") +

    ggplot2::xlab("") +

    ggplot2::guides(fill = "none")


  # plot 1 for PD:
  plot_PD <- ggplot2::ggplot(data = final_plot_df[which(final_plot_df$metric == "PD"), ]) +

    ggplot2::geom_line(ggplot2::aes(y = values, x = video_nb, group = site,
                                    color = site, fill = site),
                       size = 0.9) +

    ggplot2::facet_grid(. ~ day_nb,
                        labeller = ggplot2::labeller(day_nb = day_labs)) +


    ggplot2::scale_fill_manual(values = c("#bf812d",
                                          "#80cdc1"),
                               name = "Site") +

    ggplot2::scale_colour_manual(values = c("#bf812d",
                                            "#80cdc1"),
                                 name = "Site",
                                 labels = c("Poorly Protected",
                                            "Fully Protected")) +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +


    ggplot2::scale_x_discrete(labels= c("7:30", "", "", "", "8:40", "",
                                        "", "", "9:45", "", "", "",
                                        "10:55", "", "", "", "12:20", "",
                                        "", "", "13:40", "", "", "",
                                        "15:00", "", "", "", "16:30", "",
                                        "", "", "17:30")) +

    ggplot2::expand_limits(y = 0) +

    ggplot2::ylab("Faith's PD") +

    ggplot2::xlab("") +

    ggplot2::guides(fill = "none")


  # plot 1 for FDis:
  plot_FDis <- ggplot2::ggplot(data = final_plot_df[which(final_plot_df$metric == "FDis"), ]) +

    ggplot2::geom_line(ggplot2::aes(y = values, x = video_nb, group = site,
                                    color = site, fill = site),
                       size = 0.9) +

    ggplot2::facet_grid(. ~ day_nb,
                        labeller = ggplot2::labeller(day_nb = day_labs)) +


    ggplot2::scale_fill_manual(values = c("#bf812d",
                                          "#80cdc1"),
                               name = "Site") +

    ggplot2::scale_colour_manual(values = c("#bf812d",
                                            "#80cdc1"),
                                 name = "Site",
                                 labels = c("Poorly Protected",
                                            "Fully Protected")) +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +


    ggplot2::scale_x_discrete(labels= c("7:30", "", "", "", "8:40", "",
                                        "", "", "9:45", "", "", "",
                                        "10:55", "", "", "", "12:20", "",
                                        "", "", "13:40", "", "", "",
                                        "15:00", "", "", "", "16:30", "",
                                        "", "", "17:30")) +

    ggplot2::ylab("FDis") +

    ggplot2::xlab("") +

    ggplot2::guides(fill = "none")


  # plot 1 for FSpe:
  plot_FSpe <- ggplot2::ggplot(data = final_plot_df[which(final_plot_df$metric == "FSpe"), ]) +

    ggplot2::geom_line(ggplot2::aes(y = values, x = video_nb, group = site,
                                    color = site, fill = site),
                       size = 0.9) +

    ggplot2::facet_grid(. ~ day_nb,
                        labeller = ggplot2::labeller(day_nb = day_labs)) +


    ggplot2::scale_fill_manual(values = c("#bf812d",
                                          "#80cdc1"),
                               name = "Site") +

    ggplot2::scale_colour_manual(values = c("#bf812d",
                                            "#80cdc1"),
                                 name = "Site",
                                 labels = c("Poorly Protected",
                                            "Fully Protected")) +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +


    ggplot2::scale_x_discrete(labels= c("7:30", "", "", "", "8:40", "",
                                        "", "", "9:45", "", "", "",
                                        "10:55", "", "", "", "12:20", "",
                                        "", "", "13:40", "", "", "",
                                        "15:00", "", "", "", "16:30", "",
                                        "", "", "17:30")) +

    ggplot2::ylab("FSpe") +

    ggplot2::xlab("") +

    ggplot2::guides(fill = "none")


  # plot 1 for FDiv:
  plot_FDiv <- ggplot2::ggplot(data = final_plot_df[which(final_plot_df$metric == "FDiv"), ]) +

    ggplot2::geom_line(ggplot2::aes(y = values, x = video_nb, group = site,
                                    color = site, fill = site),
                       size = 0.9) +

    ggplot2::facet_grid(. ~ day_nb,
                        labeller = ggplot2::labeller(day_nb = day_labs)) +


    ggplot2::scale_fill_manual(values = c("#bf812d",
                                          "#80cdc1"),
                               name = "Site") +

    ggplot2::scale_colour_manual(values = c("#bf812d",
                                            "#80cdc1"),
                                 name = "Site",
                                 labels = c("Poorly Protected",
                                            "Fully Protected")) +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +


    ggplot2::scale_x_discrete(labels= c("7:30", "", "", "", "8:40", "",
                                        "", "", "9:45", "", "", "",
                                        "10:55", "", "", "", "12:20", "",
                                        "", "", "13:40", "", "", "",
                                        "15:00", "", "", "", "16:30", "",
                                        "", "", "17:30")) +

    ggplot2::ylab("FDiv") +

    ggplot2::xlab("") +

    ggplot2::guides(fill = "none")


  # plot 1 for FIde_PC1:
  plot_FIde <- ggplot2::ggplot(data = final_plot_df[which(final_plot_df$metric == "FIde_PC1"), ]) +

    ggplot2::geom_line(ggplot2::aes(y = values, x = video_nb, group = site,
                                    color = site, fill = site),
                       size = 0.9) +

    ggplot2::facet_grid(. ~ day_nb,
                        labeller = ggplot2::labeller(day_nb = day_labs)) +


    ggplot2::scale_fill_manual(values = c("#bf812d",
                                          "#80cdc1"),
                               name = "Site") +

    ggplot2::scale_colour_manual(values = c("#bf812d",
                                            "#80cdc1"),
                                 name = "Site",
                                 labels = c("Poorly Protected",
                                            "Fully Protected")) +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +


    ggplot2::scale_x_discrete(labels= c("7:30", "", "", "", "8:40", "",
                                        "", "", "9:45", "", "", "",
                                        "10:55", "", "", "", "12:20", "",
                                        "", "", "13:40", "", "", "",
                                        "15:00", "", "", "", "16:30", "",
                                        "", "", "17:30")) +

    ggplot2::ylab("FIde PC1") +

    ggplot2::xlab("") +

    ggplot2::guides(fill = "none")


  # plot 2 for FIde_PC2:
  plot_FIde2 <- ggplot2::ggplot(data = final_plot_df[which(final_plot_df$metric == "FIde_PC2"), ]) +

    ggplot2::geom_line(ggplot2::aes(y = values, x = video_nb, group = site,
                                    color = site, fill = site),
                       size = 0.9) +

    ggplot2::facet_grid(. ~ day_nb,
                        labeller = ggplot2::labeller(day_nb = day_labs)) +


    ggplot2::scale_fill_manual(values = c("#bf812d",
                                          "#80cdc1"),
                               name = "Site") +

    ggplot2::scale_colour_manual(values = c("#bf812d",
                                            "#80cdc1"),
                                 name = "Site",
                                 labels = c("Poorly Protected",
                                            "Fully Protected")) +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +


    ggplot2::scale_x_discrete(labels= c("7:30", "", "", "", "8:40", "",
                                        "", "", "9:45", "", "", "",
                                        "10:55", "", "", "", "12:20", "",
                                        "", "", "13:40", "", "", "",
                                        "15:00", "", "", "", "16:30", "",
                                        "", "", "17:30")) +

    ggplot2::ylab("FIde PC2") +

    ggplot2::xlab("") +

    ggplot2::guides(fill = "none")


  # plot 3 for FIde_PC3:
  plot_FIde3 <- ggplot2::ggplot(data = final_plot_df[which(final_plot_df$metric == "FIde_PC3"), ]) +

    ggplot2::geom_line(ggplot2::aes(y = values, x = video_nb, group = site,
                                    color = site, fill = site),
                       size = 0.9) +

    ggplot2::facet_grid(. ~ day_nb,
                        labeller = ggplot2::labeller(day_nb = day_labs)) +


    ggplot2::scale_fill_manual(values = c("#bf812d",
                                          "#80cdc1"),
                               name = "Site") +

    ggplot2::scale_colour_manual(values = c("#bf812d",
                                            "#80cdc1"),
                                 name = "Site",
                                 labels = c("Poorly Protected",
                                            "Fully Protected")) +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +


    ggplot2::scale_x_discrete(labels= c("7:30", "", "", "", "8:40", "",
                                        "", "", "9:45", "", "", "",
                                        "10:55", "", "", "", "12:20", "",
                                        "", "", "13:40", "", "", "",
                                        "15:00", "", "", "", "16:30", "",
                                        "", "", "17:30")) +

    ggplot2::ylab("FIde PC3") +

    ggplot2::xlab("") +

    ggplot2::guides(fill = "none")


  # plot 3 for FIde_PC4:
  plot_FIde4 <- ggplot2::ggplot(data = final_plot_df[which(final_plot_df$metric == "FIde_PC4"), ]) +

    ggplot2::geom_line(ggplot2::aes(y = values, x = video_nb, group = site,
                                    color = site, fill = site),
                       size = 0.9) +

    ggplot2::facet_grid(. ~ day_nb,
                        labeller = ggplot2::labeller(day_nb = day_labs)) +


    ggplot2::scale_fill_manual(values = c("#bf812d",
                                          "#80cdc1"),
                               name = "Site") +

    ggplot2::scale_colour_manual(values = c("#bf812d",
                                            "#80cdc1"),
                                 name = "Site",
                                 labels = c("Poorly Protected",
                                            "Fully Protected")) +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +


    ggplot2::scale_x_discrete(labels= c("7:30", "", "", "", "8:40", "",
                                        "", "", "9:45", "", "", "",
                                        "10:55", "", "", "", "12:20", "",
                                        "", "", "13:40", "", "", "",
                                        "15:00", "", "", "", "16:30", "",
                                        "", "", "17:30")) +

    ggplot2::ylab("FIde PC4") +

    ggplot2::xlab("") +

    ggplot2::guides(fill = "none")


  # Assemble the 3 plots to return:

  plot_richness <- (plot_TD + plot_PD + plot_FRic) +
    patchwork::plot_layout(byrow = TRUE, heights = c(1, 1), widths = c(1, 1),
                           ncol = 3, nrow = 1, guides = "collect") +
    patchwork::plot_annotation(tag_levels = "A")

  plot_fd_ind <- (plot_FDis + plot_FSpe + plot_FDiv) +
    patchwork::plot_layout(byrow = TRUE, heights = c(1, 1), widths = c(1, 1),
                           ncol = 3, nrow = 1, guides = "collect") +
    patchwork::plot_annotation(tag_levels = "A")

  plot_fide <- (plot_FIde + plot_FIde2 + plot_FIde3 + plot_FIde4) +
    patchwork::plot_layout(byrow = TRUE, heights = c(1, 1), widths = c(1, 1),
                           ncol = 2, nrow = 2, guides = "collect") +
    patchwork::plot_annotation(tag_levels = "A")



  # save:
  ggplot2::ggsave(filename = here::here("outputs", "interday_richness.pdf"),
                  plot = plot_richness,
                  device = "pdf",
                  scale = 1,
                  height = 6000,
                  width = 14000,
                  units = "px",
                  dpi = 800)
  ggplot2::ggsave(filename = here::here("outputs", "interday_fd_ind.pdf"),
                  plot = plot_fd_ind,
                  device = "pdf",
                  scale = 1,
                  height = 6000,
                  width = 17000,
                  units = "px",
                  dpi = 800)
  ggplot2::ggsave(filename = here::here("outputs", "interday_fide.pdf"),
                  plot = plot_fide,
                  device = "pdf",
                  scale = 1,
                  height = 6000,
                  width = 17000,
                  units = "px",
                  dpi = 800)



  return(list(plot_richness, plot_fd_ind, plot_fide))


}




#####


plot.delta.alpha.inter.day.accum <- function(TD_accum_df,
                                             PD_accum_df,
                                             FD_accum_df,
                                             facets_colors,
                                             linewidth) {

  # create a dataframe with info: site, day, video_nb, fric, td, pd:
  complete_plot_df <- TD_accum_df[, c(ncol(TD_accum_df) - 1,
                                      ncol(TD_accum_df) - 3,
                                      ncol(TD_accum_df) - 5,
                                      ncol(TD_accum_df) - 6,
                                      ncol(TD_accum_df))]
  colnames(complete_plot_df)[ncol(complete_plot_df)] <- "TD"

  # add FD and PD information:
  complete_plot_df$FRic <- FD_accum_df$fric

  complete_plot_df$PD <- PD_accum_df$accum_PD


  # compute delta NG - B:
  complete_plot_NG_df <- complete_plot_df[which(complete_plot_df$site == "N'Gouja"), ]
  complete_plot_B_df <- complete_plot_df[which(complete_plot_df$site == "Boueni"), ]
  complete_plot_delta_df <- complete_plot_NG_df
  complete_plot_delta_df$site <- rep("Delta", nrow(complete_plot_delta_df))

  complete_plot_delta_df$TD <- complete_plot_NG_df$TD - complete_plot_B_df$TD
  colnames(complete_plot_delta_df)[5] <- "delta_TD"

  complete_plot_delta_df$FRic <- complete_plot_NG_df$FRic - complete_plot_B_df$FRic
  colnames(complete_plot_delta_df)[6] <- "delta_FRic"

  complete_plot_delta_df$PD <- complete_plot_NG_df$PD - complete_plot_B_df$PD
  colnames(complete_plot_delta_df)[7] <- "delta_PD"


  # now merge TD, FD and PD columns -> metric:
  final_plot_df <- reshape2::melt(complete_plot_delta_df,
                                  id.vars = c("day_nb", "site", "day", "video_nb"),
                                  variable.name = 'metric', value.name = 'values')

  # right class:
  final_plot_df$site <- as.factor(final_plot_df$site)
  final_plot_df$day <- as.factor(final_plot_df$day)
  final_plot_df$day <- as.factor(final_plot_df$day_nb)
  final_plot_df$video_nb <- ordered(final_plot_df$video_nb, levels = paste0(rep("video_", 33),
                                                                            c(1:33)))
  final_plot_df$metric <- as.factor(final_plot_df$metric)
  final_plot_df$metric <- ordered(final_plot_df$metric, levels = c("delta_TD",
                                                                   "delta_PD",
                                                                   "delta_FRic"))



  # Create new names from facets day_1, day_2 and day_3:
  day_labs <- c("Sampling day 1", "Sampling day 2", "Sampling day 3")
  names(day_labs) <- c("day_1", "day_2", "day_3")

  # plot :
  plot_delta <- ggplot2::ggplot(data = final_plot_df) +

    ggplot2::geom_line(ggplot2::aes(y = values, x = video_nb, group = metric,
                                    color = metric),
                       size = 0.9) +

    ggplot2::facet_grid(. ~ day_nb,
                        labeller = ggplot2::labeller(day_nb = day_labs)) +


    ggplot2::scale_colour_manual(values = facets_colors,
                                 name = "Delta Metrics",
                                 labels = c("Species richness", "Faith's PD",
                                            "FRic")) +

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

    ggplot2::ylab("Delta (Fully Protected - Poorly Protected)") +

    ggplot2::xlab("") +

    ggplot2::guides(fill = "none") +

    ggplot2:: geom_hline(yintercept = 0, linetype = "dashed", color = "black")


  # save:
  ggplot2::ggsave(filename = here::here("outputs", "delta_interday_richness.pdf"),
                  plot = plot_delta,
                  device = "pdf",
                  scale = 0.8,
                  height = 10000,
                  width = 14000,
                  units = "px",
                  dpi = 800)

  return(plot_delta)

}




#' Title
#'
#' @param TD_accum_df
#' @param FD_accum_df
#' @param PD_accum_df
#'
#' @return
#' @export
#'
#' @examples


compute.beta.accum <- function(TD_accum_df, PD_accum_df, sp_faxes_coord) {


  # compute the new dataframe on which beta values will be saved:
  beta_inter_accum_df <- as.data.frame(matrix(ncol = 8, nrow = 1, NA))
  colnames(beta_inter_accum_df) <- c("day_nb", "vid_nb", "beta_TD",
                                     "beta_PD", "beta_FD", "turn_TD",
                                     "turn_PD", "turn_FD")


  # compute phylo:
  sp_nm_all <- colnames(PD_accum_df[, c(1:(ncol(PD_accum_df) - 7))])
  phylo <- fishtree::fishtree_phylogeny(species = sp_nm_all)


  # loop on day_nb:
  for (i in (1:length(unique(TD_accum_df$day_nb)))) {

    day_studied <- as.character(unique(TD_accum_df$day_nb)[i])

    print(day_studied)

    # filter the dataframes so only keep the day_nb studied:
    TD_accum_df_day <- dplyr::filter(TD_accum_df, day_nb == day_studied)
    PD_accum_df_day <- dplyr::filter(PD_accum_df, day_nb == day_studied)


    # loop on the video_nb:
    for (j in (1:length(unique(TD_accum_df_day$video_nb)))) {

      video_studied <- as.character(unique(TD_accum_df_day$video_nb)[j])

      print(video_studied)

      # filter the dataframes so only keep the video studied (two rows):
      TD_accum_df_day_vid <- dplyr::filter(TD_accum_df_day, video_nb == video_studied)
      PD_accum_df_day_vid <- dplyr::filter(PD_accum_df_day, video_nb == video_studied)

      # compute beta TD:
      beta_TD <- betapart::beta.pair(TD_accum_df_day_vid[, c(1:(ncol(TD_accum_df_day_vid) - 7))],
                                     index.family = "jaccard")

      # compute beta PD (before put numerical format):
      PD_accum_df_day_vid <- apply(PD_accum_df_day_vid[, c(1:(ncol(PD_accum_df_day_vid) - 7))],
                                   2, as.numeric)
      beta_PD <- betapart::phylo.beta.pair(PD_accum_df_day_vid[, c(1:(ncol(PD_accum_df_day_vid) - 7))],
                                           phylo, index.family = "jaccard")

      # compute beta FD (before remove species which are absent from the 2 rows):
      FD_accum_df_day_vid <- TD_accum_df_day_vid[, c(1:(ncol(TD_accum_df_day_vid) - 7))]
      FD_accum_df_day_vid[3, ] <- apply(FD_accum_df_day_vid, 2, sum)
      FD_accum_df_day_vid <- FD_accum_df_day_vid[c(1,2),
                                                 which(FD_accum_df_day_vid[3, ] != 0)]
      rownames(FD_accum_df_day_vid) <- TD_accum_df_day_vid$video_id

      beta_FD <- mFD::beta.fd.multidim(
        sp_faxes_coord   = as.matrix(sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4", "PC5")]),
        asb_sp_occ       = FD_accum_df_day_vid,
        check_input      = TRUE,
        beta_family      = c("Jaccard"),
        details_returned = TRUE)


      # fill the df:
      beta_tot_TD <- beta_TD$beta.jac
      turn_TD <- beta_TD$beta.jtu
      beta_tot_PD <- beta_PD$phylo.beta.jac
      turn_PD <- beta_PD$phylo.beta.jtu
      beta_tot_FD <- beta_FD$pairasb_fbd_indices$jac_diss
      turn_FD <- beta_FD$pairasb_fbd_indices$jac_turn

      beta_inter_accum_df <- dplyr::add_row(beta_inter_accum_df,
                                            day_nb = day_studied,
                                            vid_nb = video_studied,
                                            beta_TD = beta_tot_TD[1],
                                            beta_PD = beta_tot_PD[1],
                                            beta_FD = beta_tot_FD[1],
                                            turn_TD = turn_TD[1],
                                            turn_PD = turn_PD[1],
                                            turn_FD = turn_FD[1])


    } # end loop on video_nb

  } # end loop on day_nb

  # remove first empty row:
  beta_inter_accum_df <- beta_inter_accum_df[-1, ]
  return(beta_inter_accum_df)

}



#' Title
#'
#' @param beta_accum
#' @param facets_colors
#' @param linewidth
#'
#' @return
#' @export
#'
#' @examples


plot.beta.inter.accum <- function(beta_accum, facets_colors, linewidth){


  # Merge TD, FD and PD columns for total beta:
  beta_plot_df <- reshape2::melt(beta_accum[, -c(6:9)],
                                  id.vars = c("day_nb", "vid_nb"),
                                  variable.name = 'metric', value.name = 'values')

  # Merge TD, FD and PD columns for turnover beta:
  turn_plot_df <- reshape2::melt(beta_accum[, -c(3:5)],
                                 id.vars = c("day_nb", "vid_nb"),
                                 variable.name = 'metric', value.name = 'values')

  # Right classes for total beta:
  beta_plot_df$day_nb <- as.factor(beta_plot_df$day_nb)
  beta_plot_df$vid_nb <- ordered(beta_plot_df$vid_nb, levels = paste0(rep("video_", 33),
                                                                            c(1:33)))
  beta_plot_df$metric <- as.factor(beta_plot_df$metric)

  # Right classes for turn beta:
  turn_plot_df$day_nb <- as.factor(turn_plot_df$day_nb)
  turn_plot_df$vid_nb <- ordered(turn_plot_df$vid_nb, levels = paste0(rep("video_", 33),
                                                                      c(1:33)))
  turn_plot_df$metric <- as.factor(turn_plot_df$metric)



  # Create new names from facets day_1, day_2 and day_3:
  day_labs <- c("Sampling day 1", "Sampling day 2", "Sampling day 3")
  names(day_labs) <- c("day_1", "day_2", "day_3")

  # plot :
  plot_beta_tot <- ggplot2::ggplot(data = beta_plot_df) +

    ggplot2::geom_line(ggplot2::aes(y = values, x = vid_nb, group = metric,
                                    color = metric),
                       size = 0.9) +

    ggplot2::facet_grid(. ~ day_nb,
                        labeller = ggplot2::labeller(day_nb = day_labs)) +


    ggplot2::scale_colour_manual(values = facets_colors,
                                 name = "Facets",
                                 labels = c("TD Dissimilarity", "PD Dissimilarity",
                                            "FD Dissimilarity")) +

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

    ggplot2::ylim(0, max(beta_plot_df$values)) +

    ggplot2::ylab("Dissimilarity") +

    ggplot2::xlab("") +

    ggplot2::guides(fill = "none")

  # save:
  ggplot2::ggsave(filename = here::here("outputs", "beta_tot_interday_variation.pdf"),
                  plot = plot_beta_tot,
                  device = "pdf",
                  scale = 0.8,
                  height = 10000,
                  width = 14000,
                  units = "px",
                  dpi = 800)



  plot_turn <- ggplot2::ggplot(data = turn_plot_df) +

    ggplot2::geom_line(ggplot2::aes(y = values, x = vid_nb, group = metric,
                                    color = metric),
                       size = 0.9) +

    ggplot2::facet_grid(. ~ day_nb,
                        labeller = ggplot2::labeller(day_nb = day_labs)) +


    ggplot2::scale_colour_manual(values = facets_colors,
                                 name = "Facets",
                                 labels = c("Turnover TD", "Turnover PD",
                                            "Turnover FD")) +

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

    ggplot2::ylim(0, max(beta_plot_df$values)) +

    ggplot2::ylab("Turnover") +

    ggplot2::xlab("") +

    ggplot2::guides(fill = "none")


  # save:
  ggplot2::ggsave(filename = here::here("outputs", "turnover_interday_variation.pdf"),
                  plot = plot_turn,
                  device = "pdf",
                  scale = 0.8,
                  height = 10000,
                  width = 14000,
                  units = "px",
                  dpi = 800)


  return(list(plot_beta_tot, plot_turn))

}
