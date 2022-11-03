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


    ggplot2::scale_x_discrete(labels= c("7:30", "", "8:00", "", "8:40", "",
                                        "9:15", "", "9:45", "", "10:20", "",
                                        "10:55", "", "11:40", "", "12:20", "",
                                        "12:55", "", "13:40", "", "14:25", "",
                                        "15:00", "", "15:45", "", "16:30", "",
                                        "17:00", "", "17:30")) +

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


    ggplot2::scale_x_discrete(labels= c("7:30", "", "8:00", "", "8:40", "",
                                        "9:15", "", "9:45", "", "10:20", "",
                                        "10:55", "", "11:40", "", "12:20", "",
                                        "12:55", "", "13:40", "", "14:25", "",
                                        "15:00", "", "15:45", "", "16:30", "",
                                        "17:00", "", "17:30")) +

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


    ggplot2::scale_x_discrete(labels= c("7:30", "", "8:00", "", "8:40", "",
                                        "9:15", "", "9:45", "", "10:20", "",
                                        "10:55", "", "11:40", "", "12:20", "",
                                        "12:55", "", "13:40", "", "14:25", "",
                                        "15:00", "", "15:45", "", "16:30", "",
                                        "17:00", "", "17:30")) +

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


    ggplot2::scale_x_discrete(labels= c("7:30", "", "8:00", "", "8:40", "",
                                        "9:15", "", "9:45", "", "10:20", "",
                                        "10:55", "", "11:40", "", "12:20", "",
                                        "12:55", "", "13:40", "", "14:25", "",
                                        "15:00", "", "15:45", "", "16:30", "",
                                        "17:00", "", "17:30")) +

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


    ggplot2::scale_x_discrete(labels= c("7:30", "", "8:00", "", "8:40", "",
                                        "9:15", "", "9:45", "", "10:20", "",
                                        "10:55", "", "11:40", "", "12:20", "",
                                        "12:55", "", "13:40", "", "14:25", "",
                                        "15:00", "", "15:45", "", "16:30", "",
                                        "17:00", "", "17:30")) +

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


    ggplot2::scale_x_discrete(labels= c("7:30", "", "8:00", "", "8:40", "",
                                        "9:15", "", "9:45", "", "10:20", "",
                                        "10:55", "", "11:40", "", "12:20", "",
                                        "12:55", "", "13:40", "", "14:25", "",
                                        "15:00", "", "15:45", "", "16:30", "",
                                        "17:00", "", "17:30")) +

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


    ggplot2::scale_x_discrete(labels= c("7:30", "", "8:00", "", "8:40", "",
                                        "9:15", "", "9:45", "", "10:20", "",
                                        "10:55", "", "11:40", "", "12:20", "",
                                        "12:55", "", "13:40", "", "14:25", "",
                                        "15:00", "", "15:45", "", "16:30", "",
                                        "17:00", "", "17:30")) +

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


    ggplot2::scale_x_discrete(labels= c("7:30", "", "8:00", "", "8:40", "",
                                        "9:15", "", "9:45", "", "10:20", "",
                                        "10:55", "", "11:40", "", "12:20", "",
                                        "12:55", "", "13:40", "", "14:25", "",
                                        "15:00", "", "15:45", "", "16:30", "",
                                        "17:00", "", "17:30")) +

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


    ggplot2::scale_x_discrete(labels= c("7:30", "", "8:00", "", "8:40", "",
                                        "9:15", "", "9:45", "", "10:20", "",
                                        "10:55", "", "11:40", "", "12:20", "",
                                        "12:55", "", "13:40", "", "14:25", "",
                                        "15:00", "", "15:45", "", "16:30", "",
                                        "17:00", "", "17:30")) +

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


    ggplot2::scale_x_discrete(labels= c("7:30", "", "8:00", "", "8:40", "",
                                        "9:15", "", "9:45", "", "10:20", "",
                                        "10:55", "", "11:40", "", "12:20", "",
                                        "12:55", "", "13:40", "", "14:25", "",
                                        "15:00", "", "15:45", "", "16:30", "",
                                        "17:00", "", "17:30")) +

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
                                 labels = c("Species richness", "FRic", "Faith's PD")) +

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

    ggplot2::ylab("Delta (N'Gouja - Boueni)") +

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

