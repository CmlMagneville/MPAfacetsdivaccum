###############################################################################
##
## Script to plot intra day accumulation for the three facets
##
## 6_Plot_intra_day_variation.R
##
## 11/08/2022
##
##
##
###############################################################################





plot.intra.day.accum <- function(TD_accum_df,
                                 PD_accum_df,
                                 FD_accum_df,
                                 hline_df,
                                 facets_colors,
                                 linewidth) {


  # create a dataframe with info: site, day, video_nb, fric, td, pd:
  complete_plot_df <- TD_accum_df[, c(ncol(TD_accum_df) - 5,
                                      ncol(TD_accum_df) - 7,
                                      ncol(TD_accum_df) - 8,
                                      ncol(TD_accum_df))]
  colnames(complete_plot_df)[ncol(complete_plot_df)] <- "TD"

  # add FD and PD information:
  complete_plot_df$PD <- PD_accum_df$accum_PD
  complete_plot_df$FD <- FD_accum_df$fric


  # now merge TD, FD and PD columns -> metric:
  final_plot_df <- reshape2::melt(complete_plot_df,
                                  id.vars = c("site", "day", "video_nb"),
                                  variable.name = 'metric', value.name = 'values')

  # rename TD, PD and FD to have richn names:
  final_plot_df$metric <- as.character(final_plot_df$metric)
  final_plot_df$metric[which(final_plot_df$metric == "TD")] <- "Species richness"
  final_plot_df$metric[which(final_plot_df$metric == "PD")] <- "Faith's PD"
  final_plot_df$metric[which(final_plot_df$metric == "FD")] <- "FRic"
  final_plot_df$metric <- as.factor(final_plot_df$metric)
  final_plot_df$metric <- ordered(final_plot_df$metric,
                                  levels = c("Species richness",
                                             "Faith's PD",
                                             "FRic"))



  # right class:
  final_plot_df$site <- as.character(final_plot_df$site)
  final_plot_df$site[which(final_plot_df$site == "N'Gouja")] <- "Fully Protected"
  final_plot_df$site[which(final_plot_df$site == "Boueni")] <- "Poorly Protected"
  final_plot_df$site <- as.factor(final_plot_df$site)
  final_plot_df$site <- ordered(final_plot_df$site,
                                  levels = c("Fully Protected",
                                             "Poorly Protected"))
  final_plot_df$day <- as.factor(final_plot_df$day)
  final_plot_df$video_nb <- ordered(final_plot_df$video_nb, levels = paste0(rep("video_", 33),
                                                                                 c(1:33)))

  # build labels for sites:
  sites_labs <- c("Fully Protected", "Poorly Protected")
  names(sites_labs) <- c("N'Gouja", "Boueni")


  # plot:
  accum_day_plot <- ggplot2::ggplot(final_plot_df) +

    ggplot2::geom_line(ggplot2::aes(x = video_nb, y = values, group = day,
                                    color = metric,
                                    linetype = day),
                       size = 0.9) +

    ggplot2::geom_hline(data = hline_df, ggplot2::aes(yintercept = hline_value),
                        color = "grey70",
                        size = linewidth) +

    ggplot2::facet_grid(forcats::fct_relevel(metric,
                                            c("Species richness",
                                              "Faith's PD",
                                              "FRic")) ~ forcats::fct_relevel(site,
                                                                              c("Fully Protected",
                                                                                "Poorly Protected"))) +

    ggplot2::scale_colour_manual(values = facets_colors,
                                 name = "Metric") +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90"),
                   strip.text.y = ggplot2::element_text(size = 8,
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 8,
                                                        face = "bold"),
                   panel.spacing = ggplot2::unit(2, "lines")) +

    ggplot2::scale_x_discrete(labels = c("7:30", "", "8:00", "", "8:40", "",
                                         "9:15", "", "9:45", "", "10:20", "",
                                         "10:55", "", "11:40", "", "12:20", "",
                                         "12:55", "", "13:40", "", "14:25", "",
                                         "15:00", "", "15:45", "", "16:30", "",
                                         "17:00", "", "17:30")) +

    ggplot2::guides(fill = "none", linetype = "none", color = "none") +

    ggplot2::xlab("") +

    ggplot2::ylab("Percentage of diversity")

  # save:
  ggplot2::ggsave(filename = here::here("outputs", "facets_accum_day_plot.pdf"),
                  plot = accum_day_plot,
                  device = "pdf",
                  scale = 1,
                  height = 6000,
                  width = 10000,
                  units = "px",
                  dpi = 600)



  return(accum_day_plot)

}



