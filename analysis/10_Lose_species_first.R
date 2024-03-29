###############################################################################
##
## Script to compute and plot the evolution of FD and PD when loosing ...
## ... rarest/most common/random species first
##
## 10_Lose_species_first.R
##
## 23/08/2022
##
##
##
###############################################################################



# Step 1: Call data ####


rarcom_df <- readRDS(here::here("transformed_data/rarcom_df.rds"))
sp_faxes_coord <- readRDS(here::here("transformed_data/sp_faxes_coord.rds"))
day_asb_sp_df <- readRDS(here::here("transformed_data", "asb_sp_site_day.rds"))


# create one subset for N'Gouja:
rarcom_df_NG <- rarcom_df[which(rarcom_df$site == "N'Gouja"), ]

# create one subset for Boueni:
rarcom_df_B <- rarcom_df[which(rarcom_df$site == "Boueni"), ]


# Step 2: Compute N'Gouja species lost species plot ####



# set colours:
site_color <- "#80cdc1"

# other arguments:
rarcom_df_site <- rarcom_df_NG
asb_sp_site <- day_asb_sp_df
site <- "NGouja"

plot_lose_sp_NG <- lose.species.div.plot(rarcom_df_site,
                                         sp_faxes_coord,
                                         asb_sp_site,
                                         site_color,
                                         site)
plot_lose_sp_NG_df <- plot_lose_sp_NG[[1]]
saveRDS(plot_lose_sp_NG, here::here("transformed_data", "plot_lose_sp_NG.rds"))

# Step 3: Compute Boueni species lost species plot ####


# set colours:
site_color <- "#bf812d"

# other arguments:
rarcom_df_site <- rarcom_df_B
asb_sp_site <- day_asb_sp_df
site <- "Boueni"

plot_lose_sp_B <- lose.species.div.plot(rarcom_df_site,
                                         sp_faxes_coord,
                                         asb_sp_site,
                                         site_color,
                                         site)
plot_lose_sp_B_df <- plot_lose_sp_B[[1]]
saveRDS(plot_lose_sp_B, here::here("transformed_data", "plot_lose_sp_B.rds"))


# Step 3: Combine the two plots with patchwork ####


# FRic and PD

plot_lose_sp_NG_FP <- plot_lose_sp_NG[[2]]

plot_lose_sp_B_FP <- plot_lose_sp_B[[2]]

plot_lose_sp_FRic_PD <- (plot_lose_sp_NG_FP + plot_lose_sp_B_FP) +
  patchwork::plot_layout(byrow = TRUE, heights = c(1, 1), widths = c(1, 1),
                         ncol = 2, nrow = 1, guides = "collect") +
  patchwork::plot_annotation(tag_levels = "A")


# FSpe and FDis:

plot_lose_sp_NG_FF <- plot_lose_sp_NG[[3]]

plot_lose_sp_B_FF <- plot_lose_sp_B[[3]]

plot_lose_sp_FSpe_FDis <- (plot_lose_sp_NG_FF + plot_lose_sp_B_FF) +
  patchwork::plot_layout(byrow = TRUE, heights = c(1, 1), widths = c(1, 1),
                         ncol = 2, nrow = 1, guides = "collect")


# save FRIC PD plot:
ggplot2::ggsave(filename = here::here("outputs", "Sp_loss_PD_FRic.pdf"),
                plot = plot_lose_sp_FRic_PD,
                device = "pdf",
                scale = 1,
                height = 8000,
                width = 10000,
                units = "px",
                dpi = 800)

# save FSpe FDis plot:
ggplot2::ggsave(filename = here::here("outputs", "Sp_loss_FDis_FSpe.pdf"),
                plot = plot_lose_sp_FSpe_FDis,
                device = "pdf",
                scale = 1,
                height = 8000,
                width = 10000,
                units = "px",
                dpi = 800)



# Step 4: Compute interesting figures about FRic ####


# How much is the supplemental FRic loss when losing the 20% rarest species compared...
# ... to the 20% most common species and to the 20%random species (based on median)?


## get the FRic loss when losing the 20% rarest species:

## NGOUJA

NG_nb_sp <- length(unique(plot_lose_sp_NG_df$Species_loss)) - 1
NG_20 <- floor(0.2*NG_nb_sp)
fric_rare_NG <- plot_lose_sp_NG_df[which(plot_lose_sp_NG_df$ind == "FRic" & plot_lose_sp_NG_df$metric == "rares"), ]
fric_rare_NG <- fric_rare_NG[order(fric_rare_NG$Species_loss), ]
fric_rare_NG_loss <- ((fric_rare_NG[1, "values"] - fric_rare_NG[NG_20, "values"])/fric_rare_NG[1, "values"])*100

fric_common_NG <- plot_lose_sp_NG_df[which(plot_lose_sp_NG_df$ind == "FRic" & plot_lose_sp_NG_df$metric == "commons"), ]
fric_common_NG <- fric_common_NG[order(fric_common_NG$Species_loss), ]
fric_common_NG_loss <- ((fric_common_NG[1, "values"] - fric_common_NG[NG_20, "values"])/fric_common_NG[1, "values"])*100

fric_common_NG_loss - fric_rare_NG_loss

# Loosing 20% of the rarest species leads to ...
# ... a supplemental loss of 47.10% of FRic when compared to most common species loss

fric_random_NG <- plot_lose_sp_NG_df[which(plot_lose_sp_NG_df$ind == "FRic" & plot_lose_sp_NG_df$metric == "random"), ]
fric_random_NG <- fric_random_NG[order(fric_random_NG$Species_loss), ]
fric_random_NG_loss <- ((fric_random_NG[1, "values"] - fric_random_NG[NG_20, "values"])/fric_random_NG[1, "values"])*100

fric_random_NG_loss - fric_rare_NG_loss

# Loosing 20% of the rarest species leads to ...
# ... a supplemental loss of 30.77% of FRic when compared to random species loss


## BOUENI


B_nb_sp <- length(unique(plot_lose_sp_B_df$Species_loss)) - 1
B_20 <- floor(0.2*B_nb_sp)
fric_rare_B <- plot_lose_sp_B_df[which(plot_lose_sp_B_df$ind == "FRic" & plot_lose_sp_B_df$metric == "rares"), ]
fric_rare_B <- fric_rare_B[order(fric_rare_B$Species_loss), ]
fric_rare_B_loss <- ((fric_rare_B[1, "values"] - fric_rare_B[B_20, "values"])/fric_rare_B[1, "values"])*100

fric_common_B <- plot_lose_sp_B_df[which(plot_lose_sp_B_df$ind == "FRic" & plot_lose_sp_B_df$metric == "commons"), ]
fric_common_B <- fric_common_B[order(fric_common_B$Species_loss), ]
fric_common_B_loss <- ((fric_common_B[1, "values"] - fric_common_B[B_20, "values"])/fric_common_B[1, "values"])*100

fric_common_B_loss - fric_rare_B_loss

# Loosing 20% of the rarest species leads to ...
# ... a supplemental loss of 8.17% of FRic when compared to most common species loss

fric_random_B <- plot_lose_sp_B_df[which(plot_lose_sp_B_df$ind == "FRic" & plot_lose_sp_B_df$metric == "random"), ]
fric_random_B <- fric_random_B[order(fric_random_B$Species_loss), ]
fric_random_B_loss <- ((fric_random_B[1, "values"] - fric_random_B[B_20, "values"])/fric_random_B[1, "values"])*100

fric_random_B_loss - fric_rare_B_loss

# Loosing 20% of the rarest species leads to ...
# ... a supplemental loss of 1.73% of FRic when compared to random species loss


# PD: Compute the PD loss when loosing the first 20% species:

# N'Gouja:
NG_nb_sp <- length(unique(plot_lose_sp_NG_df$Species_loss)) - 1
NG_20 <- floor(0.2*NG_nb_sp)
PD_rare_NG <- plot_lose_sp_NG_df[which(plot_lose_sp_NG_df$ind == "Faith's PD" & plot_lose_sp_NG_df$metric == "rares"), ]
PD_rare_NG <- PD_rare_NG[order(PD_rare_NG$Species_loss), ]
PD_loss_NG <- ((PD_rare_NG[1, "values"] - PD_rare_NG[NG_20, "values"]))*100

PD_common_NG <- plot_lose_sp_NG_df[which(plot_lose_sp_NG_df$ind == "Faith's PD" & plot_lose_sp_NG_df$metric == "commons"), ]
PD_common_NG <- PD_common_NG[order(PD_common_NG$Species_loss), ]
PD_common_NG_loss <- ((PD_common_NG[1, "values"] - PD_common_NG[NG_20, "values"])/PD_common_NG[1, "values"])*100

PD_common_NG_loss - PD_loss_NG

PD_random_NG <- plot_lose_sp_NG_df[which(plot_lose_sp_NG_df$ind == "Faith's PD" & plot_lose_sp_NG_df$metric == "random"), ]
PD_random_NG <- PD_random_NG[order(PD_random_NG$Species_loss), ]
PD_random_NG_loss <- ((PD_random_NG[1, "values"] - PD_random_NG[NG_20, "values"])/PD_random_NG[1, "values"])*100

PD_random_NG_loss - PD_loss_NG

# Boueni:
B_nb_sp <- length(unique(plot_lose_sp_B_df$Species_loss)) - 1
B_20 <- floor(0.2*B_nb_sp)
PD_rare_B <- plot_lose_sp_B_df[which(plot_lose_sp_B_df$ind == "Faith's PD" & plot_lose_sp_B_df$metric == "rares"), ]
PD_rare_B <- PD_rare_B[order(PD_rare_B$Species_loss), ]
PD_loss_B <- (PD_rare_B[1, "values"] - PD_rare_B[B_20, "values"])*100


PD_common_B <- plot_lose_sp_B_df[which(plot_lose_sp_B_df$ind == "Faith's PD" & plot_lose_sp_B_df$metric == "commons"), ]
PD_common_B <- PD_common_B[order(PD_common_B$Species_loss), ]
PD_common_B_loss <- ((PD_common_B[1, "values"] - PD_common_B[B_20, "values"])/PD_common_B[1, "values"])*100

PD_common_B_loss - PD_loss_B

PD_random_B <- plot_lose_sp_B_df[which(plot_lose_sp_B_df$ind == "Faith's PD" & plot_lose_sp_B_df$metric == "random"), ]
PD_random_B <- PD_random_B[order(PD_random_B$Species_loss), ]
PD_random_B_loss <- ((PD_random_B[1, "values"] - PD_random_B[B_20, "values"])/PD_random_B[1, "values"])*100

PD_random_B_loss - PD_loss_B

