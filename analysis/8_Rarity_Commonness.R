###############################################################################
##
## Script to see rare/medium/common species in both sites and where they ...
## ... are in the functional space and the phylogenetic tree
##
## 8_Rarity_Commonness.R
##
## 12/08/2022
##
## Camille Magneville
##
###############################################################################


# Step 1: Call data ####


# load the basic_accum_df (created and saved in 6_Intra_days_accumul.R):
basic_accum_df <- readRDS(here::here("transformed_data", "basic_accumul_df.rds"))


# Step 2: Compute and plot the nb of videos in which each species occur #####


# Compute the dataframe with site, species_nm and vid_occ_nb and rarity info:
# > 50% common, 25-50 medium, < 25% rare:

rarcom_df <- rarcom.computation(basic_accum_df)
saveRDS(rarcom_df, here::here("transformed_data", "rarcom_df.rds"))


# Compute figures: ( rare = super_ rare, medium = rare, common = common):

# Percentage of super rare/rare/common species per site:
perc_rare_sp_NG <- (nrow(rarcom_df[which(rarcom_df$site == "N'Gouja" & rarcom_df$rarity == "super rare"), ])/
  nrow(rarcom_df[which(rarcom_df$site == "N'Gouja"), ]))*100
perc_common_sp_NG <- (nrow(rarcom_df[which(rarcom_df$site == "N'Gouja" & rarcom_df$rarity == "common"), ])/
                      nrow(rarcom_df[which(rarcom_df$site == "N'Gouja"), ]))*100
perc_med_sp_NG <- (nrow(rarcom_df[which(rarcom_df$site == "N'Gouja" & rarcom_df$rarity == "rare"), ])/
                        nrow(rarcom_df[which(rarcom_df$site == "N'Gouja"), ]))*100

perc_rare_sp_B <- (nrow(rarcom_df[which(rarcom_df$site == "Boueni" & rarcom_df$rarity == "super rare"), ])/
                      nrow(rarcom_df[which(rarcom_df$site == "Boueni"), ]))*100
perc_common_sp_B <- (nrow(rarcom_df[which(rarcom_df$site == "Boueni" & rarcom_df$rarity == "common"), ])/
                        nrow(rarcom_df[which(rarcom_df$site == "Boueni"), ]))*100
perc_med_sp_B <- (nrow(rarcom_df[which(rarcom_df$site == "Boueni" & rarcom_df$rarity == "rare"), ])/
                     nrow(rarcom_df[which(rarcom_df$site == "Boueni"), ]))*100

perc_unique_sp_NG <- (nrow(rarcom_df[which(rarcom_df$site == "N'Gouja" & rarcom_df$site_presence == "N'Gouja only"), ])/
                        nrow(rarcom_df[which(rarcom_df$site == "N'Gouja"), ]))*100

perc_unique_sp_B <- (nrow(rarcom_df[which(rarcom_df$site == "Boueni" & rarcom_df$site_presence == "Boueni only"), ])/
                        nrow(rarcom_df[which(rarcom_df$site == "Boueni"), ]))*100

# Number of common species present in both sites (ie > 25 % of both sites):
comm <- rarcom_df[which(rarcom_df$site_presence == "both" & rarcom_df$perc_vid_occ >= 25), ]
# only count species which are in double:
nb_comm_bothsites <- nrow(comm) - length(unique(comm$species_nm))
# compute proportion:
(nb_comm_bothsites / length(unique(rarcom_df$species_nm)))*100

# Number of common species present in both sites (ie > 75 % of both sites):
rare <- rarcom_df[which(rarcom_df$site_presence == "both" & rarcom_df$perc_vid_occ >= 75), ]
# only count species which are in double:
nb_rare_bothsites <- nrow(rare) - length(unique(rare$species_nm))
# compute proportion:
(nb_rare_bothsites / length(unique(rarcom_df$species_nm)))*100

# Plot (and save) for each site species occurrence:
sites_colors <- c("grey85", "#bf812d", "#80cdc1")

plot.rarcom(rarcom_df, sites_colors)



# Step 3: Where are the rare/medium/common species in the functional sp for each site? ####


# Call data:

basic_accum_df <- readRDS(here::here("transformed_data", "basic_accumul_df.rds"))

sp_tr <- readRDS(here::here("transformed_data/sp_tr_final.rds"))
tr_cat <- readRDS(here::here("transformed_data/tr_cat_df.rds"))

site_asb_df <- readRDS(here::here("transformed_data/site_asb_df.rds"))


# argument:
rarity_shapes <- c(24, 22, 21)
rarity_colors_NG <- c("#00441b", "#238b45", "#66c2a4")
rarity_colors_B <- c("#543005", "#8c510a", "#bf812d")
sites_colors <- c("#bf812d", "#80cdc1")

# Plot:
spot.rare.sp.fd(basic_accum_df,
                            site_asb_fe_df = site_asb_df,
                            sp_tr,
                            tr_cat,
                            rarcom_df,
                            rarity_shapes,
                            rarity_colors_B,
                            rarity_colors_NG,
                            sites_colors)


# Step 4: Where are the rare/medium species in the phylogenetic tree of each site? ####


# Call data:
rar_com_df <- readRDS(here::here("transformed_data", "rarcom_df.rds"))


# Rename species with their sister species for some so can be found ...
# ... in the phylogenetic tree:

rar_com_df[which(rar_com_df$species_nm == "Gomphosus_caeruleus"), "species_nm"] <- "Gomphosus_varius"
# rar_com_df[which(rar_com_df$species_nm == "Scolopsis_frenata"), "species_nm"] <- "Scolopsis_bilineata"
rar_com_df[which(rar_com_df$species_nm == "Scolopsis_ghanam"), "species_nm"] <- "Scolopsis_bimaculata"
rar_com_df[which(rar_com_df$species_nm == "Cetoscarus_ocellatus"), "species_nm"] <- "Cetoscarus_bicolor"
rar_com_df[which(rar_com_df$species_nm == "Scarus_falcipinnis"), "species_nm"] <- "Scarus_altipinnis"
rar_com_df[which(rar_com_df$species_nm == "Scarus_scaber"), "species_nm"] <- "Scarus_oviceps"
rar_com_df[which(rar_com_df$species_nm == "Tylosurus_crocodilus"), "species_nm"] <- "Tylosurus_crocodilus_crocodilus"
# rar_com_df[which(rar_com_df$species_nm == "Chlorurus_strongylocephalus"), "species_nm"] <- "Chlorurus_microrhinos"
# rar_com_df[which(rar_com_df$species_nm == "Canthigaster_cyanospilota"), "species_nm"] <- "Canthigaster_coronata"
rar_com_df[which(rar_com_df$species_nm == "Labropsis_xanthonota"), "species_nm"] <- "Labropsis_australis"
rar_com_df[which(rar_com_df$species_nm == "Ac_Cten_dark"), "species_nm"] <- "Ctenochaetus_striatus"


# plot N'Gouja phylogeny in colors according to species rarity on the global phylogeny:

# get global phylo:
sp_nm_all <- unique(rar_com_df$species_nm)
phylo <- fishtree::fishtree_phylogeny(species = sp_nm_all)
# convert into a dataframe:
phylo_df <- tidytree::as_tibble(phylo)

# create the phylo df for N'Gouja:
phylo_df_NG <- phylo_df
# add a column that will contain info about rarity (none if not in NG = black, ...
# ... rare/medium/common otherwise)
phylo_df_NG$rarity <- rep("none", nrow(phylo_df_NG))


# loop on each tip labels:
for (i in phylo_df_NG$label) {

  # if the species is seen in N'Gouja, add its rarity:
  if (i %in% rar_com_df[which(rar_com_df$site  == "N'Gouja"), "species_nm"]) {

    phylo_df_NG[which(phylo_df_NG$label == i), "rarity"] <- rar_com_df[which(
                                                              rar_com_df$species_nm == i &
                                                              rar_com_df$site == "N'Gouja"),
                                                              "rarity"]
  }

}


# now plot it:
NG_rar_tree <- ggtree::ggtree(ape::as.phylo(phylo_df_NG), ggplot2::aes(color = phylo_df_NG$rarity),
               size = 1, layout = "circular") +

  ggplot2::scale_color_manual(values =  c("#66c2a4",  "#238b45", "grey80", "#00441b"),
                               name = "Rarity") +

  ggtree::geom_tiplab(hjust = -.1, size = 3)

# save it:
ggplot2::ggsave(filename = here::here("outputs", "NG_rarity_tree.pdf"),
                plot = NG_rar_tree,
                device = "pdf",
                scale = 1.4,
                height = 6000,
                width = 10000,
                units = "px",
                dpi = 600)


# create the phylo df for Boueni:
phylo_df_B <- phylo_df
# add a column that will contain info about rarity (none if not in NG = black, ...
# ... rare/medium/common otherwise)
phylo_df_B$rarity <- rep("none", nrow(phylo_df_B))


# loop on each tip labels:
for (i in phylo_df_B$label) {

  # if the species is seen in Boueni, add its rarity:
  if (i %in% rar_com_df[which(rar_com_df$site  == "Boueni"), "species_nm"]) {

    phylo_df_B[which(phylo_df_B$label == i), "rarity"] <- rar_com_df[which(
      rar_com_df$species_nm == i &
        rar_com_df$site == "Boueni"),
      "rarity"]
  }

}


# now plot it:
B_rar_tree <- ggtree::ggtree(ape::as.phylo(phylo_df_B), ggplot2::aes(color = phylo_df_B$rarity),
                              size = 1, layout = "circular") +

  ggplot2::scale_color_manual(values =  c("#bf812d", "#8c510a", "grey80", "#543005"),
                              name = "Rarity") +

  ggtree::geom_tiplab(hjust = -.1, size = 3)


# save it:
ggplot2::ggsave(filename = here::here("outputs", "B_rarity_tree.pdf"),
                plot = B_rar_tree,
                device = "pdf",
                scale = 1.4,
                height = 6000,
                width = 10000,
                units = "px",
                dpi = 600)



# Then  new plot with no colors on the tree but points on the tips:

## create a matrix that will served as a heatmap showing on each tip...
# .. the rarity on both sites:

phylo_df <- as.data.frame(phylo_df)
phylo_df_NG <- as.data.frame(phylo_df_NG)
phylo_df_B <- as.data.frame(phylo_df_B)

heatmap_df <- phylo_df_NG[which(! is.na(phylo_df_NG$label)), c(4, 5)]
colnames(heatmap_df)[2] <- "NGouja"
heatmap_df$Boueni <- rep(0, nrow(heatmap_df))

for (i in heatmap_df$label) {

  heatmap_df[which(heatmap_df$label == i), "Boueni"] <- phylo_df_B[which(phylo_df_B$label == i), "rarity"]

}

# format the column class:
heatmap_df$NGouja <- as.factor(heatmap_df$NGouja)
heatmap_df$Boueni <- as.factor(heatmap_df$Boueni)


# compute the basic tree:
phylo_df <- tidytree::as_tibble(phylo)
rar_tree <- ggtree::ggtree(ape::as.phylo(phylo_df),
                              size = 1, layout = "circular",
                           right = TRUE, ladderize = FALSE) +

  ggtree::geom_tiplab(ggplot2::aes(label = paste0(substr(phylo_df$label, 1, 1), sep = ". ", gsub(".*_","",phylo_df$label))),
    hjust = -.1, size = 3, offset = 10)

  # theme(plot.margin = unit(rep(-2,  4), "cm"))


# get basic tree information:
tree_dt <- as.data.frame(rar_tree$data)

# get only the tips:
tree_dt <- tree_dt[tree_dt$"isTip" == TRUE, ]

tree_dt <- tree_dt[order(tree_dt$"y"), ]

## Change x coordinates so can add points and chose color palette:
tree_dt <- dplyr::left_join(tree_dt, heatmap_df)
tree_dt$"x" <-  max(tree_dt$"x") + 5
pal <- harrypotter::hp(n = 5, house = "Ravenclaw")

# Add NGouja points:
pts1 <- rar_tree +

  ggplot2::geom_point(data = tree_dt,
                      ggplot2::aes(x = x, y = y, color = NGouja),
             shape = 20, size = 4, alpha = 0.8) +

  ggplot2::scale_color_manual(values =  c(pal[4], "grey95", pal[3],  pal[1]),
                              name = "Rarity")

# Add Boueni points:
pts2 <- pts1 +

  ggplot2::geom_point(data = tree_dt,
                      ggplot2::aes(x = x + 5, y = y, color = Boueni),
                      shape = 17, size = 3, alpha = 0.8)

# save it:
ggplot2::ggsave(filename = here::here("outputs", "global_rarity_tree.pdf"),
                plot = pts2,
                device = "pdf",
                scale = 1.2,
                height = 6000,
                width = 10000,
                units = "px",
                dpi = 600)



# Step 5 - D phylogenetic index  ####

# Create the global phylo:
rarcom_df <- readRDS(here::here("transformed_data", "rarcom_df.rds"))

# Rename species with their sister species for some so can be found ...
# ... in the phylogenetic tree:

rarcom_df[which(rarcom_df$species_nm == "Gomphosus_caeruleus"), "species_nm"] <- "Gomphosus_varius"
# rarcom_df[which(rarcom_df$species_nm == "Scolopsis_frenata"), "species_nm"] <- "Scolopsis_bilineata"
rarcom_df[which(rarcom_df$species_nm == "Scolopsis_ghanam"), "species_nm"] <- "Scolopsis_bimaculata"
rarcom_df[which(rarcom_df$species_nm == "Cetoscarus_ocellatus"), "species_nm"] <- "Cetoscarus_bicolor"
rarcom_df[which(rarcom_df$species_nm == "Scarus_falcipinnis"), "species_nm"] <- "Scarus_altipinnis"
rarcom_df[which(rarcom_df$species_nm == "Scarus_scaber"), "species_nm"] <- "Scarus_oviceps"
rarcom_df[which(rarcom_df$species_nm == "Tylosurus_crocodilus"), "species_nm"] <- "Tylosurus_crocodilus_crocodilus"
# rarcom_df[which(rarcom_df$species_nm == "Chlorurus_strongylocephalus"), "species_nm"] <- "Chlorurus_microrhinos"
# rarcom_df[which(rarcom_df$species_nm == "Canthigaster_cyanospilota"), "species_nm"] <- "Canthigaster_coronata"
rarcom_df[which(rarcom_df$species_nm == "Labropsis_xanthonota"), "species_nm"] <- "Labropsis_australis"
rarcom_df[which(rarcom_df$species_nm == "Ac_Cten_dark"), "species_nm"] <- "Ctenochaetus_striatus"

# get global phylo:
sp_nm_all <- unique(rar_com_df$species_nm)
phylo <- fishtree::fishtree_phylogeny(species = sp_nm_all)

# Compute the phylogenetic D to see if super rare and rare species have a ...
# ... phylogenetic random distrib or if they are clumped in the phylogeny:

## WITH THE WHOLE PHYLOGENETIC TREE (NG + B)
# Boueni: rare and super rare in the same group
rarcom_df_B <- rarcom_df[which(rarcom_df$site == "Boueni"), ]
rarcom_df_B$rarity[which(rarcom_df_B$rarity %in% c("rare", "super rare"))] <- "rare"
caper::phylo.d(data = rarcom_df_B[, c("species_nm", "rarity")],
               phy = phylo,
               names.col = species_nm,
               binvar = rarity)
# D = 0.77
# closely related species were not necessarily more similar in their
# degree of ecological rarity than distantly related species

# NGouja: rare and super rare in the same group
rarcom_df_NG <- rarcom_df[which(rarcom_df$site == "N'Gouja"), ]
rarcom_df_NG$rarity[which(rarcom_df_NG$rarity %in% c("rare", "super rare"))] <- "rare"
caper::phylo.d(data = rarcom_df_NG[, c("species_nm", "rarity")],
               phy = phylo,
               names.col = species_nm,
               binvar = rarity)
# D = 0.84
# closely related species were not necessarily more similar in their
# degree of ecological rarity than distantly related species


# Boueni: rare and common in the same group
rarcom_df_B <- rarcom_df[which(rarcom_df$site == "Boueni"), ]
rarcom_df_B$rarity[which(rarcom_df_B$rarity %in% c("rare", "common"))] <- "common"
caper::phylo.d(data = rarcom_df_B[, c("species_nm", "rarity")],
               phy = phylo,
               names.col = species_nm,
               binvar = rarity)
# D = 1.018
# closely related species were not necessarily more similar in their
# degree of ecological rarity than distantly related species

# NGouja: rare and super rare in the same group
rarcom_df_NG <- rarcom_df[which(rarcom_df$site == "N'Gouja"), ]
rarcom_df_NG$rarity[which(rarcom_df_NG$rarity %in% c("rare", "common"))] <- "common"
caper::phylo.d(data = rarcom_df_NG[, c("species_nm", "rarity")],
               phy = phylo,
               names.col = species_nm,
               binvar = rarity)
# D = 0.85
# closely related species were not necessarily more similar in their
# degree of ecological rarity than distantly related species

## WITH ONE PHYLOGENY FOR EACH SITE

# Create phylogeny Boueni:
rarcom_df_B <- rarcom_df[which(rarcom_df$site == "Boueni"), ]
sp_nm_B <- unique(rarcom_df_B$species_nm)
phylo_B <- fishtree::fishtree_phylogeny(species = sp_nm_B)

# Create phylogeny NGouja:
rarcom_df_NG <- rarcom_df[which(rarcom_df$site == "N'Gouja"), ]
sp_nm_NG <- unique(rarcom_df_NG$species_nm)
phylo_NG <- fishtree::fishtree_phylogeny(species = sp_nm_NG)


# Boueni: rare and super rare in the same group
rarcom_df_B <- rarcom_df[which(rarcom_df$site == "Boueni"), ]
rarcom_df_B$rarity[which(rarcom_df_B$rarity %in% c("rare", "super rare"))] <- "rare"
caper::phylo.d(data = rarcom_df_B[, c("species_nm", "rarity")],
               phy = phylo_B,
               names.col = species_nm,
               binvar = rarity)
# D = 0.73
# closely related species were not necessarily more similar in their
# degree of ecological rarity than distantly related species

# NGouja: rare and super rare in the same group
rarcom_df_NG <- rarcom_df[which(rarcom_df$site == "N'Gouja"), ]
rarcom_df_NG$rarity[which(rarcom_df_NG$rarity %in% c("rare", "super rare"))] <- "rare"
caper::phylo.d(data = rarcom_df_NG[, c("species_nm", "rarity")],
               phy = phylo_NG,
               names.col = species_nm,
               binvar = rarity)
# D = 0.83
# closely related species were not necessarily more similar in their
# degree of ecological rarity than distantly related species


# Boueni: rare and common in the same group
rarcom_df_B <- rarcom_df[which(rarcom_df$site == "Boueni"), ]
rarcom_df_B$rarity[which(rarcom_df_B$rarity %in% c("rare", "common"))] <- "common"
caper::phylo.d(data = rarcom_df_B[, c("species_nm", "rarity")],
               phy = phylo_B,
               names.col = species_nm,
               binvar = rarity)
# D = 0.98
# closely related species were not necessarily more similar in their
# degree of ecological rarity than distantly related species

# NGouja: rare and super rare in the same group
rarcom_df_NG <- rarcom_df[which(rarcom_df$site == "N'Gouja"), ]
rarcom_df_NG$rarity[which(rarcom_df_NG$rarity %in% c("rare", "common"))] <- "common"
caper::phylo.d(data = rarcom_df_NG[, c("species_nm", "rarity")],
               phy = phylo_NG,
               names.col = species_nm,
               binvar = rarity)
# D = 0.86
# closely related species were not necessarily more similar in their
# degree of ecological rarity than distantly related species


# Step 6: Plot the functional specialisation of each rare/common species and Test ####


# call site asb fe df:
site_asb_sp_df <- readRDS(here::here("transformed_data/site_asb_sp_df.rds"))

# call sp coordinates:
sp_faxes_coord <- readRDS(here::here("transformed_data", "sp_faxes_coord.rds"))

# rar comm informations about species:
rarcom_df <- readRDS(here::here("transformed_data", "rarcom_df.rds"))


# compute fspe:
alpha_fd_indices_site <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = as.matrix(sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4", "PC5")]),
  asb_sp_w         = as.matrix(site_asb_sp_df),
  ind_vect         = c("fspe"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

sp_dist_gravcenter <- alpha_fd_indices_site$details$pool_sp_dist_O / max(alpha_fd_indices_site$details$pool_sp_dist_O)


rarfspe <- plot.rarity.fspe(sp_dist_gravcenter,
                            basic_accum_df,
                            rarcom_df)


# TEST


# get the fspe rarity df to test fspe differences:
rarspe_df <- rarfspe[[1]]

# add a column with site_rarity and test if difference of FSpe between sites and Rarity levels:
# ... taking all species, unique and shared:
rarspe_df$site_rar <- paste0(rarspe_df$site, sep = "_", rarspe_df$rarity)


# FOR ALL SPECIES POOLED: SHARED AND UNIQUE


## Test differences of FSpe with N'Gouja data (difference of FSpe between rarity levels):

kruskal.test(data = rarspe_df[which(rarspe_df$site == "N'Gouja"), ],
             relat_dist_to_grav ~ site_rar)

# yes differences between categories: between which: Dunn Test:
FSA::dunnTest(relat_dist_to_grav ~ site_rar,
         data = rarspe_df[which(rarspe_df$site == "N'Gouja"), ],
         method = "bonferroni")
# diff signif for common and super rare


## Test differences of FSpe with Boueni data (difference of FSpe between rarity levels):

kruskal.test(data = rarspe_df[which(rarspe_df$site == "Boueni"), ],
             relat_dist_to_grav ~ site_rar)

# yes differences between categories: between which: Dunn Test:
FSA::dunnTest(relat_dist_to_grav ~ site_rar,
              data = rarspe_df[which(rarspe_df$site == "Boueni"), ],
              method = "bonferroni")
# diff signif for common and super rare and between common and rare.


## Test differences of FSpe with N'Gouja AND Boueni data (difference of FSpe ...
# ... between rarity levels AND sites):

kruskal.test(data = rarspe_df,
             relat_dist_to_grav ~ site_rar)

# yes differences between categories: between which: Dunn Test:
results_Dunn_site_rar <- FSA::dunnTest(relat_dist_to_grav ~ site_rar,
              data = rarspe_df,
              method = "bonferroni")
results_Dunn_site_rar_df <- results_Dunn_site_rar$res
# diff signif for common and super rare


# ONLY FOR SPECIES UNIQUE AT EACH SITE:


rarspe_df <- rarspe_df[which(rarspe_df$site_presence %in% c("N'Gouja only",
                                                            "Boueni only")), ]


## Test differences of FSpe with N'Gouja data (difference of FSpe between rarity levels):

kruskal.test(data = rarspe_df[which(rarspe_df$site == "N'Gouja"), ],
             relat_dist_to_grav ~ site_rar)

# no differences between rarity levels of unique species in N'Gouja

## Test differences of FSpe with Boueni data (difference of FSpe between rarity levels):

kruskal.test(data = rarspe_df[which(rarspe_df$site == "Boueni"), ],
             relat_dist_to_grav ~ site_rar)

# no differences between rarity levels of unique species in Boueni


## Test differences of FSpe with N'Gouja AND Boueni data (difference of FSpe ...
# ... between rarity levels AND sites):

kruskal.test(data = rarspe_df,
             relat_dist_to_grav ~ site_rar)

# no differences between rarity levels of unique species in N'Gouja AND Boueni


