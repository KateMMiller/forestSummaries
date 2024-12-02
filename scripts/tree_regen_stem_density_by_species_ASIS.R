#-------------------------------------------------------------------
# Densities of Tree, sapling and seedlings in first cycle
# ++++++++ MUST RUN source_script.R FIRST ++++++++
#-------------------------------------------------------------------

library(cowplot)

if(!exists('trspp_grps')){stop("Must run source_script_MIDN.R before this script will work.")}
head(trspp_grps)


# Colors for ggplot legend
cols = c(
  "Acer rubrum (red maple)" = "#38A800",
  "Acer platanoides (Norway maple)" = "#8b0000",
  "Acer spp. (maple)" = "#00FF00",
  "Ailanthus altissima (tree-of-heaven)" = "#cd4a8f",
  "Asimina triloba (pawpaw)" = "#FF00C5",
  "Betula lenta (black birch)" = "#fffac8",
  "Betula spp. (birch)" = "#fffac8", # Either use BETLEN or BETSPP
  "Carya spp. (hickory)" = "#911eb4",
  "Fagus grandifolia (American beech)" = "#FFAA00",
  "Fraxinus spp. (ash)" = "#A87000",
  "Ilex opaca (American holly)" = "#42d4f4",
  "Juniperus virginiana (eastern redcedar)" = "#9371B9",
  "Liquidambar styraciflua (sweetgum)" = "#FFFF00",
  "Liriodendron tulipifera (tulip poplar)" = "#4363d8",
  "Nyssa sylvatica (black gum)" = "#000075",
  "Other exotic spp." = "#ca0020",
  "Other native canopy spp." = "#d9d9d9",
  "Pinus spp. (pine)" = "#5A462B",
  "Pinus strobus (eastern white pine)" = "#5A1111",
  "Pinus taeda (loblolly pine)" = "#5A1111", #assumes no overlap in PINSTR and PINTAE
  "Pinus virginiana (Virginia pine)" = "#E5740D",
  "Prunus spp. (native cherry)" ="#00E6A9", 
  "Pyrus calleryana (Bradford pear)" = "#cd4a8f",
  "Quercus spp. (oak)" = "#0E5D2C",
  "Robinia pseudoacacia (black locust)" = "#cccc00",
  "Other native subcanopy spp." = "#ffa8b4",
  "Tsuga canadensis (eastern hemlock)" = "#9bd2ef",
  "Ulmus spp. (native elm)" = "#808000", 
  "Unknown spp." = "#CACACA",
  "Diospyros virginiana (persimmon)" = "#006666", #for ASIS only
  "Amelanchier spp. (serviceberry)" = "#ffd8b1", #for ASIS only
  "Sassafras albidum (sassafrass)" = "#59538A") #for ASIS only

lines = c(
  "Acer rubrum (red maple)" = "solid",
  "Acer platanoides (Norway maple)" = "solid",
  "Acer spp. (maple)" = "solid",
  "Ailanthus altissima (tree-of-heaven)" = "solid",
  "Asimina triloba (pawpaw)" = "dashed",
  "Betula lenta (black birch)" = "dashed",
  "Betula spp. (birch)" = "dashed", # Either use BETLEN or BETSPP
  "Carya spp. (hickory)" = "solid",
  "Fagus grandifolia (American beech)" = "solid",
  "Fraxinus spp. (ash)" = "solid",
  "Ilex opaca (American holly)" = "solid",
  "Juniperus virginiana (eastern redcedar)" = "dotdash",
  "Liquidambar styraciflua (sweetgum)" = "solid",
  "Liriodendron tulipifera (tulip poplar)" = "solid",
  "Nyssa sylvatica (black gum)" = "dashed",
  "Other exotic spp." = "dashed",
  "Other native canopy spp." = "solid",
  "Pinus spp. (pine)" = "dotdash",
  "Pinus strobus (eastern white pine)" = "dotdash",
  "Pinus taeda (loblolly pine)" = "dotdash",
  "Pinus virginiana (Virginia pine)" = "dotdash",
  "Prunus spp. (native cherry)" = "dotdash", 
  "Pyrus calleryana (Bradford pear)" = "dotted",
  "Quercus spp. (oak)" = "solid",
  "Robinia pseudoacacia (black locust)" = "dashed",
  "Other native subcanopy spp." = "solid",
  "Tsuga canadensis (eastern hemlock)" = "dashed",
  "Ulmus spp. (native elm)" = "dashed", 
  "Unknown spp." = "dotted",
  "Diospyros virginiana (American persimmon)" = "dashed", #for ASIS only
  "Amelanchier spp. (serviceberry)" = "dashed", #for ASIS only
  "Sassafras albidum (sassafrass)" = "dashed") #for ASIS only)

wd_spp <- sumSpeciesList(park = "ASIS", from = from, to = to) |> 
  filter(ScientificName %in% trspp_grps$Species)

table(wd_spp$ScientificName)

wd_spp2 <- left_join(wd_spp, trspp_grps |> select(Species, spp_grp), 
                     by = c("ScientificName" = "Species"))

table(wd_spp2$ScientificName)

# Revising species groups because low diversity and unique from other MIDN/NCBN parks
wd_spp2$spp_grp[wd_spp2$spp_grp == "Pinus spp. (pine)"] <- "Pinus taeda (loblolly pine)"
wd_spp2$spp_grp[wd_spp2$ScientificName == "Morella cerifera"] <- "Subcanopy"
wd_spp2$spp_grp[wd_spp2$ScientificName == "Diospyros virginiana"] <- "Diospyros virginiana (persimmon)"
wd_spp2$spp_grp[wd_spp2$ScientificName == "Persea"] <- "Subcanopy"
wd_spp2$spp_grp[wd_spp2$ScientificName == "Persea palustris"] <- "Subcanopy"
wd_spp2$spp_grp[wd_spp2$ScientificName == "Amelanchier canadensis"] <- "Amelanchier spp. (serviceberry)"
wd_spp2$spp_grp[wd_spp2$ScientificName == "Amelanchier"] <- "Amelanchier spp. (serviceberry)"
wd_spp2$spp_grp[wd_spp2$ScientificName == "Sassafras albidum"] <- "Sassafras albidum (sassafrass)"
wd_spp2$spp_grp[wd_spp2$ScientificName == "Juniperus virginiana"] <- "Other native subcanopy spp."
wd_spp2$spp_grp[wd_spp2$spp_grp == "Subcanopy"] <- "Other native subcanopy spp."
wd_spp2$spp_grp[wd_spp2$spp_grp == "Other Native"] <- "Other native canopy spp."
wd_spp2$spp_grp[wd_spp2$spp_grp == "Other Exotic"] <- "Other exotic spp."

table(wd_spp2$spp_grp)

# Tree Plots
tr_sum <- wd_spp2 |> group_by(spp_grp) |> summarize(avg_dens = sum(tree_stems)/num_plots,
                                                    avg_dens_ha = (avg_dens * 10000)/400,
                                                    avg_BA = sum(BA_cm2)/num_plots,
                                                    avg_BA_m2ha = avg_BA/400) |> 
                                          select(-avg_dens, -avg_BA) 
#colSums(tr_sum[,2:3])

tr_dens_plot <- 
  ggplot(tr_sum, aes(y = avg_dens_ha, x = "Tree Density", fill = fct_reorder(spp_grp, avg_dens_ha))) + 
    geom_bar(position = 'stack', stat = 'identity', color = 'dimgrey') + 
    labs(y = "stems/ha") +
    scale_fill_manual(values = cols,  name = NULL) +
    theme_FVM() + theme(axis.title.x = element_blank(),
                        legend.title = element_blank(), 
                        legend.position = 'none')
tr_dens_plot

tr_BA_plot <- 
  ggplot(tr_sum, aes(y = avg_BA_m2ha, x = "Tree BA", fill = fct_reorder(spp_grp, avg_BA_m2ha))) + 
  geom_bar(position = 'stack', stat = 'identity', color = 'dimgrey') + 
  labs(y = "Basal Area (sq.m/ha)") +
  scale_fill_manual(values = cols,  name = NULL) +
  theme_FVM() + theme(axis.title.x = element_blank(),
                      legend.title = element_blank(), 
                      legend.position = 'none')
tr_BA_plot

# Regen Plots
names(wd_spp2)
regen_sum <- wd_spp2 |> group_by(spp_grp) |> summarize(avg_seed_dens = sum(seed_den)/num_plots,
                                                       avg_sap_dens = sum(sap_den)/num_plots)
head(regen_sum)

regen_sum2 <- wd_spp2 |> group_by(ScientificName) |> summarize(avg_seed_dens = sum(seed_den)/num_plots,
                                                       avg_sap_dens = sum(sap_den)/num_plots)
#colSums(tr_sum[,2:3])

seed_dens_plot <- 
  ggplot(regen_sum, aes(y = avg_seed_dens, x = "Seedling Density", fill = fct_reorder(spp_grp, avg_seed_dens))) + 
  geom_bar(position = 'stack', stat = 'identity', color = 'dimgrey') + 
  labs(y = "stems/sq.m") +
  scale_fill_manual(values = cols,  name = NULL) +
  theme_FVM() + theme(axis.title.x = element_blank(),
                      legend.title = element_blank(), 
                      legend.position = 'none')
seed_dens_plot

sap_dens_plot <- 
  ggplot(regen_sum, aes(y = avg_sap_dens, x = "Sapling Density", fill = fct_reorder(spp_grp, avg_sap_dens))) + 
  geom_bar(position = 'stack', stat = 'identity', color = 'dimgrey') + 
  labs(y = "stems/sq.m") +
  scale_fill_manual(values = cols,  name = NULL) +
  theme_FVM() + theme(axis.title.x = element_blank(),
                      legend.title = element_blank(), 
                      legend.position = 'none')
sap_dens_plot

# create fake plot with all species in dataframe, so can use 1 legend.
fake_plot <- ggplot(wd_spp2, aes(x = ParkUnit, y = SampleYear, fill = spp_grp)) + 
  geom_bar(stat = 'identity') + 
  scale_fill_manual(values = cols,  name = NULL) +
  theme_FVM() + theme(axis.title.x = element_blank(),
                      legend.title = element_blank(), 
                      legend.position = 'bottom') +
  guides(fill = guide_legend(nrow = 4))
fake_plot

spp_leg <- get_legend(fake_plot)

full_plot <- 
  plot_grid(
  plot_grid(tr_BA_plot, tr_dens_plot, sap_dens_plot, seed_dens_plot, ncol = 4, nrow = 1),
  spp_leg, ncol = 1, nrow = 2)

full_plot

ggsave(paste0(new_path, "figures/", "Figure_4_", park, "_tree_sapling_regen_by_species.svg"),
       height = 5.5, width = 7.5, units = 'in')

ggsave(paste0(new_path, "figures/", "Figure_4_", park, "_tree_sapling_regen_by_species.png"),
       height = 5.5, width = 7.5, units = 'in', dpi = 600)

